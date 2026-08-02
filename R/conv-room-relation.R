# ROOM_RELATION -> ZoneVentilation:DesignFlowRate for relations connected to OUTSIDE.
# VENT_SCHEDULE_ID is a foreign key to SCHEDULE_YEAR. The Access field comment
# says that the referenced 8760-value schedule corresponds to air changes, so
# those hourly schedule values are treated as hourly ACH values. For
# VENT_TYPE=1, VENT_SET_MAX is a second SCHEDULE_YEAR foreign key containing the
# maximum ACH. The documented rule is represented by a base minimum object and
# a max-minus-min supplement gated by outdoor-temperature setpoint schedules.
ventilation__convert <- function(dest, ep) {
    if (!db_has_rows(dest, "ROOM_RELATION")) return(NULL)

    relation <- DBI::dbGetQuery(
        dest,
        "
        SELECT
            RR.ID,
            RR.NAME,
            RR.OF_BUILDING,
            RR.ROOM_ID,
            R.NAME AS ROOM_NAME,
            RR.RELA_ROOM_ID,
            O.NAME AS OUTSIDE_NAME,
            RR.VENT_SCHEDULE_ID,
            S.NAME AS SCHEDULE_NAME,
            RR.VENT_SET_MAX,
            SMAX.NAME AS MAX_SCHEDULE_NAME,
            RR.VENT_TYPE,
            RR.START_POINT_ID,
            RR.END_POINT_ID,
            RR.EXT_PROPERTY
        FROM ROOM_RELATION RR
        LEFT JOIN ROOM R
        ON RR.ROOM_ID = R.ID
        LEFT JOIN OUTSIDE O
        ON RR.RELA_ROOM_ID = O.OUTSIDE_ID
        LEFT JOIN SCHEDULE_YEAR S
        ON RR.VENT_SCHEDULE_ID = S.SCHEDULE_ID
        LEFT JOIN SCHEDULE_YEAR SMAX
        ON RR.VENT_TYPE = 1 AND RR.VENT_SET_MAX = SMAX.SCHEDULE_ID
        ORDER BY RR.ID
        "
    )
    data.table::setDT(relation)
    data.table::set(relation, NULL, "IS_OUTDOOR_RELATION", !is.na(relation$OUTSIDE_NAME))

    # Resolve and validate every documented range-control dependency before
    # building objects, so malformed ranges cannot silently fall back to their
    # minimum ACH schedule.
    range <- ventilation__range_controls(dest)
    range_index <- match(relation$ID, range$RELATION_ID)
    range_fields <- c(
        "INCREMENT_SCHEDULE_NAME", "INCREMENT_AIR_CHANGES_PER_HOUR",
        "HEATING_SCHEDULE_NAME", "COOLING_SCHEDULE_NAME"
    )
    for (field in range_fields) {
        value <- if (field %in% names(range)) {
            range[[field]][range_index]
        } else if (identical(field, "INCREMENT_AIR_CHANGES_PER_HOUR")) {
            rep(NA_real_, nrow(relation))
        } else {
            rep(NA_character_, nrow(relation))
        }
        data.table::set(relation, NULL, field, value)
    }

    # Only OUTSIDE-linked records have an unambiguous ZoneVentilation mapping.
    # Inter-room records need a separate ZoneMixing interpretation.
    skip_reason <- rep(NA_character_, nrow(relation))
    skip_reason[!relation$IS_OUTDOOR_RELATION] <- "RELA_ROOM_ID does not reference OUTSIDE"
    skip_reason[is.na(relation$ROOM_NAME)] <- "ROOM_ID does not reference ROOM"
    skip_reason[is.na(relation$SCHEDULE_NAME)] <- "VENT_SCHEDULE_ID does not reference SCHEDULE_YEAR"
    data.table::set(relation, NULL, "SKIP_REASON", skip_reason)
    data.table::set(relation, NULL, "CAN_CONVERT", is.na(skip_reason))
    data.table::set(
        relation, NULL, "RANGE_CONTROL_CONVERTED",
        relation$VENT_TYPE != 1L |
            !is.na(relation$INCREMENT_AIR_CHANGES_PER_HOUR)
    )
    data.table::set(
        relation, NULL, "RANGE_CONTROL_METHOD",
        ifelse(
            relation$VENT_TYPE == 1L,
            "documented_outdoor_temperature_band",
            "fixed_schedule"
        )
    )
    data.table::set(
        relation, NULL, "RANGE_CONTROL_FIDELITY",
        ifelse(
            relation$VENT_TYPE == 1L,
            "documented_rule_not_solver_equivalent",
            "source_schedule"
        )
    )
    data.table::set(relation, NULL, "HVAC_AVAILABILITY_GATED", FALSE)
    # EnergyPlus multiplies the ACH design level by the schedule fraction/value;
    # use a unit ACH design level so the referenced DeST schedule DATA values
    # pass through as the actual hourly ACH sequence.
    data.table::set(relation, NULL, "AIR_CHANGES_PER_HOUR", 1)
    data.table::set(relation, NULL, "ENERGYPLUS_NAME", ventilation__names(relation))

    supplement_candidates <- paste(
        relation$ENERGYPLUS_NAME, "Documented Range Supplement"
    )
    data.table::set(
        relation, NULL, "RANGE_ENERGYPLUS_NAME",
        ventilation__reserve_names(
            supplement_candidates, relation$ENERGYPLUS_NAME
        )
    )

    if (any(!relation$CAN_CONVERT)) {
        warn(sprintf(
            "Skipped %i ROOM_RELATION row(s) that do not describe supported outdoor ventilation.",
            sum(!relation$CAN_CONVERT)
        ))
    }
    unresolved_range <- relation$CAN_CONVERT & relation$VENT_TYPE == 1L &
        !relation$RANGE_CONTROL_CONVERTED
    if (any(unresolved_range)) {
        abort(sprintf(
            paste0(
                "Cannot resolve documented ventilation-range control for ",
                "ROOM_RELATION ID(s): [%s]."
            ),
            fmt_integer_sample(relation$ID[unresolved_range])
        ))
    }
    range_converted <- relation$CAN_CONVERT & relation$VENT_TYPE == 1L &
        relation$RANGE_CONTROL_CONVERTED
    if (any(range_converted)) {
        warn(sprintf(
            paste0(
                "Mapped %i DeST ventilation-range ROOM_RELATION row(s) using ",
                "the documented outdoor-temperature-band rule. This preserves ",
                "the declared minimum/maximum ACH schedules but does not claim ",
                "equivalence to DeST's undocumented solver-state coupling."
            ),
            sum(range_converted)
        ))
    }

    ventilation <- relation[relation$CAN_CONVERT]
    if (nrow(ventilation) == 0L) return(NULL)

    base_values <- lapply(seq_len(nrow(ventilation)), function(i) {
        ventilation__value(ventilation, i)
    })
    supplement <- ventilation[
        ventilation$VENT_TYPE == 1L &
            ventilation$INCREMENT_AIR_CHANGES_PER_HOUR > 0
    ]
    supplement_values <- lapply(seq_len(nrow(supplement)), function(i) {
        ventilation__range_value(supplement, i)
    })
    out <- conv__combine_outputs(
        list(
            base = conv__add_objects(
                dest, ep, "ZoneVentilation:DesignFlowRate", base_values
            ),
            range = conv__add_objects(
                dest, ep, "ZoneVentilation:DesignFlowRate", supplement_values
            )
        ),
        table = relation
    )

    out
}

# Allocate deterministic derived names without renaming any source schedule or
# ventilation object that already occupies the requested name.
ventilation__reserve_names <- function(candidates, reserved = character()) {
    used <- reserved[!is.na(reserved) & nzchar(reserved)]
    out <- character(length(candidates))
    for (i in seq_along(candidates)) {
        root <- candidates[[i]]
        value <- root
        suffix <- 2L
        while (value %in% used) {
            value <- sprintf("%s (%i)", root, suffix)
            suffix <- suffix + 1L
        }
        out[[i]] <- value
        used <- c(used, value)
    }
    out
}

# Decode one DeST yearly schedule and reject truncated or non-finite BLOB data
# before it can participate in range arithmetic or temperature comparisons.
ventilation__schedule_values <- function(blob, schedule_id, role) {
    value <- readBin(blob, "double", n = 8761L)
    if (length(value) != 8760L) {
        abort(sprintf(
            "%s schedule %s contains %i values; expected 8760.",
            role, schedule_id, length(value)
        ))
    }
    invalid <- which(!is.finite(value))
    if (length(invalid) > 0L) {
        abort(sprintf(
            "%s schedule %s is non-finite at DeST hour index %i.",
            role, schedule_id, invalid[[1L]] - 1L
        ))
    }
    value
}

# Resolve the published DeST range-control inputs and derive a fractional
# max-minus-min schedule for every unique schedule pair. This reproduces the
# manual rule only; no HVAC-availability gate is inferred here.
ventilation__range_controls <- function(dest) {
    required_tables <- c(
        "ROOM_RELATION", "ROOM", "OUTSIDE", "ROOM_TYPE_DATA", "SCHEDULE_YEAR"
    )
    if (!all(required_tables %in% DBI::dbListTables(dest)) ||
        !db_has_rows(dest, "ROOM_RELATION")) {
        return(data.table::data.table())
    }

    control <- data.table::as.data.table(DBI::dbGetQuery(dest, "
        SELECT
            RR.ID AS RELATION_ID,
            RR.NAME AS RELATION_NAME,
            R.NAME AS ROOM_NAME,
            RR.VENT_SCHEDULE_ID AS MIN_SCHEDULE_ID,
            VMIN.NAME AS MIN_SCHEDULE_NAME,
            VMIN.DATA AS MIN_SCHEDULE_DATA,
            RR.VENT_SET_MAX AS MAX_SCHEDULE_ID,
            VMAX.NAME AS MAX_SCHEDULE_NAME,
            VMAX.DATA AS MAX_SCHEDULE_DATA,
            R.TYPE AS ROOM_TYPE_ID,
            T.ID AS ROOM_TYPE_DATA_ID,
            T.SET_T_MIN_SCHEDULE AS HEATING_SCHEDULE_ID,
            TMIN.NAME AS HEATING_SCHEDULE_NAME,
            TMIN.DATA AS HEATING_SCHEDULE_DATA,
            T.SET_T_MAX_SCHEDULE AS COOLING_SCHEDULE_ID,
            TMAX.NAME AS COOLING_SCHEDULE_NAME,
            TMAX.DATA AS COOLING_SCHEDULE_DATA
        FROM ROOM_RELATION RR
        INNER JOIN ROOM R
        ON RR.ROOM_ID = R.ID
        INNER JOIN OUTSIDE O
        ON RR.RELA_ROOM_ID = O.OUTSIDE_ID
        LEFT JOIN SCHEDULE_YEAR VMIN
        ON RR.VENT_SCHEDULE_ID = VMIN.SCHEDULE_ID
        LEFT JOIN SCHEDULE_YEAR VMAX
        ON RR.VENT_SET_MAX = VMAX.SCHEDULE_ID
        LEFT JOIN ROOM_TYPE_DATA T
        ON R.TYPE = T.ID
        LEFT JOIN SCHEDULE_YEAR TMIN
        ON T.SET_T_MIN_SCHEDULE = TMIN.SCHEDULE_ID
        LEFT JOIN SCHEDULE_YEAR TMAX
        ON T.SET_T_MAX_SCHEDULE = TMAX.SCHEDULE_ID
        WHERE RR.VENT_TYPE = 1
        ORDER BY RR.ID
    "))
    if (nrow(control) == 0L) return(control)

    required <- c(
        "MIN_SCHEDULE_NAME", "MAX_SCHEDULE_NAME", "ROOM_TYPE_DATA_ID",
        "HEATING_SCHEDULE_NAME", "COOLING_SCHEDULE_NAME"
    )
    unresolved <- apply(is.na(control[, ..required]), 1L, any)
    if (any(unresolved)) {
        rows <- control[unresolved]
        abort(sprintf(
            paste0(
                "Cannot resolve documented ventilation-range inputs for ",
                "ROOM_RELATION ID(s): [%s]."
            ),
            fmt_integer_sample(rows$RELATION_ID)
        ))
    }

    pair_columns <- c(
        "MIN_SCHEDULE_ID", "MIN_SCHEDULE_NAME", "MIN_SCHEDULE_DATA",
        "MAX_SCHEDULE_ID", "MAX_SCHEDULE_NAME", "MAX_SCHEDULE_DATA"
    )
    pairs <- unique(control[, ..pair_columns], by = c(
        "MIN_SCHEDULE_ID", "MAX_SCHEDULE_ID"
    ))
    increment_fraction <- vector("list", nrow(pairs))
    increment_ach <- numeric(nrow(pairs))
    for (i in seq_len(nrow(pairs))) {
        minimum <- ventilation__schedule_values(
            pairs$MIN_SCHEDULE_DATA[[i]], pairs$MIN_SCHEDULE_ID[[i]],
            "Minimum ventilation"
        )
        maximum <- ventilation__schedule_values(
            pairs$MAX_SCHEDULE_DATA[[i]], pairs$MAX_SCHEDULE_ID[[i]],
            "Maximum ventilation"
        )
        negative <- which(minimum < 0 | maximum < 0)
        if (length(negative) > 0L) {
            abort(sprintf(
                paste0(
                    "Ventilation schedules %s/%s contain a negative ACH at ",
                    "DeST hour index %i."
                ),
                pairs$MIN_SCHEDULE_ID[[i]], pairs$MAX_SCHEDULE_ID[[i]],
                negative[[1L]] - 1L
            ))
        }
        inverted <- which(maximum < minimum)
        if (length(inverted) > 0L) {
            abort(sprintf(
                paste0(
                    "Maximum ventilation schedule %s is below minimum ",
                    "schedule %s at DeST hour index %i."
                ),
                pairs$MAX_SCHEDULE_ID[[i]], pairs$MIN_SCHEDULE_ID[[i]],
                inverted[[1L]] - 1L
            ))
        }
        delta <- maximum - minimum
        peak <- max(delta)
        increment_ach[[i]] <- peak
        increment_fraction[[i]] <- if (peak > 0) delta / peak else delta
    }

    existing_names <- DBI::dbGetQuery(
        dest, "SELECT NAME FROM SCHEDULE_YEAR"
    )$NAME
    candidates <- sprintf(
        "DeST Derived Ventilation Range %s Minus %s",
        pairs$MAX_SCHEDULE_ID, pairs$MIN_SCHEDULE_ID
    )
    data.table::set(
        pairs, NULL, "INCREMENT_SCHEDULE_NAME",
        ventilation__reserve_names(candidates, existing_names)
    )
    data.table::set(
        pairs, NULL, "INCREMENT_AIR_CHANGES_PER_HOUR", increment_ach
    )
    data.table::set(pairs, NULL, "INCREMENT_FRACTION", increment_fraction)

    pair_key <- paste(pairs$MIN_SCHEDULE_ID, pairs$MAX_SCHEDULE_ID)
    control_key <- paste(control$MIN_SCHEDULE_ID, control$MAX_SCHEDULE_ID)
    pair_index <- match(control_key, pair_key)
    for (field in c(
        "INCREMENT_SCHEDULE_NAME", "INCREMENT_AIR_CHANGES_PER_HOUR",
        "INCREMENT_FRACTION"
    )) {
        data.table::set(control, NULL, field, pairs[[field]][pair_index])
    }

    # The temperature pair can vary independently of the ACH schedule pair.
    # Validate every relation because these schedules form the actual gate.
    for (i in seq_len(nrow(control))) {
        heating <- ventilation__schedule_values(
            control$HEATING_SCHEDULE_DATA[[i]],
            control$HEATING_SCHEDULE_ID[[i]], "Heating setpoint"
        )
        cooling <- ventilation__schedule_values(
            control$COOLING_SCHEDULE_DATA[[i]],
            control$COOLING_SCHEDULE_ID[[i]], "Cooling setpoint"
        )
        inverted <- which(heating > cooling)
        if (length(inverted) > 0L) {
            abort(sprintf(
                paste0(
                    "Heating setpoint schedule %s exceeds cooling schedule ",
                    "%s at DeST hour index %i for ROOM_RELATION %s."
                ),
                control$HEATING_SCHEDULE_ID[[i]],
                control$COOLING_SCHEDULE_ID[[i]],
                inverted[[1L]] - 1L, control$RELATION_ID[[i]]
            ))
        }
    }

    raw_fields <- grep("_SCHEDULE_DATA$", names(control), value = TRUE)
    for (field in raw_fields) data.table::set(control, NULL, field, NULL)
    control
}

# Expose the derived fractional schedules to the ordinary schedule conversion
# pipeline so the returned IDF remains self-contained and needs no sidecar CSV.
ventilation__range_schedule_rows <- function(dest) {
    control <- ventilation__range_controls(dest)
    if (nrow(control) == 0L) return(data.table::data.table())

    keep <- control$INCREMENT_AIR_CHANGES_PER_HOUR > 0
    pairs <- unique(control[keep], by = c(
        "MIN_SCHEDULE_ID", "MAX_SCHEDULE_ID"
    ))
    if (nrow(pairs) == 0L) return(data.table::data.table())

    source_ids <- DBI::dbGetQuery(
        dest, "SELECT SCHEDULE_ID FROM SCHEDULE_YEAR"
    )$SCHEDULE_ID
    first_id <- min(c(0, source_ids), na.rm = TRUE) - nrow(pairs)
    out <- data.table::data.table(
        SCHEDULE_ID = seq.int(first_id, length.out = nrow(pairs)),
        NAME = pairs$INCREMENT_SCHEDULE_NAME,
        TYPE = 1L
    )
    data.table::set(out, NULL, "DATA", pairs$INCREMENT_FRACTION)
    out
}

# Build stable EnergyPlus object names for ROOM_RELATION records whose DeST NAME
# is usually empty or a literal dot in observed models.
ventilation__names <- function(relation) {
    raw_name <- relation$NAME
    use_room_name <- is.na(raw_name) | raw_name == "." | !nzchar(raw_name)
    raw_name[use_room_name] <- paste(relation$ROOM_NAME[use_room_name], "Outdoor Ventilation")
    raw_name[is.na(raw_name)] <- paste("ROOM_RELATION", relation$ID[is.na(raw_name)])

    make_unique_name(raw_name)
}

# Create the EnergyPlus ventilation object value list for one external
# ROOM_RELATION row.
ventilation__value <- function(ventilation, i) {
    list(
        name = ventilation$ENERGYPLUS_NAME[[i]],
        zone_or_zonelist_or_space_or_spacelist_name = ventilation$ROOM_NAME[[i]],
        schedule_name = ventilation$SCHEDULE_NAME[[i]],
        design_flow_rate_calculation_method = "AirChanges/Hour",
        design_flow_rate = NULL,
        flow_rate_per_floor_area = NULL,
        flow_rate_per_person = NULL,
        air_changes_per_hour = ventilation$AIR_CHANGES_PER_HOUR[[i]],
        ventilation_type = "Natural"
    )
}

# Create the supplemental EnergyPlus ventilation object whose fractional
# schedule supplies max-minus-min ACH only inside the documented outdoor band.
ventilation__range_value <- function(ventilation, i) {
    list(
        name = ventilation$RANGE_ENERGYPLUS_NAME[[i]],
        zone_or_zonelist_or_space_or_spacelist_name =
            ventilation$ROOM_NAME[[i]],
        schedule_name = ventilation$INCREMENT_SCHEDULE_NAME[[i]],
        design_flow_rate_calculation_method = "AirChanges/Hour",
        air_changes_per_hour =
            ventilation$INCREMENT_AIR_CHANGES_PER_HOUR[[i]],
        ventilation_type = "Natural",
        fan_pressure_rise = 0,
        fan_total_efficiency = 1,
        constant_term_coefficient = 1,
        temperature_term_coefficient = 0,
        velocity_term_coefficient = 0,
        velocity_squared_term_coefficient = 0,
        minimum_indoor_temperature = -100,
        maximum_indoor_temperature = 100,
        delta_temperature = -100,
        minimum_outdoor_temperature_schedule_name =
            ventilation$HEATING_SCHEDULE_NAME[[i]],
        maximum_outdoor_temperature_schedule_name =
            ventilation$COOLING_SCHEDULE_NAME[[i]],
        maximum_wind_speed = 40
    )
}
