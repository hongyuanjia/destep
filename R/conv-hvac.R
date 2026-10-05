# Stop conversion before unsupported DeST HVAC families can be silently
# represented by an unrelated EnergyPlus system.
hvac__assert_supported_system_types <- function(dest, inventory = NULL) {
    if (is.null(inventory)) {
        inventory <- hvac__source_inventory(dest)
    }
    unsupported <- inventory$systems[
        inventory$systems$SYSTEM_STATE == "unsupported_type"
    ]
    if (!nrow(unsupported)) {
        return(invisible(TRUE))
    }

    # Include source identifiers and names so users can locate every rejected
    # system directly in DeST instead of receiving only an opaque type code.
    details <- unsupported[, sprintf(
        "%s (NAME=%s, AC_SYS_TYPE=%s)",
        data.table::fifelse(is.na(AC_SYS_ID), "NA", as.character(AC_SYS_ID)),
        data.table::fifelse(is.na(NAME), "NA", as.character(NAME)),
        data.table::fifelse(
            is.na(AC_SYS_TYPE),
            "NA",
            as.character(AC_SYS_TYPE)
        )
    )]
    abort(
        paste0(
            "Unsupported DeST air-conditioning system type detected. ",
            "destep currently supports AC_SYS_TYPE values 0 and 1 only: ",
            paste(details, collapse = "; "),
            "."
        ),
        class = "destep_unsupported_hvac_system_type"
    )
}

# List target representation fields used by the native fan graph. All receive
# defaults internally; this list is not a required user-input contract.
hvac__common_required_options <- function() {
    c(
        "supply_fan_total_efficiency",
        "supply_fan_delta_pressure_pa",
        "supply_fan_motor_efficiency",
        "supply_fan_motor_in_air_fraction",
        "return_fan_total_efficiency",
        "return_fan_delta_pressure_pa",
        "return_fan_motor_efficiency",
        "return_fan_motor_in_air_fraction",
        paste0("return_fan_power_coefficient_", seq_len(5L)),
        "zone_exhaust_fan_total_efficiency",
        "zone_exhaust_fan_pressure_rise_pa"
    )
}

# Validate resolved internal fields for each supported target graph. Source
# technology is always water; no compatibility branch selects electric heat.
hvac__required_options <- function(
    path = c("single_zone_cav", "multizone_cav", "multizone_vav")
) {
    path <- match.arg(path)
    required <- hvac__common_required_options()
    if (path == "multizone_cav") {
        required <- setdiff(
            required,
            paste0("return_fan_power_coefficient_", seq_len(5L))
        )
    }
    if (path == "single_zone_cav") {
        c(
            required,
            "cooling_coil_design_setpoint_c",
            "heating_coil_design_setpoint_c",
            "heating_coil_type",
            "heating_coil_rated_air_water_convection_ratio"
        )
    } else {
        c(required, hvac__terminal_required_options())
    }
}

# Validate the external equipment parameters that DeST does not fully define.
hvac__validate_options <- function(
    options,
    path = c("single_zone_cav", "multizone_cav", "multizone_vav")
) {
    path <- match.arg(path)
    checkmate::assert_list(options, names = "unique")
    required <- hvac__required_options(path)
    checkmate::assert_names(
        names(options),
        must.include = required
    )

    bounded_zero_one <- c(
        "supply_fan_total_efficiency",
        "supply_fan_motor_efficiency",
        "supply_fan_motor_in_air_fraction",
        "return_fan_total_efficiency",
        "return_fan_motor_efficiency",
        "return_fan_motor_in_air_fraction",
        "zone_exhaust_fan_total_efficiency"
    )
    positive <- c(
        "supply_fan_total_efficiency",
        "supply_fan_motor_efficiency",
        "return_fan_total_efficiency",
        "return_fan_motor_efficiency",
        "zone_exhaust_fan_total_efficiency"
    )
    character_fields <- c(
        "cooling_coil_type",
        "preheat_coil_type",
        "heating_coil_type"
    )
    structured_fields <- "zone_outdoor_air_flow_m3_s"
    numeric_fields <- setdiff(
        required,
        c(character_fields, structured_fields)
    )
    for (field in numeric_fields) {
        checkmate::assert_number(
            options[[field]],
            finite = TRUE,
            .var.name = paste0("hvac_options$", field)
        )
    }
    for (field in intersect(bounded_zero_one, required)) {
        checkmate::assert_number(
            options[[field]],
            lower = 0,
            upper = 1,
            finite = TRUE,
            .var.name = paste0("hvac_options$", field)
        )
    }
    for (field in intersect(positive, required)) {
        checkmate::assert_true(
            options[[field]] > 0,
            .var.name = paste0("hvac_options$", field)
        )
    }
    for (field in intersect(
        c(
            "supply_fan_delta_pressure_pa",
            "return_fan_delta_pressure_pa",
            "zone_exhaust_fan_pressure_rise_pa"
        ),
        required
    )) {
        checkmate::assert_true(
            options[[field]] >= 0,
            .var.name = paste0("hvac_options$", field)
        )
    }
    for (field in intersect(
        character_fields,
        required
    )) {
        checkmate::assert_string(
            options[[field]],
            min.chars = 1L,
            .var.name = paste0("hvac_options$", field)
        )
    }

    if (path == "single_zone_cav") {
        checkmate::assert_choice(
            options$heating_coil_type,
            "HotWater",
            .var.name = "source main heating coil type"
        )
    }

    invisible(options)
}

# Map verified DeST outdoor-air control codes to EnergyPlus economizers.
hvac__economizer_type <- function(outdoor_air_control_type) {
    control_types <- c(
        `1` = "NoEconomizer",
        `5` = "DifferentialDryBulb",
        `6` = "DifferentialEnthalpy"
    )
    source_code <- as.character(outdoor_air_control_type)
    checkmate::assert_choice(
        source_code,
        names(control_types),
        .var.name = "DeST FRESH_AIR_TYPE"
    )
    unname(control_types[[source_code]])
}

# Select the active controller maximum while retaining source capacity.
hvac__maximum_outdoor_air_flow <- function(source) {
    checkmate::assert_data_table(source, nrows = 1L)
    if (source$economizer_type[[1L]] == "NoEconomizer") {
        return(source$outdoor_air_flow_m3_s[[1L]])
    }
    source$outdoor_air_capacity_m3_s[[1L]]
}

# Resolve the DeST minimum and maximum supply-temperature schedules used by a
# physical air system and reduce their common cooling trajectory to one sizing
# temperature. Distinct trajectories require a control model that is not yet
# represented by the current EnergyPlus HVAC path.
hvac__supply_temperature_source <- function(dest, system) {
    checkmate::assert_class(dest, "DBIConnection")
    checkmate::assert_data_table(system, nrows = 1L)
    checkmate::assert_names(
        names(system),
        must.include = c("SUPPLY_T_MIN", "SUPPLY_T_MAX")
    )
    if (
        !db_has_rows(dest, "SCHEDULE_YEAR") ||
            !db_has_fields(
                dest,
                "SCHEDULE_YEAR",
                c("SCHEDULE_ID", "NAME", "DATA")
            )
    ) {
        abort(
            paste0(
                "Physical HVAC conversion requires SCHEDULE_YEAR fields ",
                "SCHEDULE_ID, NAME, and DATA for AC_SYS supply temperature."
            ),
            class = "destep_missing_hvac_supply_temperature_schedule"
        )
    }

    schedule_ids <- as.integer(c(
        system$SUPPLY_T_MIN[[1L]],
        system$SUPPLY_T_MAX[[1L]]
    ))
    checkmate::assert_integerish(
        schedule_ids,
        len = 2L,
        lower = 1L,
        any.missing = FALSE,
        .var.name = "AC_SYS SUPPLY_T_MIN/MAX references"
    )
    schedules <- data.table::as.data.table(DBI::dbGetQuery(
        dest,
        sprintf(
            paste(
                "SELECT SCHEDULE_ID, NAME, DATA FROM SCHEDULE_YEAR",
                "WHERE SCHEDULE_ID IN (%s) ORDER BY SCHEDULE_ID"
            ),
            paste(unique(schedule_ids), collapse = ", ")
        )
    ))
    unresolved <- setdiff(schedule_ids, schedules$SCHEDULE_ID)
    if (length(unresolved) > 0L) {
        abort(
            sprintf(
                "Cannot resolve AC_SYS supply-temperature schedule ID(s): [%s].",
                paste(unresolved, collapse = ", ")
            ),
            class = "destep_missing_hvac_supply_temperature_schedule"
        )
    }

    values <- lapply(schedule_ids, function(schedule_id) {
        raw_data <- schedules[SCHEDULE_ID == schedule_id, DATA][[1L]]
        readBin(raw_data, what = "double", n = 8760L)
    })
    for (index in seq_along(values)) {
        checkmate::assert_numeric(
            values[[index]],
            len = 8760L,
            finite = TRUE,
            .var.name = paste0(
                "SCHEDULE_YEAR.DATA for supply-temperature schedule ",
                schedule_ids[[index]]
            )
        )
    }
    if (!isTRUE(all.equal(values[[1L]], values[[2L]], tolerance = 1e-8))) {
        abort(
            paste0(
                "The current multizone physical HVAC path requires AC_SYS ",
                "SUPPLY_T_MIN and SUPPLY_T_MAX to define the same hourly ",
                "temperature trajectory."
            ),
            class = "destep_unsupported_hvac_supply_temperature_range"
        )
    }

    minimum_row <- schedules[SCHEDULE_ID == schedule_ids[[1L]]]
    checkmate::assert_data_table(minimum_row, nrows = 1L)
    data.table::data.table(
        supply_temperature_schedule_id = schedule_ids[[1L]],
        supply_temperature_schedule = minimum_row$NAME[[1L]],
        cooling_design_supply_temperature_c = min(values[[1L]])
    )
}

# Read and validate the conditioned-room ownership records used by physical HVAC.
hvac__conditioned_controls <- function(dest) {
    controls <- control__room_table(dest)
    controls <- controls[
        !is.na(IS_AC_ROOM) &
            IS_AC_ROOM != 0L &
            !is.na(OF_AC_SYS) &
            OF_AC_SYS != 0L
    ]
    checkmate::assert_data_table(
        controls,
        min.rows = 1L,
        .var.name = "conditioned DeST room controls"
    )
    ideal_loads__assert_schedules(controls)
    data.table::setorderv(controls, c("OF_AC_SYS", "ROOM_ID"))
    controls
}

# Read source-backed fields for one single-zone type-0 air system.
hvac__single_zone_cav_source <- function(dest, system_id) {
    checkmate::assert_number(
        system_id,
        finite = TRUE,
        .var.name = "DeST AC_SYS_ID"
    )
    required <- list(
        AC_SYS = c(
            "AC_SYS_ID",
            "NAME",
            "AC_SYS_TYPE",
            "WATER_TYPE",
            "FRESH_AIR_TYPE",
            "MIN_FRESH_AIR_VOLUME",
            "MAX_FRESH_AIR_VOLUME"
        ),
        AHU = c("AHU_ID", "OF_AC_SYS", "HEATER"),
        ROOM = c(
            "ID",
            "VOLUME",
            "SET_AIR_FLOWNUM_MAX",
            "OF_ROOM_GROUP"
        )
    )
    for (table in names(required)) {
        if (
            !db_has_rows(dest, table) ||
                !db_has_fields(dest, table, required[[table]])
        ) {
            stop(
                sprintf(
                    "Physical HVAC conversion requires DeST table %s with fields: %s.",
                    table,
                    paste(required[[table]], collapse = ", ")
                ),
                call. = FALSE
            )
        }
    }

    controls <- hvac__conditioned_controls(dest)
    controls <- controls[OF_AC_SYS == system_id]
    checkmate::assert_data_table(
        controls,
        nrows = 1L,
        .var.name = paste("one conditioned room for AC_SYS", system_id)
    )

    rooms <- data.table::as.data.table(DBI::dbReadTable(dest, "ROOM"))
    rooms <- rooms[ID == controls$ROOM_ID[[1L]]]
    checkmate::assert_data_table(
        rooms,
        nrows = 1L,
        .var.name = "one physical-HVAC ROOM"
    )

    systems <- data.table::as.data.table(DBI::dbReadTable(dest, "AC_SYS"))
    systems <- systems[AC_SYS_ID == system_id]
    checkmate::assert_data_table(
        systems,
        nrows = 1L,
        .var.name = paste("one physical-HVAC AC_SYS", system_id)
    )
    if (systems$AC_SYS_TYPE[[1L]] != 0L) {
        stop(
            paste0(
                "Single-zone physical HVAC conversion requires DeST ",
                "AC_SYS_TYPE=0; found ",
                systems$AC_SYS_TYPE[[1L]],
                " for AC_SYS ",
                system_id,
                "."
            ),
            call. = FALSE
        )
    }
    if (!systems$FRESH_AIR_TYPE[[1L]] %in% c(1L, 5L, 6L)) {
        stop(
            paste0(
                "Physical HVAC conversion supports the verified DeST ",
                "FRESH_AIR_TYPE values 1, 5, and 6; found ",
                systems$FRESH_AIR_TYPE[[1L]],
                " for AC_SYS ",
                system_id,
                "."
            ),
            call. = FALSE
        )
    }

    air_handlers <- data.table::as.data.table(DBI::dbReadTable(dest, "AHU"))
    air_handlers <- air_handlers[OF_AC_SYS == system_id]
    checkmate::assert_data_table(
        air_handlers,
        nrows = 1L,
        .var.name = paste("one physical-HVAC AHU for AC_SYS", system_id)
    )

    supply_flow <- rooms$SET_AIR_FLOWNUM_MAX[[1L]] *
        rooms$VOLUME[[1L]] /
        3600
    outdoor_minimum <- systems$MIN_FRESH_AIR_VOLUME[[1L]] / 3600
    outdoor_maximum <- systems$MAX_FRESH_AIR_VOLUME[[1L]] / 3600
    checkmate::assert_number(
        supply_flow,
        lower = .Machine$double.eps,
        finite = TRUE,
        .var.name = "ROOM maximum supply-air flow"
    )
    checkmate::assert_number(
        outdoor_minimum,
        lower = .Machine$double.eps,
        finite = TRUE,
        .var.name = "AC_SYS minimum outdoor-air flow"
    )
    checkmate::assert_number(
        outdoor_maximum,
        lower = .Machine$double.eps,
        finite = TRUE,
        .var.name = "AC_SYS maximum outdoor-air flow"
    )
    checkmate::assert_true(
        outdoor_maximum >= outdoor_minimum,
        .var.name = "outdoor-air capacity not lower than fixed flow"
    )
    checkmate::assert_true(
        supply_flow >= outdoor_minimum,
        .var.name = "supply flow not lower than outdoor-air flow"
    )

    data.table::data.table(
        ac_system_id = systems$AC_SYS_ID[[1L]],
        ac_system_name = systems$NAME[[1L]],
        ac_system_type = systems$AC_SYS_TYPE[[1L]],
        source_water_type = systems$WATER_TYPE[[1L]],
        outdoor_air_control_type = systems$FRESH_AIR_TYPE[[1L]],
        economizer_type = hvac__economizer_type(
            systems$FRESH_AIR_TYPE[[1L]]
        ),
        ahu_id = air_handlers$AHU_ID[[1L]],
        source_heater_id = air_handlers$HEATER[[1L]],
        room_id = controls$ROOM_ID[[1L]],
        zone_name = controls$ROOM_NAME[[1L]],
        availability_schedule = controls$AC_SCHEDULE_NAME[[1L]],
        heating_schedule = controls$HEATING_SCHEDULE_NAME[[1L]],
        cooling_schedule = controls$COOLING_SCHEDULE_NAME[[1L]],
        supply_flow_m3_s = supply_flow,
        outdoor_air_flow_m3_s = outdoor_minimum,
        outdoor_air_capacity_m3_s = outdoor_maximum,
        return_air_flow_m3_s = supply_flow - outdoor_minimum
    )
}

# The independent heater field must not decide main water-coil technology.
# Zero and an unselected product (-1) add no independent heater; selected
# products need a separate mapping and cannot be silently replaced.
hvac__assert_heater_source <- function(heater_id, system_id) {
    checkmate::assert_number(system_id, finite = TRUE)
    if (!is.na(heater_id) && heater_id <= 0L) {
        return(invisible(TRUE))
    }
    abort(
        sprintf(
            "AC_SYS %s has an unresolved independent AHU.HEATER=%s product.",
            system_id,
            heater_id
        ),
        class = "destep_unsupported_hvac_heater_state"
    )
}

# List remaining temporary-template parameters. Room reheat and zone outdoor
# air are read or derived from source inputs rather than required user values.
hvac__terminal_required_options <- function() {
    c("cooling_coil_type", "preheat_coil_type")
}

# Validate external equipment values and source-selected reheat topology before
# temporary EnergyPlus templates can add zone equipment.
hvac__validate_terminal_options <- function(options, source) {
    checkmate::assert_list(source, names = "unique")
    checkmate::assert_names(
        names(source),
        must.include = c("system", "zones")
    )
    checkmate::assert_data_table(source$system, nrows = 1L)
    path <- if (source$system$ac_system_type[[1L]] == 0L) {
        "multizone_cav"
    } else {
        "multizone_vav"
    }
    hvac__assert_heater_source(
        source$system$source_heater_id[[1L]],
        source$system$ac_system_id[[1L]]
    )
    hvac__validate_options(options, path)
    checkmate::assert_choice(options$cooling_coil_type, "ChilledWater")
    checkmate::assert_choice(options$preheat_coil_type, "None")
    if ("reheat_coil_type" %in% names(options)) {
        abort(
            "Do not supply hvac_options$reheat_coil_type; terminal capacity and type belong to source ROOM records.",
            class = "destep_conflicting_hvac_equipment_option"
        )
    }
    # AHU reheat is a central component, not permission for zone reheat.
    # Stop until that separate component has an independently verified mapping.
    reheat_type <- source$system$source_reheat_type[[1L]]
    if (is.na(reheat_type) || reheat_type != 0L) {
        abort(
            sprintf(
                "AC_SYS %s has AHU.REHEAT_TYPE=%s; central AHU reheat is not yet mapped and cannot be substituted with room terminals.",
                source$system$ac_system_id[[1L]],
                reheat_type
            ),
            class = "destep_unsupported_hvac_reheat_type"
        )
    }
    if (
        any(source$zones$terminal_has_reheat & source$zones$terminal_type == 2L)
    ) {
        abort(
            "Source ROOM hot-water terminal reheat is not yet connected to a verified water loop.",
            class = "destep_unsupported_hvac_terminal_type"
        )
    }
    hvac__terminal_outdoor_air(source, options$zone_outdoor_air_flow_m3_s)
}

# Reconcile a small terminal-flow closure gap against minimum outdoor air.
hvac__reconcile_minimum_supply_flows <- function(
    minimum_supply_flows,
    minimum_outdoor_air_flow,
    relative_tolerance = 1e-4
) {
    checkmate::assert_numeric(
        minimum_supply_flows,
        min.len = 1L,
        lower = .Machine$double.eps,
        finite = TRUE
    )
    checkmate::assert_number(
        minimum_outdoor_air_flow,
        lower = .Machine$double.eps,
        finite = TRUE
    )
    checkmate::assert_number(
        relative_tolerance,
        lower = .Machine$double.eps,
        upper = 1,
        finite = TRUE
    )

    supply_total <- sum(minimum_supply_flows)
    deficit <- minimum_outdoor_air_flow - supply_total
    if (deficit <= 0) {
        return(minimum_supply_flows)
    }

    tolerance <- max(
        1e-8,
        minimum_outdoor_air_flow * relative_tolerance
    )
    if (deficit > tolerance) {
        abort(
            sprintf(
                paste0(
                    "ROOM minimum supply-air flows sum to %.9f m3/s, ",
                    "below the AC_SYS minimum outdoor-air flow %.9f m3/s ",
                    "by more than the %.4f%% reconciliation tolerance."
                ),
                supply_total,
                minimum_outdoor_air_flow,
                relative_tolerance * 100
            ),
            class = "destep_invalid_hvac_air_balance"
        )
    }

    # Preserve each zone's source share while closing the system air balance.
    reconciled <- minimum_supply_flows *
        minimum_outdoor_air_flow /
        supply_total
    reconciled[[length(reconciled)]] <- reconciled[[length(reconciled)]] +
        minimum_outdoor_air_flow -
        sum(reconciled)
    if (deficit > 1e-8) {
        warn(
            sprintf(
                paste0(
                    "ROOM minimum supply-air flows sum to %.9f m3/s, ",
                    "slightly below the AC_SYS minimum outdoor-air flow ",
                    "%.9f m3/s. Terminal minimum flows were scaled ",
                    "proportionally to close the air balance."
                ),
                supply_total,
                minimum_outdoor_air_flow
            ),
            class = "destep_normalized_hvac_minimum_flow"
        )
    }
    reconciled
}

# Read one supported multizone airside ownership and flow graph. Central AHU
# reheat and each ROOM terminal definition are independent source inputs.
hvac__multizone_airside_source <- function(dest, system_id) {
    checkmate::assert_number(
        system_id,
        finite = TRUE,
        .var.name = "DeST AC_SYS_ID"
    )
    required <- list(
        AC_SYS = c(
            "AC_SYS_ID",
            "NAME",
            "AC_SYS_TYPE",
            "WATER_TYPE",
            "FRESH_AIR_TYPE",
            "MIN_FRESH_AIR_VOLUME",
            "MAX_FRESH_AIR_VOLUME",
            "SUPPLY_T_MIN",
            "SUPPLY_T_MAX"
        ),
        AHU = c("AHU_ID", "OF_AC_SYS", "HEATER", "REHEAT_TYPE"),
        ROOM = c(
            "ID",
            "VOLUME",
            "SET_AIR_FLOWNUM_MIN",
            "SET_AIR_FLOWNUM_MAX",
            "SET_TERMINAL_MAX",
            "MIN_FRESH_FLOW_NUM",
            "OF_ROOM_GROUP"
        )
    )
    for (table in names(required)) {
        if (
            !db_has_rows(dest, table) ||
                !db_has_fields(dest, table, required[[table]])
        ) {
            stop(
                sprintf(
                    "Multizone physical HVAC conversion requires DeST table %s with fields: %s.",
                    table,
                    paste(required[[table]], collapse = ", ")
                ),
                call. = FALSE
            )
        }
    }

    controls <- hvac__conditioned_controls(dest)
    controls <- controls[OF_AC_SYS == system_id]
    checkmate::assert_data_table(
        controls,
        min.rows = 1L,
        .var.name = paste("conditioned rooms for AC_SYS", system_id)
    )

    systems <- data.table::as.data.table(DBI::dbReadTable(dest, "AC_SYS"))
    systems <- systems[AC_SYS_ID == system_id]
    checkmate::assert_data_table(
        systems,
        nrows = 1L,
        .var.name = "one multizone physical-HVAC AC_SYS"
    )
    if (!systems$AC_SYS_TYPE[[1L]] %in% c(0L, 1L)) {
        stop(
            paste0(
                "The current multizone physical HVAC path supports DeST ",
                "AC_SYS_TYPE values 0 and 1; found ",
                systems$AC_SYS_TYPE[[1L]],
                "."
            ),
            call. = FALSE
        )
    }
    if (!systems$FRESH_AIR_TYPE[[1L]] %in% c(1L, 5L, 6L)) {
        stop(
            paste0(
                "The current physical HVAC path supports the verified ",
                "DeST FRESH_AIR_TYPE values 1, 5, and 6; found ",
                systems$FRESH_AIR_TYPE[[1L]],
                "."
            ),
            call. = FALSE
        )
    }

    supply_temperature <- hvac__supply_temperature_source(dest, systems)

    air_handlers <- data.table::as.data.table(DBI::dbReadTable(dest, "AHU"))
    air_handlers <- air_handlers[OF_AC_SYS == system_id]
    checkmate::assert_data_table(
        air_handlers,
        nrows = 1L,
        .var.name = "one multizone physical-HVAC AHU"
    )

    rooms <- data.table::as.data.table(DBI::dbReadTable(dest, "ROOM"))
    room_index <- match(controls$ROOM_ID, rooms$ID)
    checkmate::assert_integerish(
        room_index,
        len = nrow(controls),
        lower = 1L,
        any.missing = FALSE,
        .var.name = "conditioned ROOM references"
    )
    zones <- data.table::data.table(
        room_id = controls$ROOM_ID,
        zone_name = controls$ROOM_NAME,
        availability_schedule = controls$AC_SCHEDULE_NAME,
        heating_schedule = controls$HEATING_SCHEDULE_NAME,
        cooling_schedule = controls$COOLING_SCHEDULE_NAME,
        volume_m3 = rooms$VOLUME[room_index],
        minimum_air_changes_per_hour = rooms$SET_AIR_FLOWNUM_MIN[room_index],
        maximum_air_changes_per_hour = rooms$SET_AIR_FLOWNUM_MAX[room_index],
        source_minimum_outdoor_air_flow_m3_s = rooms$MIN_FRESH_FLOW_NUM[
            room_index
        ] /
            3600
    )
    terminals <- hvac__room_terminal_source(dest, zones$room_id)
    for (field in setdiff(names(terminals), "room_id")) {
        data.table::set(zones, NULL, field, terminals[[field]])
    }
    data.table::setorderv(zones, "room_id")
    zones[,
        source_minimum_supply_flow_m3_s := minimum_air_changes_per_hour *
            volume_m3 /
            3600
    ]
    zones[,
        maximum_supply_flow_m3_s := maximum_air_changes_per_hour *
            volume_m3 /
            3600
    ]
    checkmate::assert_numeric(
        zones$source_minimum_supply_flow_m3_s,
        len = nrow(zones),
        lower = .Machine$double.eps,
        finite = TRUE,
        .var.name = "ROOM declared minimum supply-air flows"
    )
    checkmate::assert_numeric(
        zones$maximum_supply_flow_m3_s,
        len = nrow(zones),
        lower = .Machine$double.eps,
        finite = TRUE,
        .var.name = "ROOM maximum supply-air flows"
    )
    checkmate::assert_true(
        all(
            zones$maximum_supply_flow_m3_s >=
                zones$source_minimum_supply_flow_m3_s
        ),
        .var.name = "ROOM maximum flows not lower than minimum flows"
    )

    # Type 0 operates each terminal at its design flow; the stored ROOM minimum
    # does not describe the constant-flow operating limit observed in AE300.
    if (systems$AC_SYS_TYPE[[1L]] == 0L) {
        zones[, minimum_supply_flow_m3_s := maximum_supply_flow_m3_s]
    } else {
        zones[,
            minimum_supply_flow_m3_s := source_minimum_supply_flow_m3_s
        ]
    }
    availability <- unique(zones$availability_schedule)
    checkmate::assert_character(
        availability,
        len = 1L,
        min.chars = 1L,
        .var.name = "one shared multizone availability schedule"
    )

    outdoor_minimum <- systems$MIN_FRESH_AIR_VOLUME[[1L]] / 3600
    outdoor_maximum <- systems$MAX_FRESH_AIR_VOLUME[[1L]] / 3600
    supply_maximum <- sum(zones$maximum_supply_flow_m3_s)
    checkmate::assert_number(
        outdoor_minimum,
        lower = .Machine$double.eps,
        finite = TRUE,
        .var.name = "AC_SYS minimum outdoor-air flow"
    )
    checkmate::assert_number(
        outdoor_maximum,
        lower = outdoor_minimum,
        finite = TRUE,
        .var.name = "AC_SYS maximum outdoor-air flow"
    )
    if (systems$AC_SYS_TYPE[[1L]] == 1L) {
        zones[,
            minimum_supply_flow_m3_s := hvac__reconcile_minimum_supply_flows(
                minimum_supply_flow_m3_s,
                outdoor_minimum
            )
        ]
    }
    checkmate::assert_true(
        all(
            zones$maximum_supply_flow_m3_s >= zones$minimum_supply_flow_m3_s
        ),
        .var.name = "normalized minimum flows not above maximum flows"
    )
    supply_minimum <- sum(zones$minimum_supply_flow_m3_s)
    checkmate::assert_true(
        supply_minimum >= outdoor_minimum,
        .var.name = "minimum supply flow not lower than outdoor-air flow"
    )
    checkmate::assert_true(
        supply_maximum + 1e-8 >= outdoor_maximum,
        .var.name = "maximum supply flow not lower than outdoor-air capacity"
    )

    list(
        system = data.table::data.table(
            ac_system_id = systems$AC_SYS_ID[[1L]],
            ac_system_name = systems$NAME[[1L]],
            ac_system_type = systems$AC_SYS_TYPE[[1L]],
            source_water_type = systems$WATER_TYPE[[1L]],
            outdoor_air_control_type = systems$FRESH_AIR_TYPE[[1L]],
            economizer_type = hvac__economizer_type(
                systems$FRESH_AIR_TYPE[[1L]]
            ),
            ahu_id = air_handlers$AHU_ID[[1L]],
            source_heater_id = air_handlers$HEATER[[1L]],
            source_reheat_type = air_handlers$REHEAT_TYPE[[1L]],
            availability_schedule = availability[[1L]],
            minimum_supply_flow_m3_s = supply_minimum,
            maximum_supply_flow_m3_s = supply_maximum,
            outdoor_air_flow_m3_s = outdoor_minimum,
            outdoor_air_capacity_m3_s = outdoor_maximum,
            return_air_capacity_m3_s = supply_maximum - outdoor_minimum,
            supply_temperature_schedule_id = supply_temperature$supply_temperature_schedule_id,
            supply_temperature_schedule = supply_temperature$supply_temperature_schedule,
            cooling_design_supply_temperature_c = supply_temperature$cooling_design_supply_temperature_c
        ),
        zones = zones
    )
}

# Add the temporary HVAC templates used to produce a complete direct graph.
hvac__add_single_zone_cav_template <- function(
    model,
    source,
    options,
    include_common = TRUE
) {
    checkmate::assert_class(model, "Idf")
    checkmate::assert_data_table(source, nrows = 1L)
    checkmate::assert_flag(include_common)
    hvac__validate_options(options)

    # Keep generated system names ASCII and stable across DeST language packs.
    system_name <- paste0("DeST AC_SYS ", source$ac_system_id[[1L]])
    thermostat_name <- paste0(
        source$zone_name[[1L]],
        " DeST HVAC Thermostat"
    )
    availability <- source$availability_schedule[[1L]]
    maximum_outdoor_air_flow <- hvac__maximum_outdoor_air_flow(source)

    model$add(
        `HVACTemplate:System:ConstantVolume` = list(
            name = system_name,
            system_availability_schedule_name = availability,
            supply_fan_maximum_flow_rate = source$supply_flow_m3_s[[1L]],
            supply_fan_total_efficiency = options$supply_fan_total_efficiency,
            supply_fan_delta_pressure = options$supply_fan_delta_pressure_pa,
            supply_fan_motor_efficiency = options$supply_fan_motor_efficiency,
            supply_fan_motor_in_air_stream_fraction = options$supply_fan_motor_in_air_fraction,
            cooling_coil_type = "ChilledWater",
            cooling_coil_setpoint_control_type = "ControlZone",
            cooling_coil_control_zone_name = source$zone_name[[1L]],
            cooling_coil_design_setpoint_temperature = options$cooling_coil_design_setpoint_c,
            heating_coil_type = options$heating_coil_type,
            heating_coil_availability_schedule_name = availability,
            heating_coil_setpoint_control_type = "ControlZone",
            heating_coil_control_zone_name = source$zone_name[[1L]],
            heating_coil_design_setpoint = options$heating_coil_design_setpoint_c,
            preheat_coil_type = "None",
            maximum_outdoor_air_flow_rate = maximum_outdoor_air_flow,
            minimum_outdoor_air_flow_rate = source$outdoor_air_flow_m3_s[[1L]],
            economizer_type = source$economizer_type[[1L]],
            supply_fan_placement = "DrawThrough",
            heat_recovery_type = "None",
            dehumidification_control_type = "None",
            humidifier_type = "None",
            return_fan = "Yes",
            return_fan_total_efficiency = options$return_fan_total_efficiency,
            return_fan_delta_pressure = options$return_fan_delta_pressure_pa,
            return_fan_motor_efficiency = options$return_fan_motor_efficiency,
            return_fan_motor_in_air_stream_fraction = options$return_fan_motor_in_air_fraction
        ),
        `HVACTemplate:Thermostat` = list(
            name = thermostat_name,
            heating_setpoint_schedule_name = source$heating_schedule[[1L]],
            cooling_setpoint_schedule_name = source$cooling_schedule[[1L]]
        ),
        `HVACTemplate:Zone:ConstantVolume` = list(
            zone_name = source$zone_name[[1L]],
            template_constant_volume_system_name = system_name,
            template_thermostat_name = thermostat_name,
            supply_air_maximum_flow_rate = source$supply_flow_m3_s[[1L]],
            outdoor_air_method = "Flow/Zone",
            outdoor_air_flow_rate_per_zone = source$outdoor_air_flow_m3_s[[1L]],
            reheat_coil_type = "None",
            baseboard_heating_type = "None"
        )
    )

    # Add shared sizing and plant templates only once across all air systems.
    if (include_common) {
        model$add(
            SimulationControl = list(
                do_zone_sizing_calculation = "Yes",
                do_system_sizing_calculation = "Yes",
                do_plant_sizing_calculation = "Yes",
                run_simulation_for_sizing_periods = "Yes",
                run_simulation_for_weather_file_run_periods = "Yes"
            ),
            `Sizing:Parameters` = list(
                heating_sizing_factor = 1,
                cooling_sizing_factor = 1,
                timesteps_in_averaging_window = 12L
            ),
            `SizingPeriod:WeatherFileDays` = list(
                name = "DeST Annual Sizing",
                begin_month = 1L,
                begin_day_of_month = 1L,
                end_month = 12L,
                end_day_of_month = 31L,
                day_of_week_for_start_day = "Monday",
                use_weather_file_daylight_saving_period = "Yes",
                use_weather_file_rain_and_snow_indicators = "No"
            )
        )
        hvac__add_boundary_plants(model, options)
    }
    invisible(model)
}

# Add temporary VAV templates with the source-selected terminal reheat state.
# Type-0 operation is imposed on the expanded direct objects below.
hvac__add_multizone_airside_template <- function(
    model,
    source,
    options,
    include_common = TRUE
) {
    checkmate::assert_class(model, "Idf")
    checkmate::assert_flag(include_common)
    source <- hvac__validate_terminal_options(options, source)
    system <- source$system
    zones <- source$zones
    system_name <- paste0("DeST AC_SYS ", system$ac_system_id[[1L]])
    availability <- system$availability_schedule[[1L]]

    model$add(
        `HVACTemplate:System:VAV` = list(
            name = system_name,
            system_availability_schedule_name = availability,
            supply_fan_maximum_flow_rate = system$maximum_supply_flow_m3_s[[
                1L
            ]],
            supply_fan_minimum_flow_rate = system$minimum_supply_flow_m3_s[[
                1L
            ]],
            supply_fan_total_efficiency = options$supply_fan_total_efficiency,
            supply_fan_delta_pressure = options$supply_fan_delta_pressure_pa,
            supply_fan_motor_efficiency = options$supply_fan_motor_efficiency,
            supply_fan_motor_in_air_stream_fraction = options$supply_fan_motor_in_air_fraction,
            cooling_coil_type = options$cooling_coil_type,
            cooling_coil_setpoint_schedule_name = system$supply_temperature_schedule[[
                1L
            ]],
            cooling_coil_design_setpoint = system$cooling_design_supply_temperature_c[[
                1L
            ]],
            heating_coil_type = "HotWater",
            heating_coil_availability_schedule_name = availability,
            heating_coil_setpoint_schedule_name = system$supply_temperature_schedule[[
                1L
            ]],
            preheat_coil_type = "None",
            maximum_outdoor_air_flow_rate = hvac__maximum_outdoor_air_flow(
                system
            ),
            minimum_outdoor_air_flow_rate = system$outdoor_air_flow_m3_s[[1L]],
            minimum_outdoor_air_control_type = "FixedMinimum",
            economizer_type = system$economizer_type[[1L]],
            economizer_lockout = "NoLockout",
            supply_fan_placement = "DrawThrough",
            supply_fan_part_load_power_coefficients = "InletVaneDampers",
            heat_recovery_type = "None",
            cooling_coil_setpoint_reset_type = "None",
            heating_coil_setpoint_reset_type = "None",
            dehumidification_control_type = "None",
            humidifier_type = "None",
            return_fan = "Yes",
            return_fan_total_efficiency = options$return_fan_total_efficiency,
            return_fan_delta_pressure = options$return_fan_delta_pressure_pa,
            return_fan_motor_efficiency = options$return_fan_motor_efficiency,
            return_fan_motor_in_air_stream_fraction = options$return_fan_motor_in_air_fraction
        )
    )

    for (index in seq_len(nrow(zones))) {
        thermostat_name <- paste0(
            "DeST ROOM ",
            zones$room_id[[index]],
            " HVAC Thermostat"
        )
        model$add(
            `HVACTemplate:Thermostat` = list(
                name = thermostat_name,
                heating_setpoint_schedule_name = zones$heating_schedule[[
                    index
                ]],
                cooling_setpoint_schedule_name = zones$cooling_schedule[[index]]
            )
        )
        model$add(
            `HVACTemplate:Zone:VAV` = list(
                zone_name = zones$zone_name[[index]],
                template_vav_system_name = system_name,
                template_thermostat_name = thermostat_name,
                supply_air_maximum_flow_rate = zones$maximum_supply_flow_m3_s[[
                    index
                ]],
                zone_minimum_air_flow_input_method = "FixedFlowRate",
                fixed_minimum_air_flow_rate = zones$minimum_supply_flow_m3_s[[
                    index
                ]],
                outdoor_air_method = "Flow/Zone",
                outdoor_air_flow_rate_per_zone = zones$outdoor_air_flow_m3_s[[
                    index
                ]],
                reheat_coil_type = if (zones$terminal_has_reheat[[index]]) {
                    "Electric"
                } else {
                    "None"
                },
                reheat_coil_availability_schedule_name = availability,
                damper_heating_action = "Reverse",
                baseboard_heating_type = "None",
                zone_cooling_design_supply_air_temperature_input_method = "SupplyAirTemperature",
                zone_cooling_design_supply_air_temperature = system$cooling_design_supply_temperature_c[[
                    1L
                ]]
            )
        )
    }

    # Add shared sizing and chilled-water templates only once across systems.
    if (include_common) {
        model$add(
            SimulationControl = list(
                do_zone_sizing_calculation = "Yes",
                do_system_sizing_calculation = "Yes",
                do_plant_sizing_calculation = "Yes",
                run_simulation_for_sizing_periods = "Yes",
                run_simulation_for_weather_file_run_periods = "Yes"
            ),
            `Sizing:Parameters` = list(
                heating_sizing_factor = 1,
                cooling_sizing_factor = 1,
                timesteps_in_averaging_window = 12L
            ),
            `SizingPeriod:WeatherFileDays` = list(
                name = "DeST Annual Sizing",
                begin_month = 1L,
                begin_day_of_month = 1L,
                end_month = 12L,
                end_day_of_month = 31L,
                day_of_week_for_start_day = "Monday",
                use_weather_file_daylight_saving_period = "Yes",
                use_weather_file_rain_and_snow_indicators = "No"
            )
        )
        hvac__add_boundary_plants(model, options)
    }
    invisible(model)
}

# Expand templates with the EnergyPlus installation matching the target IDD.
hvac__expand_templates <- function(model) {
    checkmate::assert_class(model, "Idf")
    version <- as.character(model$version())
    suppressMessages(eplusr::use_eplus(version))
    installation <- eplusr::eplus_config(version)
    executable <- file.path(installation$dir, "ExpandObjects")
    checkmate::assert_file_exists(executable, access = "x")

    working_directory <- tempfile("destep-hvac-expand-")
    dir.create(working_directory, recursive = TRUE, showWarnings = FALSE)
    on.exit(
        unlink(working_directory, recursive = TRUE, force = TRUE),
        add = TRUE
    )
    input_path <- file.path(working_directory, "in.idf")
    # Keep absolute Schedule:File dependencies in their user-owned directory;
    # copying them into this disposable expansion directory would lose them.
    model$save(input_path, overwrite = TRUE, copy_external = FALSE)
    writeLines(
        c("[program]", paste0("dir=", installation$dir, "/")),
        file.path(working_directory, "Energy+.ini")
    )
    process <- processx::run(
        executable,
        wd = working_directory,
        error_on_status = FALSE,
        echo = FALSE
    )
    if (process$status != 0L) {
        stop(
            paste(
                "EnergyPlus ExpandObjects failed.",
                process$stdout,
                process$stderr
            ),
            call. = FALSE
        )
    }
    expanded_path <- file.path(working_directory, "expanded.idf")
    checkmate::assert_file_exists(expanded_path, access = "r")
    # Detach the returned model from the directory removed on function exit.
    # This also lets a later save/transition resolve external files correctly.
    conv__detach_temporary_path(eplusr::read_idf(expanded_path, idd = version))
}

# Replace invalid loop volume tokens emitted by the 9.0.1 template expander.
hvac__normalize_loop_volumes <- function(model) {
    specifications <- list(
        c("PlantLoop", "plant_loop_volume"),
        c("CondenserLoop", "condenser_loop_volume")
    )
    for (specification in specifications) {
        class_name <- specification[[1L]]
        field_name <- specification[[2L]]
        if (!class_name %in% model$class_name()) {
            next
        }

        object_ids <- unlist(
            model$object_id(class = class_name),
            use.names = FALSE
        )
        for (object_id in object_ids) {
            arguments <- stats::setNames(list("autocalculate"), field_name)
            invisible(do.call(model$object(object_id)$set, arguments))
        }
    }
    invisible(model)
}

# Refine the expanded graph to preserve the verified single-zone flow balance.
hvac__refine_single_zone_cav <- function(model, source, options) {
    system_name <- paste0("DeST AC_SYS ", source$ac_system_id[[1L]])
    supply_flow <- source$supply_flow_m3_s[[1L]]
    outdoor_flow <- source$outdoor_air_flow_m3_s[[1L]]
    return_flow <- source$return_air_flow_m3_s[[1L]]
    supply_fan_name <- paste(system_name, "Supply Fan")
    return_fan_name <- paste(system_name, "Return Fan")
    zone_name <- source$zone_name[[1L]]
    checkmate::assert_string(
        zone_name,
        min.chars = 1L,
        .var.name = "normalized physical-HVAC zone name"
    )
    connections <- data.table::as.data.table(model$to_table(
        class = "ZoneHVAC:EquipmentConnections",
        wide = TRUE
    ))
    connection <- connections[`Zone Name` == zone_name]
    checkmate::assert_data_table(
        connection,
        nrows = 1L,
        .var.name = paste(zone_name, "equipment connection")
    )
    connection_id <- connection$id[[1L]]
    availability <- unname(unlist(
        model$object(supply_fan_name)$value(
            "availability_schedule_name"
        ),
        use.names = FALSE
    ))
    checkmate::assert_string(
        availability,
        min.chars = 1L,
        .var.name = "normalized HVAC availability schedule"
    )
    exhaust_fan_name <- paste(zone_name, "Exhaust Fan")
    exhaust_node_name <- paste(zone_name, "Exhaust Node")
    equipment_lists <- data.table::as.data.table(model$to_table(
        class = "ZoneHVAC:EquipmentList",
        wide = TRUE
    ))
    equipment_list <- equipment_lists[
        Name == connection$`Zone Conditioning Equipment List Name`[[1L]]
    ]
    checkmate::assert_data_table(
        equipment_list,
        nrows = 1L,
        .var.name = paste(zone_name, "equipment list")
    )
    equipment_type <- equipment_list$`Zone Equipment 1 Object Type`[[1L]]
    checkmate::assert_choice(
        equipment_type,
        c(
            "AirTerminal:SingleDuct:Uncontrolled",
            "ZoneHVAC:AirDistributionUnit"
        ),
        .var.name = paste(zone_name, "first equipment type")
    )
    terminal_name <- equipment_list$`Zone Equipment 1 Name`[[1L]]
    terminal_type <- equipment_type
    # Newer ExpandObjects wraps the constant-volume terminal in an ADU.
    # Follow that explicit reference, preserving its inlet/outlet topology.
    if (equipment_type == "ZoneHVAC:AirDistributionUnit") {
        unit <- model$object(terminal_name)
        terminal_type <- unname(unlist(unit$value("air_terminal_object_type")))
        terminal_name <- unname(unlist(unit$value("air_terminal_name")))
        checkmate::assert_choice(
            terminal_type,
            "AirTerminal:SingleDuct:ConstantVolume:NoReheat",
            .var.name = paste(zone_name, "constant-volume terminal type")
        )
    }
    terminals <- data.table::as.data.table(model$to_table(
        class = terminal_type,
        wide = TRUE
    ))
    terminal <- terminals[Name == terminal_name]
    checkmate::assert_data_table(
        terminal,
        nrows = 1L,
        .var.name = paste(zone_name, "constant-volume terminal")
    )
    equipment_list_id <- equipment_list$id[[1L]]
    terminal_id <- terminal$id[[1L]]

    # The return path carries supply flow minus the balanced outdoor-air flow.
    suppressMessages(model$del(return_fan_name, .force = TRUE))
    model$add(
        `Fan:VariableVolume` = list(
            name = return_fan_name,
            availability_schedule_name = availability,
            fan_total_efficiency = options$return_fan_total_efficiency,
            pressure_rise = options$return_fan_delta_pressure_pa,
            maximum_flow_rate = return_flow,
            fan_power_minimum_flow_rate_input_method = "Fraction",
            fan_power_minimum_flow_fraction = 0,
            motor_efficiency = options$return_fan_motor_efficiency,
            motor_in_airstream_fraction = options$return_fan_motor_in_air_fraction,
            fan_power_coefficient_1 = options$return_fan_power_coefficient_1,
            fan_power_coefficient_2 = options$return_fan_power_coefficient_2,
            fan_power_coefficient_3 = options$return_fan_power_coefficient_3,
            fan_power_coefficient_4 = options$return_fan_power_coefficient_4,
            fan_power_coefficient_5 = options$return_fan_power_coefficient_5,
            air_inlet_node_name = paste(system_name, "Air Loop Inlet"),
            air_outlet_node_name = paste(system_name, "Return Fan Outlet")
        ),
        `Fan:ZoneExhaust` = list(
            name = exhaust_fan_name,
            availability_schedule_name = availability,
            fan_total_efficiency = options$zone_exhaust_fan_total_efficiency,
            pressure_rise = options$zone_exhaust_fan_pressure_rise_pa,
            maximum_flow_rate = outdoor_flow,
            air_inlet_node_name = exhaust_node_name,
            air_outlet_node_name = paste(zone_name, "Exhaust Fan Outlet"),
            end_use_subcategory = "General"
        ),
        NodeList = list(
            name = paste(system_name, "Mixed Air Nodes"),
            node_1_name = paste(system_name, "Cooling Coil Outlet"),
            node_2_name = paste(system_name, "Mixed Air Outlet")
        )
    )

    # Fix source-backed flow limits and order the two equivalent water stages.
    # No independent electric heater is synthesized from AHU.HEATER = -1.
    heating_coil_class <- "Coil:Heating:Water"
    model$object(system_name)$set(
        design_supply_air_flow_rate = supply_flow
    )
    model$object(terminal_id)$set(maximum_air_flow_rate = supply_flow)
    model$object(supply_fan_name)$set(
        maximum_flow_rate = supply_flow,
        air_inlet_node_name = paste(system_name, "Cooling Coil Outlet")
    )
    model$object(paste(system_name, "Main Branch"))$set(
        component_1_object_type = "Fan:VariableVolume",
        component_3_object_type = heating_coil_class,
        component_3_name = paste(system_name, "Heating Coil"),
        component_3_inlet_node_name = paste(system_name, "Mixed Air Outlet"),
        component_3_outlet_node_name = paste(
            system_name,
            "Heating Coil Outlet"
        ),
        component_4_object_type = "Coil:Cooling:Water",
        component_4_name = paste(system_name, "Cooling Coil"),
        component_4_inlet_node_name = paste(system_name, "Heating Coil Outlet"),
        component_4_outlet_node_name = paste(
            system_name,
            "Cooling Coil Outlet"
        ),
        component_5_inlet_node_name = paste(system_name, "Cooling Coil Outlet")
    )
    heating_coil <- model$object(paste(system_name, "Heating Coil"))
    heating_coil$set(
        air_inlet_node_name = paste(system_name, "Mixed Air Outlet"),
        air_outlet_node_name = paste(system_name, "Heating Coil Outlet")
    )
    if (options$heating_coil_type == "HotWater") {
        heating_coil$set(
            rated_ratio_for_air_and_water_convection = options$heating_coil_rated_air_water_convection_ratio
        )
    }
    model$object(paste(system_name, "Cooling Coil"))$set(
        design_air_flow_rate = supply_flow,
        air_inlet_node_name = paste(system_name, "Heating Coil Outlet"),
        air_outlet_node_name = paste(system_name, "Cooling Coil Outlet")
    )

    # The constant-volume template creates a second mixed-air manager whose
    # fan inlet becomes invalid after the heating and cooling coils are
    # reordered. Keeping it causes the economizer to stop below full outdoor
    # air even while the cooling coil is active.
    stale_manager_name <- paste(system_name, "Economizer Air Temp Manager")
    mixed_air_managers <- unname(unlist(
        model$object_name(class = "SetpointManager:MixedAir"),
        use.names = FALSE
    ))
    if (stale_manager_name %in% mixed_air_managers) {
        suppressMessages(model$del(stale_manager_name, .force = TRUE))
    }
    model$object(paste(system_name, "Cooling Coil Air Temp Manager"))$set(
        fan_inlet_node_name = paste(system_name, "Cooling Coil Outlet"),
        setpoint_node_or_nodelist_name = paste(system_name, "Mixed Air Nodes")
    )
    if (options$heating_coil_type == "HotWater") {
        model$object(paste(system_name, "Controllers"))$set(
            controller_1_name = paste(system_name, "Heating Coil Controller"),
            controller_2_name = paste(system_name, "Cooling Coil Controller")
        )
    }
    if (isTRUE(options$water_boundary)) {
        # Both native feedback managers respect the actual source air bounds,
        # rather than the unrelated template defaults of 12 C and 36 C.
        for (role in c("Heating", "Cooling")) {
            model$object(paste(
                system_name,
                role,
                "Supply Air Temp Manager"
            ))$set(
                minimum_supply_air_temperature = options$cooling_coil_design_setpoint_c,
                maximum_supply_air_temperature = options$heating_coil_design_setpoint_c
            )
        }
    }

    # Set every source-backed economizer field explicitly after expansion.
    model$object(paste(system_name, "OA Controller"))$set(
        minimum_outdoor_air_flow_rate = outdoor_flow,
        maximum_outdoor_air_flow_rate = hvac__maximum_outdoor_air_flow(source),
        economizer_control_type = source$economizer_type[[1L]],
        economizer_control_action_type = "ModulateFlow",
        lockout_type = "NoLockout",
        minimum_limit_type = "FixedMinimum"
    )

    # Register the balanced exhaust path as the second zone equipment item.
    model$object(equipment_list_id)$set(
        zone_equipment_2_object_type = "Fan:ZoneExhaust",
        zone_equipment_2_name = exhaust_fan_name,
        zone_equipment_2_cooling_sequence = 2,
        zone_equipment_2_heating_or_no_load_sequence = 2
    )
    model$object(connection_id)$set(
        zone_air_exhaust_node_or_nodelist_name = exhaust_node_name
    )
    hvac__normalize_loop_volumes(model)
    invisible(model)
}

# Refresh one single-zone source name after model-wide ASCII normalization.
hvac__refresh_single_zone_name <- function(model, source, zone_id) {
    checkmate::assert_class(model, "Idf")
    checkmate::assert_data_table(source, nrows = 1L)
    checkmate::assert_int(zone_id, lower = 1L)
    zones <- data.table::as.data.table(model$to_table(
        class = "Zone",
        wide = TRUE
    ))
    normalized_name <- zones[id == zone_id, Name]
    checkmate::assert_string(
        normalized_name,
        min.chars = 1L,
        .var.name = "normalized single-zone physical-HVAC name"
    )
    source[, zone_name := normalized_name]
    source
}

# Resolve stable Zone object identifiers before 9.0.1 name normalization.
hvac__zone_object_ids <- function(model, zone_names) {
    checkmate::assert_class(model, "Idf")
    checkmate::assert_character(
        zone_names,
        min.len = 1L,
        min.chars = 1L,
        any.missing = FALSE,
        unique = TRUE
    )
    zones <- data.table::as.data.table(model$to_table(
        class = "Zone",
        wide = TRUE
    ))
    zone_ids <- zones$id[match(zone_names, zones$Name)]
    checkmate::assert_integerish(
        zone_ids,
        len = length(zone_names),
        lower = 1L,
        any.missing = FALSE,
        unique = TRUE,
        .var.name = "physical-HVAC Zone object identifiers"
    )
    as.integer(zone_ids)
}

# Refresh source zone names after the model-wide ASCII normalization pass.
hvac__refresh_zone_names <- function(model, source, zone_ids) {
    checkmate::assert_class(model, "Idf")
    checkmate::assert_list(source, names = "unique")
    checkmate::assert_integerish(
        zone_ids,
        len = nrow(source$zones),
        lower = 1L,
        any.missing = FALSE,
        unique = TRUE
    )
    zones <- data.table::as.data.table(model$to_table(
        class = "Zone",
        wide = TRUE
    ))
    normalized_names <- zones$Name[match(zone_ids, zones$id)]
    checkmate::assert_character(
        normalized_names,
        len = length(zone_ids),
        min.chars = 1L,
        any.missing = FALSE,
        unique = TRUE,
        .var.name = "normalized physical-HVAC zone names"
    )
    source$zones[, zone_name := normalized_names]
    source
}

# Replace the temporary variable-volume fans with type-0 constant-flow fans.
hvac__replace_multizone_constant_volume_fans <- function(
    model,
    system,
    options,
    availability
) {
    checkmate::assert_class(model, "Idf")
    checkmate::assert_data_table(system, nrows = 1L)
    checkmate::assert_string(availability, min.chars = 1L)

    system_name <- paste0("DeST AC_SYS ", system$ac_system_id[[1L]])
    supply_fan_name <- paste(system_name, "Supply Fan")
    return_fan_name <- paste(system_name, "Return Fan")
    main_branch_name <- paste(system_name, "Main Branch")
    suppressMessages(model$del(
        c(return_fan_name, supply_fan_name),
        .force = TRUE
    ))

    # Type 0 fixes both central fan paths at their source-backed design flows.
    model$add(
        `Fan:ConstantVolume` = list(
            name = return_fan_name,
            availability_schedule_name = availability,
            fan_total_efficiency = options$return_fan_total_efficiency,
            pressure_rise = options$return_fan_delta_pressure_pa,
            maximum_flow_rate = system$return_air_capacity_m3_s[[1L]],
            motor_efficiency = options$return_fan_motor_efficiency,
            motor_in_airstream_fraction = options$return_fan_motor_in_air_fraction,
            air_inlet_node_name = paste(system_name, "Air Loop Inlet"),
            air_outlet_node_name = paste(system_name, "Return Fan Outlet")
        )
    )
    model$add(
        `Fan:ConstantVolume` = list(
            name = supply_fan_name,
            availability_schedule_name = availability,
            fan_total_efficiency = options$supply_fan_total_efficiency,
            pressure_rise = options$supply_fan_delta_pressure_pa,
            maximum_flow_rate = system$maximum_supply_flow_m3_s[[1L]],
            motor_efficiency = options$supply_fan_motor_efficiency,
            motor_in_airstream_fraction = options$supply_fan_motor_in_air_fraction,
            air_inlet_node_name = paste(system_name, "Cooling Coil Outlet"),
            air_outlet_node_name = paste(system_name, "Supply Fan Outlet")
        )
    )
    # Replace both fan type references atomically. Adding a main water heating
    # stage shifts the supply fan to slot 5; an intermediate stale fan type
    # would make the whole branch invalid after deleting variable-volume fans.
    arguments <- list(
        component_1_object_type = "Fan:ConstantVolume",
        component_1_name = return_fan_name,
        component_1_inlet_node_name = paste(system_name, "Air Loop Inlet"),
        component_1_outlet_node_name = paste(system_name, "Return Fan Outlet")
    )
    inlet <- paste(
        system_name,
        if (isTRUE(options$water_boundary)) {
            "Heating Coil Outlet"
        } else {
            "Cooling Coil Outlet"
        }
    )
    model$object(supply_fan_name)$set(air_inlet_node_name = inlet)
    slot <- if (isTRUE(options$water_boundary)) 5L else 4L
    supply_fields <- stats::setNames(
        list(
            "Fan:ConstantVolume",
            supply_fan_name,
            inlet,
            paste(system_name, "Supply Fan Outlet")
        ),
        paste0(
            "component_",
            slot,
            c("_object_type", "_name", "_inlet_node_name", "_outlet_node_name")
        )
    )
    do.call(model$object(main_branch_name)$set, c(arguments, supply_fields))
    invisible(model)
}

# Set source-backed terminal limits, fan semantics, and balanced exhaust paths.
hvac__refine_multizone_airside <- function(model, source, options) {
    checkmate::assert_class(model, "Idf")
    source <- hvac__validate_terminal_options(options, source)
    system <- source$system
    zones <- source$zones
    system_name <- paste0("DeST AC_SYS ", system$ac_system_id[[1L]])
    supply_fan_name <- paste(system_name, "Supply Fan")
    return_fan_name <- paste(system_name, "Return Fan")
    availability <- unname(unlist(
        model$object(supply_fan_name)$value(
            "availability_schedule_name"
        ),
        use.names = FALSE
    ))
    checkmate::assert_string(
        availability,
        min.chars = 1L,
        .var.name = "normalized multizone availability schedule"
    )

    # Template expansion leaves these source-backed design limits autosized.
    model$object(system_name)$set(
        design_supply_air_flow_rate = system$maximum_supply_flow_m3_s[[1L]]
    )
    if (system$ac_system_type[[1L]] == 0L) {
        hvac__replace_multizone_constant_volume_fans(
            model,
            system,
            options,
            availability
        )
    } else {
        # Return-fan overrides must not change the supply fan's native
        # InletVaneDampers curve selected by its own HVAC template.
        model$object(supply_fan_name)$set(
            maximum_flow_rate = system$maximum_supply_flow_m3_s[[1L]],
            fan_power_minimum_flow_rate_input_method = "FixedFlowRate",
            fan_power_minimum_air_flow_rate = system$minimum_supply_flow_m3_s[[
                1L
            ]]
        )
        model$object(return_fan_name)$set(
            maximum_flow_rate = system$return_air_capacity_m3_s[[1L]],
            fan_power_minimum_flow_rate_input_method = "Fraction",
            fan_power_minimum_flow_fraction = 0,
            fan_power_coefficient_1 = options$return_fan_power_coefficient_1,
            fan_power_coefficient_2 = options$return_fan_power_coefficient_2,
            fan_power_coefficient_3 = options$return_fan_power_coefficient_3,
            fan_power_coefficient_4 = options$return_fan_power_coefficient_4,
            fan_power_coefficient_5 = options$return_fan_power_coefficient_5
        )
    }
    model$object(paste(system_name, "Cooling Coil"))$set(
        design_air_flow_rate = system$maximum_supply_flow_m3_s[[1L]]
    )
    model$object(paste(system_name, "OA Controller"))$set(
        minimum_outdoor_air_flow_rate = system$outdoor_air_flow_m3_s[[1L]],
        maximum_outdoor_air_flow_rate = hvac__maximum_outdoor_air_flow(system),
        economizer_control_type = system$economizer_type[[1L]],
        economizer_control_action_type = "ModulateFlow",
        lockout_type = "NoLockout",
        minimum_limit_type = "FixedMinimum"
    )

    connections <- data.table::as.data.table(model$to_table(
        class = "ZoneHVAC:EquipmentConnections",
        wide = TRUE
    ))
    equipment_lists <- data.table::as.data.table(model$to_table(
        class = "ZoneHVAC:EquipmentList",
        wide = TRUE
    ))
    # Terminal classes vary per ROOM; an AHU code cannot select one class
    # for all zones. Resolve both classes before updating each named terminal.
    terminal_suffixes <- data.table::fifelse(
        zones$terminal_has_reheat,
        "VAV Reheat",
        "VAV"
    )
    terminal_classes <- intersect(
        c(
            "AirTerminal:SingleDuct:VAV:Reheat",
            "AirTerminal:SingleDuct:VAV:NoReheat"
        ),
        model$class_name()
    )
    terminals <- data.table::rbindlist(
        lapply(
            terminal_classes,
            function(class) model$to_table(class = class, wide = TRUE)
        ),
        fill = TRUE
    )
    # Restrict generated objects to this AC_SYS before checking cardinality.
    connections <- connections[`Zone Name` %in% zones$zone_name]
    equipment_names <- connections$`Zone Conditioning Equipment List Name`
    equipment_lists <- equipment_lists[Name %in% equipment_names]
    terminal_names <- paste(zones$zone_name, terminal_suffixes)
    terminals <- terminals[Name %in% terminal_names]
    checkmate::assert_data_table(connections, nrows = nrow(zones))
    checkmate::assert_data_table(equipment_lists, nrows = nrow(zones))
    checkmate::assert_data_table(terminals, nrows = nrow(zones))

    for (index in seq_len(nrow(zones))) {
        zone_name <- zones$zone_name[[index]]
        terminal_name <- terminal_names[[index]]
        terminal_id <- terminals$id[match(terminal_name, terminals$Name)]
        checkmate::assert_int(
            terminal_id,
            lower = 1L,
            .var.name = paste(zone_name, "VAV terminal")
        )
        model$object(terminal_id)$set(
            maximum_air_flow_rate = zones$maximum_supply_flow_m3_s[[index]],
            zone_minimum_air_flow_input_method = "FixedFlowRate",
            fixed_minimum_air_flow_rate = zones$minimum_supply_flow_m3_s[[
                index
            ]]
        )

        if (zones$terminal_has_reheat[[index]]) {
            # Source watts are the coil's actual nominal ceiling, not an
            # autosizing hint or a power density multiplied by zone area.
            terminal_row <- match(terminal_id, terminals$id)
            coil_name <- terminals$`Reheat Coil Name`[[terminal_row]]
            checkmate::assert_string(coil_name, min.chars = 1L)
            model$object(coil_name)$set(
                nominal_capacity = zones$terminal_capacity_w[[index]]
            )
        }

        connection <- connections[`Zone Name` == zone_name]
        checkmate::assert_data_table(
            connection,
            nrows = 1L,
            .var.name = paste(zone_name, "equipment connection")
        )
        equipment_list <- equipment_lists[
            Name == connection$`Zone Conditioning Equipment List Name`[[1L]]
        ]
        checkmate::assert_data_table(
            equipment_list,
            nrows = 1L,
            .var.name = paste(zone_name, "equipment list")
        )

        exhaust_fan_name <- paste(zone_name, "Exhaust Fan")
        exhaust_node_name <- paste(zone_name, "Exhaust Node")
        model$add(
            `Fan:ZoneExhaust` = list(
                name = exhaust_fan_name,
                availability_schedule_name = availability,
                fan_total_efficiency = options$zone_exhaust_fan_total_efficiency,
                pressure_rise = options$zone_exhaust_fan_pressure_rise_pa,
                maximum_flow_rate = zones$outdoor_air_flow_m3_s[[index]],
                air_inlet_node_name = exhaust_node_name,
                air_outlet_node_name = paste(zone_name, "Exhaust Fan Outlet"),
                end_use_subcategory = "General"
            )
        )
        model$object(equipment_list$id[[1L]])$set(
            zone_equipment_2_object_type = "Fan:ZoneExhaust",
            zone_equipment_2_name = exhaust_fan_name,
            zone_equipment_2_cooling_sequence = 2,
            zone_equipment_2_heating_or_no_load_sequence = 2
        )
        model$object(connection$id[[1L]])$set(
            zone_air_exhaust_node_or_nodelist_name = exhaust_node_name
        )
    }

    hvac__normalize_loop_volumes(model)
    invisible(model)
}

# Build one conversion descriptor for every referenced DeST AC_SYS.
hvac__system_sources <- function(dest, options = NULL) {
    if (!isTRUE(options$water_boundary)) {
        options <- hvac__boundary_options(dest, options)
    }
    controls <- hvac__conditioned_controls(dest)
    systems <- data.table::as.data.table(DBI::dbReadTable(dest, "AC_SYS"))
    system_ids <- sort(unique(controls$OF_AC_SYS))
    sources <- lapply(system_ids, function(system_id) {
        system <- systems[AC_SYS_ID == system_id]
        checkmate::assert_data_table(
            system,
            nrows = 1L,
            .var.name = paste("one referenced AC_SYS", system_id)
        )
        room_count <- controls[OF_AC_SYS == system_id, .N]
        system_type <- system$AC_SYS_TYPE[[1L]]
        checkmate::assert_choice(
            as.character(system_type),
            c("0", "1"),
            .var.name = paste("AC_SYS_TYPE for AC_SYS", system_id)
        )

        # A one-room type-0 system uses the dedicated constant-volume graph.
        if (system_type == 0L && room_count == 1L) {
            path <- "single_zone_cav"
            source <- hvac__single_zone_cav_source(dest, system_id)
            # The existing one-room refinement assumes an uncontrolled
            # terminal. Do not silently omit a source room reheater there.
            terminal <- hvac__room_terminal_source(dest, source$room_id)
            if (terminal$terminal_has_reheat[[1L]]) {
                abort(
                    "Single-zone CAV ROOM terminal reheat is not yet mapped; its source capacity cannot be omitted.",
                    class = "destep_unsupported_hvac_terminal_type"
                )
            }
            if (!isTRUE(options$water_boundary)) {
                hvac__assert_heater_source(
                    source$source_heater_id[[1L]],
                    system_id
                )
            }
        } else {
            path <- if (system_type == 0L) {
                "multizone_cav"
            } else {
                "multizone_vav"
            }
            source <- hvac__multizone_airside_source(dest, system_id)
            effective <- if (isTRUE(options$water_boundary)) {
                hvac__boundary_system_options(source, path, options$overrides)
            } else {
                options
            }
            source <- hvac__validate_terminal_options(effective, source)
        }
        effective <- if (isTRUE(options$water_boundary)) {
            hvac__boundary_system_options(source, path, options$overrides)
        } else {
            options
        }
        water <- NULL
        if (isTRUE(options$water_boundary)) {
            system_source <- if (path == "single_zone_cav") {
                source
            } else {
                source$system
            }
            if (
                is.na(system_source$source_heater_id[[1L]]) ||
                    system_source$source_heater_id[[1L]] > 0L
            ) {
                abort(
                    "Selected independent AHU heater product is not yet mapped.",
                    class = "destep_unsupported_hvac_heater_state"
                )
            }
            water <- hvac__water_boundary_source(
                dest,
                system_source$ahu_id[[1L]]
            )
            if (path == "single_zone_cav") {
                bounds <- hvac__single_supply_bounds(dest, system_id)
                effective$cooling_coil_design_setpoint_c <- bounds[[1L]]
                effective$heating_coil_design_setpoint_c <- bounds[[2L]]
            }
        }
        list(
            system_id = system_id,
            path = path,
            source = source,
            options = effective,
            water = water
        )
    })

    # Reject unused or misspelled room allocations after all systems are known.
    terminal_sources <- sources[vapply(
        sources,
        function(source) source$path != "single_zone_cav",
        logical(1L)
    )]
    allocation <- options$overrides$zone_outdoor_air_flow_m3_s
    if (!length(terminal_sources) && !is.null(allocation)) {
        abort(
            "Outdoor-air allocation overrides require a multizone HVAC path.",
            class = "destep_unused_hvac_equipment_options"
        )
    }
    if (length(terminal_sources) > 0L && !is.null(allocation)) {
        room_ids <- unlist(
            lapply(
                terminal_sources,
                function(source) as.character(source$source$zones$room_id)
            ),
            use.names = FALSE
        )
        checkmate::assert_set_equal(
            names(allocation),
            room_ids,
            .var.name = paste0(
                "names(hvac_options$zone_outdoor_air_flow_m3_s); ",
                "use conditioned DeST ROOM.ID values"
            )
        )
    }

    # Keep stable expansion order; both paths now use equivalent water stages
    # and prescribed water boundaries rather than a user-selected heating plant.
    priority <- vapply(
        sources,
        function(source) source$path != "single_zone_cav",
        logical(1L)
    )
    ids <- vapply(sources, function(source) source$system_id, numeric(1L))
    sources[order(priority, ids)]
}

# Require the target objects used by the selected source paths. Tested release
# numbers describe evidence, not a compatibility whitelist. Object creation
# additionally validates every emitted field against this same target IDD.
hvac__assert_target_capabilities <- function(model, paths) {
    required <- c(
        "PlantComponent:TemperatureSource",
        "HVACTemplate:Thermostat",
        "HVACTemplate:Plant:ChilledWaterLoop",
        "HVACTemplate:Plant:Chiller",
        "HVACTemplate:Plant:HotWaterLoop",
        "HVACTemplate:Plant:Boiler",
        if ("single_zone_cav" %in% paths) {
            c(
                "HVACTemplate:System:ConstantVolume",
                "HVACTemplate:Zone:ConstantVolume"
            )
        },
        if (any(paths != "single_zone_cav")) {
            c("HVACTemplate:System:VAV", "HVACTemplate:Zone:VAV")
        }
    )
    missing <- setdiff(required, model$class_name(all = TRUE))
    if (length(missing)) {
        abort(
            paste0(
                "Physical HVAC requires target IDD classes missing from EnergyPlus ",
                model$version(),
                ": ",
                paste(missing, collapse = ", "),
                "."
            ),
            class = "destep_unsupported_hvac_target_capability"
        )
    }
    invisible(model)
}

# Expand supported DeST air systems using the installed target-version tools.
hvac__convert <- function(dest, model, options) {
    # Source schedules control operation; missing target-only inputs receive
    # per-system defaults instead of requiring a fictitious plant selection.
    boundary <- hvac__boundary_options(dest, options)
    sources <- hvac__system_sources(dest, boundary)
    hvac__assert_target_capabilities(
        model,
        vapply(sources, `[[`, character(1L), "path")
    )
    water <- data.table::rbindlist(lapply(sources, `[[`, "water"))
    # Shared template loops are valid only for identical operational boundaries.
    # Do not attach different AHU water schedules to one shared loop.
    if (
        any(
            vapply(
                split(water$schedule_id, water$role),
                function(ids) length(unique(ids)),
                integer(1L)
            ) !=
                1L
        )
    ) {
        abort(
            "Different AHU water schedules require independent water-loop ownership, which is not yet mapped.",
            class = "destep_unsupported_hvac_water_loop_mapping"
        )
    }
    water <- water[!duplicated(water$role)]
    water_ids <- vapply(
        water$schedule_name,
        function(name) model$object(name)$id(),
        integer(1L)
    )
    zone_ids <- vector("list", length(sources))

    for (index in seq_along(sources)) {
        descriptor <- sources[[index]]
        source <- descriptor$source
        options <- descriptor$options
        include_common <- index == 1L
        if (descriptor$path == "single_zone_cav") {
            hvac__add_single_zone_cav_template(
                model,
                source,
                options,
                include_common
            )
            zone_ids[[index]] <- hvac__zone_object_ids(
                model,
                source$zone_name
            )
        } else {
            hvac__add_multizone_airside_template(
                model,
                source,
                options,
                include_common
            )
            zone_ids[[index]] <- hvac__zone_object_ids(
                model,
                source$zones$zone_name
            )
        }
    }

    # Normalize the base model and every HVAC template as one reference graph
    # before the target-version preprocessor expands the objects.
    conv__normalize_object_names(model)
    for (index in seq_along(sources)) {
        if (sources[[index]]$path == "single_zone_cav") {
            sources[[index]]$source <- hvac__refresh_single_zone_name(
                model,
                sources[[index]]$source,
                zone_ids[[index]][[1L]]
            )
        } else {
            sources[[index]]$source <- hvac__refresh_zone_names(
                model,
                sources[[index]]$source,
                zone_ids[[index]]
            )
        }
    }

    data.table::set(
        water,
        NULL,
        "schedule_name",
        vapply(water_ids, function(id) model$object(id)$name(), character(1L))
    )
    direct <- hvac__expand_templates(model)
    hvac__normalize_loop_volumes(direct)
    hvac__replace_boundary_plants(direct, water)
    for (descriptor in sources) {
        options <- descriptor$options
        if (descriptor$path == "single_zone_cav") {
            hvac__refine_single_zone_cav(
                direct,
                descriptor$source,
                options
            )
        } else {
            hvac__refine_multizone_airside(
                direct,
                descriptor$source,
                options
            )
        }
    }
    conv__normalize_object_names(direct)
    checkmate::assert_true(
        direct$is_valid(),
        .var.name = "direct physical HVAC EnergyPlus model"
    )
    attr(direct, "hvac_terminals") <- data.table::rbindlist(
        lapply(
            sources,
            function(descriptor) {
                if (descriptor$path == "single_zone_cav") {
                    return(NULL)
                }
                selected <- data.table::copy(descriptor$source$zones)
                data.table::set(
                    selected,
                    NULL,
                    "ac_system_id",
                    rep(descriptor$system_id, nrow(selected))
                )
                selected
            }
        ),
        fill = TRUE
    )
    attr(direct, "hvac_water") <- water
    attr(direct, "hvac_effective_options") <- lapply(sources, function(item) {
        origins <- stats::setNames(
            rep("target_representation_default", length(item$options)),
            names(item$options)
        )
        origins[intersect(
            names(boundary$overrides),
            names(origins)
        )] <- "user_override"
        origins[c(
            "chilled_water_design_setpoint_c",
            "hot_water_design_setpoint_c"
        )] <- "converter_sizing_assumption"
        origins[c(
            "heating_coil_type",
            "cooling_coil_type"
        )] <- "source_water_coil_equivalent_stages"
        origins["preheat_coil_type"] <- "no_verified_source_preheat"
        if (item$path == "single_zone_cav") {
            origins[c(
                "cooling_coil_design_setpoint_c",
                "heating_coil_design_setpoint_c"
            )] <- "source_constant_supply_air_bounds"
        }
        list(
            system_id = item$system_id,
            options = item$options,
            origins = origins,
            water = item$water
        )
    })
    hvac__warn_terminal_defaults(attr(direct, "hvac_terminals"))
    direct
}
