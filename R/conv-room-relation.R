# ROOM_RELATION -> ZoneVentilation:DesignFlowRate for relations connected to OUTSIDE.
# VENT_SCHEDULE_ID is a foreign key to SCHEDULE_YEAR. The Access field comment
# says that the referenced 8760-value schedule corresponds to air changes, so
# those hourly schedule values are treated as hourly ACH values. VENT_SET_MAX is
# retained in the diagnostic table until its unit is confirmed from DeST.
destep_conv_room_ventilation <- function(dest, ep) {
    if (!destep_has_rows(dest, "ROOM_RELATION")) return(NULL)

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
        ORDER BY RR.ID
        "
    )
    data.table::setDT(relation)
    data.table::set(relation, NULL, "IS_OUTDOOR_RELATION", !is.na(relation$OUTSIDE_NAME))

    # Only OUTSIDE-linked records have an unambiguous ZoneVentilation mapping.
    # Inter-room records need a separate ZoneMixing interpretation.
    skip_reason <- rep(NA_character_, nrow(relation))
    skip_reason[!relation$IS_OUTDOOR_RELATION] <- "RELA_ROOM_ID does not reference OUTSIDE"
    skip_reason[is.na(relation$ROOM_NAME)] <- "ROOM_ID does not reference ROOM"
    skip_reason[is.na(relation$SCHEDULE_NAME)] <- "VENT_SCHEDULE_ID does not reference SCHEDULE_YEAR"
    data.table::set(relation, NULL, "SKIP_REASON", skip_reason)
    data.table::set(relation, NULL, "CAN_CONVERT", is.na(skip_reason))
    # EnergyPlus multiplies the ACH design level by the schedule fraction/value;
    # use a unit ACH design level so the referenced DeST schedule DATA values
    # pass through as the actual hourly ACH sequence.
    data.table::set(relation, NULL, "AIR_CHANGES_PER_HOUR", 1)
    data.table::set(relation, NULL, "ENERGYPLUS_NAME", destep_room_ventilation_names(relation))

    if (any(!relation$CAN_CONVERT)) {
        warn(sprintf(
            "Skipped %i ROOM_RELATION row(s) that do not describe supported outdoor ventilation.",
            sum(!relation$CAN_CONVERT)
        ))
    }

    ventilation <- relation[relation$CAN_CONVERT]
    if (nrow(ventilation) == 0L) return(NULL)

    values <- lapply(seq_len(nrow(ventilation)), function(i) {
        destep_room_ventilation_value(ventilation, i)
    })
    out <- eval(as.call(c(
        destep_add, dest, ep,
        lapply(values, function(val) bquote("ZoneVentilation:DesignFlowRate" := .(val)))
    )))

    attr(out, "table") <- relation

    out
}

# Build stable EnergyPlus object names for ROOM_RELATION records whose DeST NAME
# is usually empty or a literal dot in observed models.
destep_room_ventilation_names <- function(relation) {
    raw_name <- relation$NAME
    use_room_name <- is.na(raw_name) | raw_name == "." | !nzchar(raw_name)
    raw_name[use_room_name] <- paste(relation$ROOM_NAME[use_room_name], "Outdoor Ventilation")
    raw_name[is.na(raw_name)] <- paste("ROOM_RELATION", relation$ID[is.na(raw_name)])

    make_unique_name(raw_name)
}

# Create the EnergyPlus ventilation object value list for one external
# ROOM_RELATION row.
destep_room_ventilation_value <- function(ventilation, i) {
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
