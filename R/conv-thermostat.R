# ROOM_TYPE_DATA temperature setpoint schedules -> ZoneControl:Thermostat.
# Each EnergyPlus zone needs its own control object, but rooms with identical
# heating/cooling schedule IDs can share the same DualSetpoint object.
thermostat__convert <- function(dest, ep) {
    if (!db_has_rows(dest, "ROOM") || !db_has_rows(dest, "ROOM_GROUP") ||
        !db_has_rows(dest, "ROOM_TYPE_DATA")) {
        return(NULL)
    }

    thermostat <- control__room_table(dest)
    thermostat__assert_schedules(thermostat)

    # A room group can exist without setpoint schedules. Keep those rows in the
    # diagnostic table but do not create incomplete EnergyPlus controls.
    skip_reason <- rep(NA_character_, nrow(thermostat))
    skip_reason[is.na(thermostat$ROOM_GROUP_ID)] <- "ROOM.OF_ROOM_GROUP does not reference ROOM_GROUP"
    missing_room_type <- is.na(skip_reason) &
        is.na(thermostat$ROOM_TYPE_DATA_ID)
    skip_reason[missing_room_type] <- "ROOM.TYPE does not reference ROOM_TYPE_DATA"
    missing_setpoint <- is.na(skip_reason) & !thermostat__has_setpoints(thermostat)
    skip_reason[missing_setpoint] <- "ROOM_TYPE_DATA setpoint schedule is zero or missing"
    # A ZoneControl:Thermostat is valid only for a Zone with equipment. Keep
    # this predicate aligned with IdealLoads so unconditioned DeST rooms do not
    # become EnergyPlus controlled zones without EquipmentConnections.
    unsupported_zone <- is.na(skip_reason) & (
        is.na(thermostat$IS_AC_ROOM) |
            thermostat$IS_AC_ROOM == 0L |
            is.na(thermostat$AC_SCHEDULE_ID) |
            thermostat$AC_SCHEDULE_ID == 0L
    )
    skip_reason[unsupported_zone] <- "ROOM_GROUP does not describe a supported ideal loads zone"
    data.table::set(thermostat, NULL, "SKIP_REASON", skip_reason)
    data.table::set(thermostat, NULL, "CAN_CONVERT", is.na(skip_reason))
    data.table::set(
        thermostat, NULL, "ENERGYPLUS_SETPOINT_NAME",
        thermostat__setpoint_names(thermostat)
    )
    data.table::set(
        thermostat, NULL, "ENERGYPLUS_ZONE_CONTROL_NAME",
        make_unique_name(paste(thermostat$ROOM_NAME, "Thermostat"))
    )

    if (any(!thermostat$CAN_CONVERT)) {
        warn(sprintf(
            "Skipped %i ROOM row(s) that do not describe supported controlled zones.",
            sum(!thermostat$CAN_CONVERT)
        ))
    }

    converted <- thermostat[thermostat$CAN_CONVERT]
    if (nrow(converted) == 0L) return(NULL)

    setpoint <- unique(converted[, c(
        "SET_T_MIN_SCHEDULE", "HEATING_SCHEDULE_NAME",
        "SET_T_MAX_SCHEDULE", "COOLING_SCHEDULE_NAME",
        "ENERGYPLUS_SETPOINT_NAME"
    ), with = FALSE])

    out <- conv__combine_outputs(list(
        control_type = thermostat__control_type_schedule(dest, ep),
        setpoint = thermostat__setpoint_objects(dest, ep, setpoint),
        control = thermostat__control_objects(dest, ep, converted)
    ), table = thermostat)

    out
}

# Non-zero setpoint IDs are explicit foreign keys to SCHEDULE_YEAR. Missing
# targets should stop conversion instead of silently creating broken references.
thermostat__assert_schedules <- function(thermostat) {
    has_heat <- !is.na(thermostat$SET_T_MIN_SCHEDULE) & thermostat$SET_T_MIN_SCHEDULE != 0L
    has_cool <- !is.na(thermostat$SET_T_MAX_SCHEDULE) & thermostat$SET_T_MAX_SCHEDULE != 0L
    unresolved_heat <- has_heat & is.na(thermostat$HEATING_SCHEDULE_NAME)
    unresolved_cool <- has_cool & is.na(thermostat$COOLING_SCHEDULE_NAME)

    if (!any(unresolved_heat | unresolved_cool)) {
        return(invisible(NULL))
    }

    unresolved <- thermostat[unresolved_heat | unresolved_cool]
    detail <- paste(sprintf(
        "%s: SET_T_MIN_SCHEDULE=%s, SET_T_MAX_SCHEDULE=%s",
        unresolved$ROOM_NAME,
        unresolved$SET_T_MIN_SCHEDULE,
        unresolved$SET_T_MAX_SCHEDULE
    ), collapse = "; ")

    stop(sprintf(
        "Cannot resolve ROOM_TYPE_DATA thermostat schedule(s) in SCHEDULE_YEAR: %s",
        detail
    ), call. = FALSE)
}

# A complete DeST thermostat needs both lower and upper temperature schedules.
# IS_AC_ROOM is deliberately not part of this predicate; it is diagnostic only.
thermostat__has_setpoints <- function(thermostat) {
    !is.na(thermostat$SET_T_MIN_SCHEDULE) &
        thermostat$SET_T_MIN_SCHEDULE != 0L &
        !is.na(thermostat$SET_T_MAX_SCHEDULE) &
        thermostat$SET_T_MAX_SCHEDULE != 0L
}

# Use stable schedule-ID based names so identical ROOM_TYPE_DATA setpoint pairs
# share one ThermostatSetpoint:DualSetpoint object across all zones.
thermostat__setpoint_names <- function(thermostat) {
    ifelse(
        thermostat__has_setpoints(thermostat),
        sprintf(
            "DeST Dual Setpoint H%s C%s",
            thermostat$SET_T_MIN_SCHEDULE,
            thermostat$SET_T_MAX_SCHEDULE
        ),
        NA_character_
    )
}

# EnergyPlus uses control type 4 to select ThermostatSetpoint:DualSetpoint. A
# single constant schedule can be shared by every ZoneControl:Thermostat.
thermostat__control_type_schedule <- function(dest, ep) {
    conv__add(
        dest, ep,
        "Schedule:Constant" := list(
            name = "DeST Dual Setpoint Control Type",
            schedule_type_limits_name = NULL,
            hourly_value = 4
        )
    )
}

# Create one shared DualSetpoint object for each distinct DeST lower/upper
# temperature schedule pair.
thermostat__setpoint_objects <- function(dest, ep, setpoint) {
    values <- lapply(seq_len(nrow(setpoint)), function(i) {
        thermostat__setpoint_value(setpoint, i)
    })

    conv__add_objects(dest, ep, "ThermostatSetpoint:DualSetpoint", values)
}

# Build the DualSetpoint value list for one distinct schedule pair.
thermostat__setpoint_value <- function(setpoint, i) {
    list(
        name = setpoint$ENERGYPLUS_SETPOINT_NAME[[i]],
        heating_setpoint_temperature_schedule_name = setpoint$HEATING_SCHEDULE_NAME[[i]],
        cooling_setpoint_temperature_schedule_name = setpoint$COOLING_SCHEDULE_NAME[[i]]
    )
}

# Create one ZoneControl:Thermostat per room while reusing the shared dual
# setpoint object selected by the room type's schedule pair.
thermostat__control_objects <- function(dest, ep, thermostat) {
    values <- lapply(seq_len(nrow(thermostat)), function(i) {
        thermostat__control_value(thermostat, i)
    })

    conv__add_objects(dest, ep, "ZoneControl:Thermostat", values)
}

# Build the ZoneControl object value list for one converted room.
thermostat__control_value <- function(thermostat, i) {
    list(
        name = thermostat$ENERGYPLUS_ZONE_CONTROL_NAME[[i]],
        zone_or_zonelist_name = thermostat$ROOM_NAME[[i]],
        control_type_schedule_name = "DeST Dual Setpoint Control Type",
        control_1_object_type = "ThermostatSetpoint:DualSetpoint",
        control_1_name = thermostat$ENERGYPLUS_SETPOINT_NAME[[i]]
    )
}
