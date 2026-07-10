# ROOM_GROUP temperature setpoint schedules -> ZoneControl:Thermostat.
# Each EnergyPlus zone needs its own control object, but rooms with identical
# heating/cooling schedule IDs can share the same DualSetpoint object.
destep_conv_thermostat <- function(dest, ep) {
    if (!destep_has_rows(dest, "ROOM") || !destep_has_rows(dest, "ROOM_GROUP")) {
        return(NULL)
    }

    thermostat <- destep_room_group_thermostat_table(dest)
    destep_assert_thermostat_schedules(thermostat)

    # A room group can exist without setpoint schedules. Keep those rows in the
    # diagnostic table but do not create incomplete EnergyPlus controls.
    skip_reason <- rep(NA_character_, nrow(thermostat))
    skip_reason[is.na(thermostat$ROOM_GROUP_ID)] <- "ROOM.OF_ROOM_GROUP does not reference ROOM_GROUP"
    missing_setpoint <- is.na(skip_reason) & !destep_has_thermostat_setpoints(thermostat)
    skip_reason[missing_setpoint] <- "ROOM_GROUP setpoint schedule is zero or missing"
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
        destep_thermostat_setpoint_names(thermostat)
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

    out <- destep_combine_outputs(list(
        control_type = destep_thermostat_control_type_schedule(dest, ep),
        setpoint = destep_thermostat_setpoint_objects(dest, ep, setpoint),
        control = destep_thermostat_control_objects(dest, ep, converted)
    ), table = thermostat)

    out
}

# Collect the room-level ROOM_GROUP thermostat inputs and resolve both setpoint
# schedule names up front, so downstream conversion can fail before writing IDF
# objects if a non-zero schedule ID is dangling.
destep_room_group_thermostat_table <- function(dest) {
    thermostat <- DBI::dbGetQuery(
        dest,
        "
        SELECT
            R.ID AS ROOM_ID,
            R.NAME AS ROOM_NAME,
            R.OF_ROOM_GROUP,
            G.ROOM_GROUP_ID,
            G.NAME AS ROOM_GROUP_NAME,
            G.IS_AC_ROOM,
            G.AC_SCHEDULE_ID,
            G.SET_T_MIN_SCHEDULE,
            S_MIN.NAME AS HEATING_SCHEDULE_NAME,
            G.SET_T_MAX_SCHEDULE,
            S_MAX.NAME AS COOLING_SCHEDULE_NAME
        FROM ROOM R
        LEFT JOIN ROOM_GROUP G
        ON R.OF_ROOM_GROUP = G.ROOM_GROUP_ID
        LEFT JOIN SCHEDULE_YEAR S_MIN
        ON G.SET_T_MIN_SCHEDULE = S_MIN.SCHEDULE_ID
        LEFT JOIN SCHEDULE_YEAR S_MAX
        ON G.SET_T_MAX_SCHEDULE = S_MAX.SCHEDULE_ID
        ORDER BY R.ID
        "
    )
    data.table::setDT(thermostat)
    thermostat
}

# Non-zero setpoint IDs are explicit foreign keys to SCHEDULE_YEAR. Missing
# targets should stop conversion instead of silently creating broken references.
destep_assert_thermostat_schedules <- function(thermostat) {
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
        "Cannot resolve ROOM_GROUP thermostat schedule(s) in SCHEDULE_YEAR: %s",
        detail
    ), call. = FALSE)
}

# A complete DeST thermostat needs both lower and upper temperature schedules.
# IS_AC_ROOM is deliberately not part of this predicate; it is diagnostic only.
destep_has_thermostat_setpoints <- function(thermostat) {
    !is.na(thermostat$SET_T_MIN_SCHEDULE) &
        thermostat$SET_T_MIN_SCHEDULE != 0L &
        !is.na(thermostat$SET_T_MAX_SCHEDULE) &
        thermostat$SET_T_MAX_SCHEDULE != 0L
}

# Use stable schedule-ID based names so identical ROOM_GROUP setpoint pairs
# share one ThermostatSetpoint:DualSetpoint object across all zones.
destep_thermostat_setpoint_names <- function(thermostat) {
    ifelse(
        destep_has_thermostat_setpoints(thermostat),
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
destep_thermostat_control_type_schedule <- function(dest, ep) {
    destep_add(
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
destep_thermostat_setpoint_objects <- function(dest, ep, setpoint) {
    values <- lapply(seq_len(nrow(setpoint)), function(i) {
        destep_thermostat_setpoint_value(setpoint, i)
    })

    eval(as.call(c(
        destep_add, dest, ep,
        lapply(values, function(val) bquote("ThermostatSetpoint:DualSetpoint" := .(val)))
    )))
}

# Build the DualSetpoint value list for one distinct schedule pair.
destep_thermostat_setpoint_value <- function(setpoint, i) {
    list(
        name = setpoint$ENERGYPLUS_SETPOINT_NAME[[i]],
        heating_setpoint_temperature_schedule_name = setpoint$HEATING_SCHEDULE_NAME[[i]],
        cooling_setpoint_temperature_schedule_name = setpoint$COOLING_SCHEDULE_NAME[[i]]
    )
}

# Create one ZoneControl:Thermostat per room while reusing the shared dual
# setpoint object selected by the room group's schedule pair.
destep_thermostat_control_objects <- function(dest, ep, thermostat) {
    values <- lapply(seq_len(nrow(thermostat)), function(i) {
        destep_thermostat_control_value(thermostat, i)
    })

    eval(as.call(c(
        destep_add, dest, ep,
        lapply(values, function(val) bquote("ZoneControl:Thermostat" := .(val)))
    )))
}

# Build the ZoneControl object value list for one converted room.
destep_thermostat_control_value <- function(thermostat, i) {
    list(
        name = thermostat$ENERGYPLUS_ZONE_CONTROL_NAME[[i]],
        zone_or_zonelist_name = thermostat$ROOM_NAME[[i]],
        control_type_schedule_name = "DeST Dual Setpoint Control Type",
        control_1_object_type = "ThermostatSetpoint:DualSetpoint",
        control_1_name = thermostat$ENERGYPLUS_SETPOINT_NAME[[i]]
    )
}
