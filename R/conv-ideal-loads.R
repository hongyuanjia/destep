# ROOM_GROUP air-conditioning availability -> ZoneHVAC:IdealLoadsAirSystem.
# This converter deliberately implements only the load-system shell: DeST
# supply-air state fields live in AC_SYS, and the current real fixture has no
# AC_SYS rows, so fixed EnergyPlus IdealLoads defaults are used here.
destep_conv_ideal_loads <- function(dest, ep) {
    if (!destep_has_rows(dest, "ROOM") || !destep_has_rows(dest, "ROOM_GROUP")) {
        return(NULL)
    }

    ideal <- destep_room_group_ideal_loads_table(dest)
    destep_assert_ideal_loads_schedules(ideal)

    # Non-air-conditioned rooms stay in the diagnostic table but do not get
    # zone equipment. AC_T_* tolerance schedules are intentionally not used.
    skip_reason <- rep(NA_character_, nrow(ideal))
    skip_reason[is.na(ideal$ROOM_GROUP_ID)] <- "ROOM.OF_ROOM_GROUP does not reference ROOM_GROUP"
    missing_ac_flag <- is.na(skip_reason) & is.na(ideal$IS_AC_ROOM)
    skip_reason[missing_ac_flag] <- "ROOM_GROUP.IS_AC_ROOM is missing"
    non_ac_room <- is.na(skip_reason) & ideal$IS_AC_ROOM == 0L
    skip_reason[non_ac_room] <- "ROOM_GROUP.IS_AC_ROOM is zero"
    missing_schedule <- is.na(skip_reason) &
        (is.na(ideal$AC_SCHEDULE_ID) | ideal$AC_SCHEDULE_ID == 0L)
    skip_reason[missing_schedule] <- "ROOM_GROUP.AC_SCHEDULE_ID is zero or missing"
    data.table::set(ideal, NULL, "SKIP_REASON", skip_reason)
    data.table::set(ideal, NULL, "CAN_CONVERT", is.na(skip_reason))
    ideal <- destep_ideal_loads_add_names(ideal)

    warning_skip <- !is.na(ideal$SKIP_REASON) &
        ideal$SKIP_REASON != "ROOM_GROUP.IS_AC_ROOM is zero"
    if (any(warning_skip)) {
        warn(sprintf(
            "Skipped %i ROOM row(s) that do not describe supported ROOM_GROUP ideal loads.",
            sum(warning_skip)
        ))
    }

    converted <- ideal[ideal$CAN_CONVERT]
    if (nrow(converted) == 0L) return(NULL)

    out <- destep_combine_outputs(list(
        ideal_loads = destep_ideal_loads_objects(dest, ep, converted),
        sequential_fraction = destep_ideal_loads_sequential_fraction_schedule(dest, ep),
        equipment_list = destep_ideal_loads_equipment_lists(dest, ep, converted),
        equipment_connections = destep_ideal_loads_equipment_connections(dest, ep, converted)
    ), table = ideal)

    out
}

# Collect room-group HVAC inputs and keep deferred DeST fields in the diagnostic
# table so future AC_SYS or AC_T_* work can compare against this converter.
destep_room_group_ideal_loads_table <- function(dest) {
    ideal <- DBI::dbGetQuery(
        dest,
        "
        SELECT
            R.ID AS ROOM_ID,
            R.NAME AS ROOM_NAME,
            R.OF_ROOM_GROUP,
            G.ROOM_GROUP_ID,
            G.NAME AS ROOM_GROUP_NAME,
            G.OF_AC_SYS,
            G.IS_AC_ROOM,
            G.AC_SCHEDULE_ID,
            S_AC.NAME AS AC_SCHEDULE_NAME,
            G.SET_RH_MIN_SCHEDULE,
            G.SET_RH_MAX_SCHEDULE,
            G.AC_T_MIN_SCHEDULE,
            G.AC_T_MAX_SCHEDULE
        FROM ROOM R
        LEFT JOIN ROOM_GROUP G
        ON R.OF_ROOM_GROUP = G.ROOM_GROUP_ID
        LEFT JOIN SCHEDULE_YEAR S_AC
        ON G.AC_SCHEDULE_ID = S_AC.SCHEDULE_ID
        ORDER BY R.ID
        "
    )
    data.table::setDT(ideal)
    ideal
}

# Air-conditioned room groups with a non-zero availability schedule must resolve
# to SCHEDULE_YEAR; otherwise the generated IdealLoads objects would be invalid.
destep_assert_ideal_loads_schedules <- function(ideal) {
    needs_schedule <- !is.na(ideal$IS_AC_ROOM) & ideal$IS_AC_ROOM != 0L &
        !is.na(ideal$AC_SCHEDULE_ID) & ideal$AC_SCHEDULE_ID != 0L
    unresolved <- needs_schedule & is.na(ideal$AC_SCHEDULE_NAME)

    if (!any(unresolved)) {
        return(invisible(NULL))
    }

    rows <- ideal[unresolved]
    detail <- paste(sprintf(
        "%s: AC_SCHEDULE_ID=%s",
        rows$ROOM_NAME,
        rows$AC_SCHEDULE_ID
    ), collapse = "; ")

    stop(sprintf(
        "Cannot resolve ROOM_GROUP ideal-loads schedule(s) in SCHEDULE_YEAR: %s",
        detail
    ), call. = FALSE)
}

# Generate stable, per-room node and object names required by the EnergyPlus
# zone equipment loop objects.
destep_ideal_loads_add_names <- function(ideal) {
    data.table::set(
        ideal, NULL, "ENERGYPLUS_IDEAL_LOADS_NAME",
        make_unique_name(paste(ideal$ROOM_NAME, "Ideal Loads"))
    )
    data.table::set(
        ideal, NULL, "ENERGYPLUS_EQUIPMENT_LIST_NAME",
        make_unique_name(paste(ideal$ROOM_NAME, "Equipment"))
    )
    data.table::set(
        ideal, NULL, "ZONE_SUPPLY_AIR_NODE_NAME",
        make_unique_name(paste(ideal$ROOM_NAME, "Ideal Loads Supply Node"))
    )
    data.table::set(
        ideal, NULL, "ZONE_EXHAUST_AIR_NODE_NAME",
        make_unique_name(paste(ideal$ROOM_NAME, "Ideal Loads Exhaust Node"))
    )
    data.table::set(
        ideal, NULL, "ZONE_AIR_NODE_NAME",
        make_unique_name(paste(ideal$ROOM_NAME, "Zone Air Node"))
    )
    data.table::set(
        ideal, NULL, "ZONE_RETURN_AIR_NODE_NAME",
        make_unique_name(paste(ideal$ROOM_NAME, "Zone Return Air Node"))
    )
    ideal
}

# Create IdealLoads systems with explicit EnergyPlus default-style supply-air
# limits; these are not inferred from DeST AC_T_* tolerance schedules.
destep_ideal_loads_objects <- function(dest, ep, ideal) {
    values <- lapply(seq_len(nrow(ideal)), function(i) {
        destep_ideal_loads_value(ideal, i)
    })

    eval(as.call(c(
        destep_add, dest, ep,
        lapply(values, function(val) bquote("ZoneHVAC:IdealLoadsAirSystem" := .(val)))
    )))
}

# Build one ZoneHVAC:IdealLoadsAirSystem value list.
destep_ideal_loads_value <- function(ideal, i) {
    list(
        name = ideal$ENERGYPLUS_IDEAL_LOADS_NAME[[i]],
        availability_schedule_name = ideal$AC_SCHEDULE_NAME[[i]],
        zone_supply_air_node_name = ideal$ZONE_SUPPLY_AIR_NODE_NAME[[i]],
        zone_exhaust_air_node_name = ideal$ZONE_EXHAUST_AIR_NODE_NAME[[i]],
        system_inlet_air_node_name = NULL,
        maximum_heating_supply_air_temperature = 50,
        minimum_cooling_supply_air_temperature = 13,
        maximum_heating_supply_air_humidity_ratio = 0.015,
        minimum_cooling_supply_air_humidity_ratio = 0.009,
        heating_limit = "NoLimit",
        maximum_heating_air_flow_rate = NULL,
        maximum_sensible_heating_capacity = NULL,
        cooling_limit = "NoLimit",
        maximum_cooling_air_flow_rate = NULL,
        maximum_total_cooling_capacity = NULL,
        heating_availability_schedule_name = NULL,
        cooling_availability_schedule_name = NULL,
        dehumidification_control_type = "ConstantSensibleHeatRatio",
        cooling_sensible_heat_ratio = 0.7,
        humidification_control_type = "None",
        design_specification_outdoor_air_object_name = NULL,
        outdoor_air_inlet_node_name = NULL,
        demand_controlled_ventilation_type = "None",
        outdoor_air_economizer_type = "NoEconomizer",
        heat_recovery_type = "None",
        sensible_heat_recovery_effectiveness = 0,
        latent_heat_recovery_effectiveness = 0
    )
}

# The EquipmentList extensible group is complete only when the sequential load
# fraction schedule names are present, so a single constant-one schedule is
# shared by every generated IdealLoads equipment list.
destep_ideal_loads_sequential_fraction_schedule <- function(dest, ep) {
    destep_add(
        dest, ep,
        "Schedule:Constant" := list(
            name = "DeST Ideal Loads Sequential Fraction",
            schedule_type_limits_name = NULL,
            hourly_value = 1
        )
    )
}

# Each conditioned zone gets a one-item equipment list that points to its own
# IdealLoads system.
destep_ideal_loads_equipment_lists <- function(dest, ep, ideal) {
    values <- lapply(seq_len(nrow(ideal)), function(i) {
        destep_ideal_loads_equipment_list_value(ideal, i)
    })

    eval(as.call(c(
        destep_add, dest, ep,
        lapply(values, function(val) bquote("ZoneHVAC:EquipmentList" := .(val)))
    )))
}

# Build one ZoneHVAC:EquipmentList value list.
destep_ideal_loads_equipment_list_value <- function(ideal, i) {
    list(
        name = ideal$ENERGYPLUS_EQUIPMENT_LIST_NAME[[i]],
        load_distribution_scheme = "SequentialLoad",
        zone_equipment_1_object_type = "ZoneHVAC:IdealLoadsAirSystem",
        zone_equipment_1_name = ideal$ENERGYPLUS_IDEAL_LOADS_NAME[[i]],
        zone_equipment_1_cooling_sequence = 1,
        zone_equipment_1_heating_or_no_load_sequence = 1,
        zone_equipment_1_sequential_cooling_fraction_schedule_name = "DeST Ideal Loads Sequential Fraction",
        zone_equipment_1_sequential_heating_fraction_schedule_name = "DeST Ideal Loads Sequential Fraction"
    )
}

# ZoneHVAC:EquipmentConnections closes the loop between a Zone, its equipment
# list, and the air nodes used by the IdealLoads system.
destep_ideal_loads_equipment_connections <- function(dest, ep, ideal) {
    values <- lapply(seq_len(nrow(ideal)), function(i) {
        destep_ideal_loads_equipment_connection_value(ideal, i)
    })

    eval(as.call(c(
        destep_add, dest, ep,
        lapply(values, function(val) bquote("ZoneHVAC:EquipmentConnections" := .(val)))
    )))
}

# Build one ZoneHVAC:EquipmentConnections value list.
destep_ideal_loads_equipment_connection_value <- function(ideal, i) {
    list(
        zone_name = ideal$ROOM_NAME[[i]],
        zone_conditioning_equipment_list_name = ideal$ENERGYPLUS_EQUIPMENT_LIST_NAME[[i]],
        zone_air_inlet_node_or_nodelist_name = ideal$ZONE_SUPPLY_AIR_NODE_NAME[[i]],
        zone_air_exhaust_node_or_nodelist_name = ideal$ZONE_EXHAUST_AIR_NODE_NAME[[i]],
        zone_air_node_name = ideal$ZONE_AIR_NODE_NAME[[i]],
        zone_return_air_node_or_nodelist_name = ideal$ZONE_RETURN_AIR_NODE_NAME[[i]]
    )
}
