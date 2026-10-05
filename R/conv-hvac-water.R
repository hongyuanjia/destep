# Resolve operational water temperatures from linked AHU properties, not from
# catalogue design temperatures or the legacy AC_SYS.WATER_TYPE column.
hvac__water_boundary_source <- function(dest, ahu_id) {
    handler <- DBI::dbGetQuery(
        dest,
        "SELECT COOLING_COIL, REHEAT_TYPE FROM AHU WHERE AHU_ID=?",
        params = list(ahu_id)
    )
    if (
        nrow(handler) != 1L ||
            is.na(handler$COOLING_COIL) ||
            handler$COOLING_COIL <= 0L
    ) {
        abort(
            "A selected source water coil is required; an absent coil cannot be synthesized.",
            class = "destep_unsupported_hvac_water_coil"
        )
    }
    if (is.na(handler$REHEAT_TYPE) || handler$REHEAT_TYPE != 0L) {
        abort(
            "Central AHU reheat is not yet mapped separately from the main water coil.",
            class = "destep_unsupported_hvac_reheat_type"
        )
    }
    properties <- hvac__read_ahu_properties(dest, ahu_id)
    water_type <- properties$data_long[properties$name == "AHU_WATER_TYPE"]
    if (length(water_type) != 1L || !water_type %in% c(1L, 2L)) {
        abort(
            "AHU water type must resolve to two-pipe (1) or four-pipe (2).",
            class = "destep_unresolved_hvac_water_boundary"
        )
    }
    keys <- if (water_type == 1L) {
        rep("AHU_TWO_PIPE_WATER_SCH", 2L)
    } else {
        c("AHU_FOUR_PIPE_COLD_WATER_SCH", "AHU_FOUR_PIPE_HOT_WATER_SCH")
    }
    selected <- lapply(keys, function(key) {
        ids <- properties$schedule_id[properties$name == key]
        if (length(ids) != 1L || is.na(ids) || ids < 1L) {
            abort(
                paste("Cannot resolve source water schedule", key),
                class = "destep_unresolved_hvac_water_boundary"
            )
        }
        schedule <- DBI::dbGetQuery(
            dest,
            "SELECT SCHEDULE_ID, NAME, DATA FROM SCHEDULE_YEAR WHERE SCHEDULE_ID=?",
            params = list(ids)
        )
        if (nrow(schedule) != 1L || length(schedule$DATA[[1L]]) != 8760L * 8L) {
            abort(
                paste("Invalid annual source water schedule", ids),
                class = "destep_unresolved_hvac_water_boundary"
            )
        }
        values <- readBin(
            schedule$DATA[[1L]],
            "double",
            8760L,
            endian = "little"
        )
        checkmate::assert_numeric(
            values,
            len = 8760L,
            finite = TRUE,
            lower = 0.1,
            upper = 99.9,
            .var.name = paste("water temperature", ids)
        )
        list(
            schedule_id = ids,
            schedule_name = schedule$NAME[[1L]],
            minimum_c = min(values),
            maximum_c = max(values)
        )
    })
    data.table::data.table(
        ahu_id = ahu_id,
        water_type = as.integer(water_type),
        role = c("cooling", "heating"),
        schedule_id = vapply(selected, `[[`, integer(1L), "schedule_id"),
        schedule_name = vapply(selected, `[[`, character(1L), "schedule_name"),
        minimum_c = vapply(selected, `[[`, numeric(1L), "minimum_c"),
        maximum_c = vapply(selected, `[[`, numeric(1L), "maximum_c")
    )
}

# Constant source supply-air bounds constrain native zone-feedback managers.
# Varying bounds need a separate verified control representation; replacing
# them with annual extrema would lose operational input semantics.
hvac__single_supply_bounds <- function(dest, system_id) {
    system <- DBI::dbGetQuery(
        dest,
        "SELECT SUPPLY_T_MIN, SUPPLY_T_MAX FROM AC_SYS WHERE AC_SYS_ID=?",
        params = list(system_id)
    )
    checkmate::assert_data_frame(system, nrows = 1L)
    # Validate raw foreign keys before coercion can truncate a fractional ID.
    ids <- unlist(system, use.names = FALSE)
    checkmate::assert_integerish(
        ids,
        tol = 0,
        len = 2L,
        lower = 1L,
        upper = .Machine$integer.max,
        any.missing = FALSE,
        .var.name = "AC_SYS SUPPLY_T_MIN/MAX references"
    )
    ids <- as.integer(ids)
    values <- vapply(
        ids,
        function(id) {
            row <- DBI::dbGetQuery(
                dest,
                "SELECT DATA FROM SCHEDULE_YEAR WHERE SCHEDULE_ID=?",
                params = list(id)
            )
            checkmate::assert_data_frame(row, nrows = 1L)
            hours <- schedule__decode(row$DATA[[1L]], as.character(id))
            checkmate::assert_numeric(
                hours,
                len = 8760L,
                finite = TRUE,
                any.missing = FALSE
            )
            if (any(hours != hours[[1L]])) {
                abort(
                    "Varying single-zone supply-air bounds are not yet mapped.",
                    class = "destep_unsupported_hvac_supply_temperature_schedule"
                )
            }
            hours[[1L]]
        },
        numeric(1L)
    )
    if (values[[1L]] > values[[2L]]) {
        abort(
            "Source minimum supply-air temperature exceeds its maximum.",
            class = "destep_invalid_hvac_supply_temperature_schedule"
        )
    }
    values
}

# Supply target representation defaults only after checking that no selected
# source plant would be lost. These are explicit EnergyPlus template defaults,
# not inferred DeST fan powers or source equipment ratings.
hvac__boundary_options <- function(dest, overrides = NULL) {
    selected <- hvac__read_model_plant(dest)
    if (any(vapply(selected, nrow, integer(1L)) > 0L)) {
        abort(
            "Selected source plant needs a verified ownership and equipment mapping; it cannot be replaced by a prescribed water boundary.",
            class = "destep_unsupported_hvac_plant_mapping"
        )
    }
    if (db_has_fields(dest, "AHU", "AHURES")) {
        networks <- DBI::dbGetQuery(dest, "SELECT AHURES FROM AHU")$AHURES
        if (any(!is.na(networks) & networks > 0L)) {
            abort(
                "Selected source duct-network/fan inputs require their own verified mapping; target fan defaults cannot replace them.",
                class = "destep_unsupported_hvac_fan_mapping"
            )
        }
    }
    forbidden <- c(
        "chiller_type",
        "chiller_nominal_cop",
        "tower_type",
        "boiler_type",
        "boiler_efficiency",
        "boiler_fuel_type",
        "condenser_water_design_setpoint_c",
        "chilled_water_design_setpoint_c",
        "hot_water_design_setpoint_c",
        "heating_coil_type",
        "cooling_coil_type",
        "preheat_coil_type",
        "preheat_coil_design_setpoint_c",
        "cooling_coil_design_setpoint_c",
        "heating_coil_design_setpoint_c",
        "reheat_coil_type",
        "water_boundary"
    )
    if (is.null(overrides)) {
        overrides <- list()
    }
    checkmate::assert_list(overrides, names = "unique")
    if (any(names(overrides) %in% forbidden)) {
        abort(
            paste(
                "Source HVAC topology and water schedules cannot be overridden with:",
                paste(intersect(names(overrides), forbidden), collapse = ", ")
            ),
            class = "destep_conflicting_hvac_equipment_option"
        )
    }
    list(overrides = overrides, water_boundary = TRUE)
}

# Resolve defaults per system: a CAV system must not inherit VAV fan pressure.
# Zero-head balancing paths introduce no invented fan/pump electricity. Their
# representation is retained separately from actual selected equipment.
hvac__boundary_system_options <- function(source, path, overrides = list()) {
    variable <- path == "multizone_vav"
    defaults <- list(
        supply_fan_total_efficiency = 0.7,
        supply_fan_delta_pressure_pa = if (variable) 1000 else 600,
        supply_fan_motor_efficiency = 0.9,
        supply_fan_motor_in_air_fraction = 1,
        return_fan_total_efficiency = 0.7,
        return_fan_delta_pressure_pa = if (variable) 500 else 300,
        return_fan_motor_efficiency = 0.9,
        return_fan_motor_in_air_fraction = 1,
        return_fan_power_coefficient_1 = 0.35071223,
        return_fan_power_coefficient_2 = 0.30850535,
        return_fan_power_coefficient_3 = -0.54137364,
        return_fan_power_coefficient_4 = 0.87198823,
        return_fan_power_coefficient_5 = 0,
        zone_exhaust_fan_total_efficiency = 0.7,
        zone_exhaust_fan_pressure_rise_pa = 0,
        heating_coil_rated_air_water_convection_ratio = 0.5
    )
    if (length(overrides)) {
        checkmate::assert_names(
            names(overrides),
            subset.of = c(names(defaults), "zone_outdoor_air_flow_m3_s")
        )
    }
    effective <- utils::modifyList(defaults, overrides)
    effective$water_boundary <- TRUE
    effective$heating_coil_type <- "HotWater"
    effective$cooling_coil_type <- "ChilledWater"
    effective$preheat_coil_type <- "None"
    # Design water temperatures size the equivalent coils; operational values
    # always come from source schedules and may differ throughout the year.
    effective$chilled_water_design_setpoint_c <- 7
    effective$hot_water_design_setpoint_c <- 60
    effective$cooling_coil_design_setpoint_c <- 13
    effective$heating_coil_design_setpoint_c <- 35
    effective
}

# Generate only water-loop scaffolding, with temporary district objects that
# ExpandObjects can wire without fabricating a chiller, boiler or tower.
hvac__add_boundary_plants <- function(model, options) {
    model$add(
        `HVACTemplate:Plant:ChilledWaterLoop` = list(
            name = "DeST Chilled Water Loop",
            pump_control_type = "Intermittent",
            chilled_water_design_setpoint = options$chilled_water_design_setpoint_c,
            chilled_water_pump_configuration = "ConstantPrimaryNoSecondary",
            primary_chilled_water_pump_rated_head = 0,
            chilled_water_setpoint_reset_type = "None"
        ),
        `HVACTemplate:Plant:Chiller` = list(
            name = "DeST Water Cooling Boundary",
            chiller_type = "DistrictChilledWater",
            capacity = "autosize",
            # Required by the template IDD but explicitly ignored for district
            # cooling. This temporary field never enters the emitted model.
            nominal_cop = 1
        ),
        `HVACTemplate:Plant:HotWaterLoop` = list(
            name = "DeST Hot Water Loop",
            pump_control_type = "Intermittent",
            hot_water_design_setpoint = options$hot_water_design_setpoint_c,
            hot_water_pump_configuration = "ConstantFlow",
            hot_water_pump_rated_head = 0,
            hot_water_setpoint_reset_type = "None"
        ),
        `HVACTemplate:Plant:Boiler` = list(
            name = "DeST Water Heating Boundary",
            boiler_type = "DistrictHotWater",
            capacity = "autosize"
        )
    )
    invisible(model)
}

# Preserve boundary provenance and target assumptions when a saved IDF loses
# its R audit attributes. Water-source heat is thermal boundary exchange, not
# purchased heat or equipment electricity.
hvac__water_comments <- function(water, effective) {
    if (is.null(water) || !nrow(water)) {
        return(character())
    }
    c(
        "destep water coil: equivalent native cooling/heating stages; target autosizing is approximate, not DeST coil performance",
        "destep water plant: scheduled temperature boundary; no inferred chiller/boiler/tower or associated electricity/fuel",
        sprintf(
            "destep AHU %s %s water: source SCHEDULE_YEAR ID=%s; range=%g..%g C",
            water$ahu_id,
            water$role,
            water$schedule_id,
            water$minimum_c,
            water$maximum_c
        ),
        unlist(
            lapply(effective, function(item) {
                fields <- names(item$options)
                selected <- which(vapply(
                    item$options,
                    function(value) is.atomic(value) && length(value) == 1L,
                    logical(1L)
                ))
                sprintf(
                    "destep AC_SYS %s %s=%s; origin=%s",
                    item$system_id,
                    fields[selected],
                    vapply(item$options[selected], as.character, character(1L)),
                    item$origins[selected]
                )
            }),
            use.names = FALSE
        )
    )
}

# Replace temporary district objects while retaining their branch nodes and
# equipment-list ownership. Native scheduled source temperature is independent
# of loop demand; no district-energy meters or fictitious COP remain.
hvac__replace_boundary_plants <- function(model, water) {
    for (index in seq_len(nrow(water))) {
        cold <- water$role[[index]] == "cooling"
        class <- if (cold) "DistrictCooling" else "DistrictHeating"
        objects <- data.table::as.data.table(model$to_table(
            class = class,
            wide = TRUE
        ))
        checkmate::assert_data_table(objects, nrows = 1L)
        name <- objects$Name[[1L]]
        inlet <- objects$`Chilled Water Inlet Node Name`[[1L]]
        outlet <- objects$`Chilled Water Outlet Node Name`[[1L]]
        if (!cold) {
            inlet <- objects$`Hot Water Inlet Node Name`[[1L]]
            outlet <- objects$`Hot Water Outlet Node Name`[[1L]]
        }
        suppressMessages(model$del(objects$id, .force = TRUE))
        model$add(
            `PlantComponent:TemperatureSource` = list(
                name = name,
                inlet_node = inlet,
                outlet_node = outlet,
                design_volume_flow_rate = "autosize",
                temperature_specification_type = "Scheduled",
                source_temperature_schedule_name = water$schedule_name[[index]]
            )
        )
        # Update every typed equipment reference, not just the visible branch.
        references <- model$to_table(class = c("Branch", "PlantEquipmentList"))
        # Evaluate outside data.table's column scope: its `class` column must
        # not shadow the local equipment class being replaced.
        positions <- which(!is.na(references$value) & references$value == class)
        refs <- references[positions]
        for (row in seq_len(nrow(refs))) {
            do.call(
                model$object(refs$id[[row]])$set,
                stats::setNames(
                    list("PlantComponent:TemperatureSource"),
                    refs$field[[row]]
                )
            )
        }
        loop_name <- if (cold) {
            "DeST Chilled Water Loop Chilled Water Loop"
        } else {
            "DeST Hot Water Loop Hot Water Loop"
        }
        loop <- model$object(loop_name)
        # Two-pipe schedules can deliver cold water to either equivalent
        # stage. The template's 10 C heating-loop floor must not clip them.
        loop$set(
            minimum_loop_temperature = 0.1,
            maximum_loop_temperature = 99.9
        )
        manager_name <- if (cold) {
            "DeST Chilled Water Loop ChW Temp Manager"
        } else {
            "DeST Hot Water Loop HW Temp Manager"
        }
        model$object(manager_name)$set(
            schedule_name = water$schedule_name[[index]]
        )
    }
    invisible(model)
}
