# Read a referenced annual RH schedule in DeST's fractional units. Missing
# controls must not be mistaken for a zero humidification requirement.
hvac__rh_schedule <- function(dest, id) {
    checkmate::assert_integerish(id, len = 1L, lower = 1L, any.missing = FALSE)
    row <- DBI::dbGetQuery(
        dest,
        "SELECT NAME, DATA FROM SCHEDULE_YEAR WHERE SCHEDULE_ID=?",
        params = list(id)
    )
    checkmate::assert_data_frame(row, nrows = 1L)
    values <- schedule__decode(row$DATA[[1L]], row$NAME[[1L]])
    checkmate::assert_numeric(
        values,
        len = 8760L,
        lower = 0,
        upper = 1,
        finite = TRUE,
        any.missing = FALSE
    )
    values
}

# Validate paired bounds at matching source hours, decoding each referenced
# schedule only once even when several rooms share it. Return extrema for
# capability checks; extrema alone cannot detect crossing hourly bounds.
hvac__rh_bounds <- function(dest, minimum_ids, maximum_ids, labels) {
    checkmate::assert_integerish(minimum_ids, lower = 1L, any.missing = FALSE)
    checkmate::assert_integerish(
        maximum_ids,
        len = length(minimum_ids),
        lower = 1L,
        any.missing = FALSE
    )
    checkmate::assert_character(
        labels,
        len = length(minimum_ids),
        any.missing = FALSE
    )
    ids <- unique(c(minimum_ids, maximum_ids))
    schedules <- lapply(ids, function(id) hvac__rh_schedule(dest, id))
    lower_index <- match(minimum_ids, ids)
    upper_index <- match(maximum_ids, ids)
    # Each pair owns a separate diagnostic; vectors compare all 8760 hours.
    extrema <- vapply(
        seq_along(minimum_ids),
        function(i) {
            lower <- schedules[[lower_index[[i]]]]
            upper <- schedules[[upper_index[[i]]]]
            crossing <- which(lower > upper)
            if (length(crossing)) {
                hour <- crossing[[1L]]
                abort(
                    sprintf(
                        "%s minimum RH exceeds maximum RH at source hour %s (schedules %s/%s: %.8g > %.8g).",
                        labels[[i]],
                        hour,
                        minimum_ids[[i]],
                        maximum_ids[[i]],
                        lower[[hour]],
                        upper[[hour]]
                    ),
                    class = "destep_invalid_hvac_humidity_control"
                )
            }
            c(maximum_lower = max(lower), minimum_upper = min(upper))
        },
        numeric(2L)
    )
    list(maximum_lower = extrema[1L, ], minimum_upper = extrema[2L, ])
}

# Resolve a named AC_SYS extension through its reachable linked list. Catalogue
# rows and properties owned by other systems cannot provide control values.
hvac__system_property <- function(dest, system_id, name) {
    root <- DBI::dbGetQuery(
        dest,
        "SELECT EXT_PROPERTY FROM AC_SYS WHERE AC_SYS_ID=?",
        params = list(system_id)
    )
    checkmate::assert_data_frame(root, nrows = 1L)
    pointer <- root$EXT_PROPERTY[[1L]]
    properties <- DBI::dbGetQuery(
        dest,
        "SELECT PROPERTY_ID, NEXT_PROPERTY, NAME, DATA_LONG FROM EXT_PROPERTY"
    )
    visited <- rep(FALSE, nrow(properties))
    selected <- integer(nrow(properties))
    count <- 0L
    # Traversal follows pointers, so it is sequential and bounded by row count.
    while (!is.na(pointer) && pointer != 0L) {
        index <- which(properties$PROPERTY_ID == pointer)
        if (length(index) != 1L || visited[[index]]) {
            abort(
                "Invalid AC_SYS extended-property chain.",
                class = "destep_unresolved_hvac_property"
            )
        }
        visited[[index]] <- TRUE
        count <- count + 1L
        selected[[count]] <- index
        pointer <- properties$NEXT_PROPERTY[[index]]
    }
    if (is.na(pointer)) {
        abort(
            "Missing AC_SYS extended-property terminator.",
            class = "destep_unresolved_hvac_property"
        )
    }
    rows <- properties[selected[seq_len(count)], , drop = FALSE]
    value <- rows$DATA_LONG[rows$NAME == name]
    if (length(value) != 1L || is.na(value)) {
        abort(
            paste("Cannot resolve AC_SYS", system_id, name),
            class = "destep_unresolved_hvac_property"
        )
    }
    as.integer(value)
}

# Extended properties store schedule IDs in DATA_LONG, outside the generic
# schedule-column scan. Resolve only the selected systems' reachable bounds.
hvac__supply_rh_schedule_ids <- function(dest, system_ids) {
    unique(as.integer(unlist(
        lapply(system_ids, function(id) {
            vapply(
                c("AC_SYS_MINF_SCH", "AC_SYS_MAXF_SCH"),
                function(name) {
                    hvac__system_property(dest, id, name)
                },
                integer(1L)
            )
        }),
        use.names = FALSE
    )))
}

# A schedule shared with room RH can already be scaled to percent. Keep that
# conversion's unit decision rather than scaling the same source twice.
hvac__supply_rh_reference <- function(dest, id) {
    row <- DBI::dbGetQuery(
        dest,
        "SELECT NAME FROM SCHEDULE_YEAR WHERE SCHEDULE_ID=?",
        params = list(id)
    )
    checkmate::assert_data_frame(row, nrows = 1L)
    percent_ids <- setdiff(
        schedule__relative_humidity_ids(dest),
        schedule__shared_relative_humidity_ids(dest)
    )
    list(name = row$NAME[[1L]], divisor = if (id %in% percent_ids) 100 else 1)
}

# Read humidity constraints independently of humidifier presence. A missing
# humidifier does not remove cooling-coil or supply-air humidity requirements;
# retain absent equipment without silently dropping the source controls.
hvac__air_treatment_source <- function(dest, system_id) {
    handler <- DBI::dbGetQuery(
        dest,
        "SELECT AHU_ID, AHURES, HUMIDIFIER, HEAT_RECOVER, MIN_T_EX_COEF, MAX_T_EX_COEF, MIN_D_EX_COEF, MAX_D_EX_COEF FROM AHU WHERE OF_AC_SYS=?",
        params = list(system_id)
    )
    checkmate::assert_data_frame(handler, nrows = 1L)
    type <- handler$HUMIDIFIER[[1L]]
    checkmate::assert_choice(as.character(type), as.character(0:3))
    recovery <- handler$HEAT_RECOVER[[1L]]
    heat_recovery <- hvac__heat_recovery_source(dest, handler)
    result <- list(
        system_id = system_id,
        ahu_id = handler$AHU_ID[[1L]],
        humidifier_code = type,
        heat_recovery_code = recovery,
        heat_recovery = heat_recovery,
        heat_recovery_raw = handler[c(
            "MIN_T_EX_COEF",
            "MAX_T_EX_COEF",
            "MIN_D_EX_COEF",
            "MAX_D_EX_COEF"
        )],
        status = "absent",
        target_type = "None",
        dehumidification = "inactive_unrestricted_upper_rh",
        supply_humidity_control = "unrestricted",
        controls = NULL
    )
    state <- DBI::dbGetQuery(
        dest,
        "SELECT SUPPLY_STATE_FLAG FROM AC_SYS WHERE AC_SYS_ID=?",
        params = list(system_id)
    )$SUPPLY_STATE_FLAG
    if (length(state) != 1L || is.na(state) || state != 0L) {
        abort(
            "User-prescribed supply humidity needs a separate control mapping.",
            class = "destep_unsupported_hvac_humidity_control"
        )
    }
    controls <- hvac__conditioned_controls(dest)
    controls <- controls[controls$OF_AC_SYS == system_id]
    ideal_loads__assert_humidity_schedules(controls)
    supply_ids <- vapply(
        c("AC_SYS_MINF_SCH", "AC_SYS_MAXF_SCH"),
        function(name) hvac__system_property(dest, system_id, name),
        integer(1L),
        USE.NAMES = FALSE
    )
    bounds <- hvac__rh_bounds(
        dest,
        c(controls$SET_RH_MIN_SCHEDULE, supply_ids[[1L]]),
        c(controls$SET_RH_MAX_SCHEDULE, supply_ids[[2L]]),
        c(paste("ROOM", controls$ROOM_ID), paste("AC_SYS", system_id, "supply"))
    )
    room_rows <- seq_len(nrow(controls))
    supply_row <- nrow(controls) + 1L
    lower <- bounds$maximum_lower[room_rows]
    supply_lower <- bounds$maximum_lower[[supply_row]]
    supply_upper <- bounds$minimum_upper[[supply_row]]
    result$maximum_room_minimum_rh <- lower
    result$minimum_room_maximum_rh <- bounds$minimum_upper[room_rows]
    result$supply_minimum_rh_schedule_id <- supply_ids[[1L]]
    result$supply_maximum_rh_schedule_id <- supply_ids[[2L]]
    result$maximum_supply_minimum_rh <- supply_lower
    result$minimum_supply_maximum_rh <- supply_upper
    result$humidity_control_status <- "unrestricted"
    result$controls <- controls
    # Source RH requirements select native target feedback, not a guarantee of
    # satisfaction. DeST's AHU optimization is not reproduced by the converter.
    dehumidifying_rooms <- which(bounds$minimum_upper[room_rows] < 1)
    if (length(dehumidifying_rooms)) {
        result$dehumidification <- "native_cooling_coil"
        result$humidity_control_status <- if (nrow(controls) == 1L) {
            "single_zone_room_maximum"
        } else {
            "multizone_room_maximum"
        }
    }
    if (
        nrow(controls) > 1L &&
            (length(dehumidifying_rooms) > 0L || (type == 2L && any(lower > 0)))
    ) {
        # Native managers otherwise impose fixed moisture limits absent from
        # DeST. These finite target guards are not source physical parameters.
        result$target_humidity_ratio_bounds <- c(minimum = 1e-9, maximum = 1)
    }
    # A supply RH ceiling requires temperature-dependent control. A room-only
    # humidifier setpoint cannot enforce it, even with zero minimum demand.
    if (supply_upper < 1) {
        abort(
            sprintf(
                "AC_SYS %s has a supply-RH upper limit below 100%%; this control is not yet mapped.",
                system_id
            ),
            class = "destep_unsupported_hvac_humidity_control"
        )
    }
    if (type == 0L) {
        if (any(lower > 0) || supply_lower > 0) {
            # Preserve the absent source device. Room moisture gains may meet
            # the lower bound naturally; do not assert a simulated shortfall.
            result$humidity_control_status <- "no_source_humidifier"
            warn(
                sprintf(
                    paste(
                        "AC_SYS %s has positive room/supply RH lower requirements but AHU %s has no humidifier.",
                        "No humidifier is added; those lower limits are not actively controlled."
                    ),
                    system_id,
                    handler$AHU_ID[[1L]]
                ),
                class = "destep_source_humidifier_absent"
            )
        }
        if (supply_lower > 0) {
            result$supply_humidity_control <- "no_source_humidifier"
        }
        return(result)
    }
    if (all(lower == 0) && supply_lower == 0) {
        result$status <- "inactive_zero_lower_rh"
        return(result)
    }
    # Native electric steam generation is not an equivalent energy source for
    # externally supplied steam or spray humidification. Do not guess a fuel.
    if (type != 2L) {
        abort(
            sprintf(
                paste(
                    "AC_SYS %s AHU %s has active HUMIDIFIER=%s.",
                    "External steam/spray humidification is not yet mapped;",
                    "it cannot be disabled or replaced by electric steam."
                ),
                system_id,
                handler$AHU_ID[[1L]],
                type
            ),
            class = "destep_unsupported_hvac_humidifier"
        )
    }
    if (nrow(controls) != 1L && supply_lower > 0) {
        abort(
            sprintf(
                paste(
                    "AC_SYS %s requires multizone supply-RH control;",
                    "only room-RH control is mapped for shared humidifiers."
                ),
                system_id
            ),
            class = "destep_unsupported_hvac_humidity_control"
        )
    }
    result$status <- "native_electric_steam"
    result$humidity_control_status <- if (
        result$dehumidification == "native_cooling_coil"
    ) {
        "single_zone_room_range"
    } else {
        "single_zone_room_minimum"
    }
    result$target_type <- "Humidifier:Steam:Electric"
    if (nrow(controls) > 1L) {
        result$humidity_control_status <- if (
            result$dehumidification == "native_cooling_coil"
        ) {
            "multizone_room_range"
        } else {
            "multizone_room_minimum"
        }
    }
    if (supply_lower > 0) {
        result$supply_humidity_control <- "ems_minimum_rh"
        result$humidity_control_status <- "single_zone_room_and_supply"
        result$supply_rh <- hvac__supply_rh_reference(dest, supply_ids[[1L]])
    }
    result$capacity_origin <- "EnergyPlus autosizing; no source rating"
    result$power_origin <- "EnergyPlus autosizing; 100 percent electric-to-steam efficiency"
    result
}

# Persist source equipment and control coverage in the final IDF header. The
# main converter rebuilds that header after template expansion and transition.
hvac__air_treatment_comments <- function(treatment) {
    vapply(
        treatment,
        function(item) {
            sprintf(
                "destep AC_SYS %s AHU %s: HUMIDIFIER=%s; conversion=%s; humidity_control=%s; dehumidification=%s; supply_humidity_control=%s; HEAT_RECOVER=%s.%s%s",
                item$system_id,
                item$ahu_id,
                item$humidifier_code,
                item$status,
                item$humidity_control_status,
                item$dehumidification,
                item$supply_humidity_control,
                item$heat_recovery_code,
                if (item$dehumidification == "native_cooling_coil") {
                    paste0(
                        " EnergyPlus native humidity override may supersede the temperature target;",
                        " source temperature/RH requirements may remain unmet.",
                        " DeST AHU optimization is not reproduced; no reheat is added for humidity control."
                    )
                } else {
                    ""
                },
                if (item$supply_humidity_control == "ems_minimum_rh") {
                    paste0(
                        " Supply minimum RH uses current supply-fan outlet temperature and barometric pressure;",
                        " EMS converts the lower bound to humidity ratio for the existing humidifier.",
                        " Equipment capacity and conflicting requirements can leave targets unmet."
                    )
                } else if (!is.null(item$target_humidity_ratio_bounds)) {
                    paste0(
                        " Multizone room RH uses native critical-zone humidity control;",
                        " target numerical humidity-ratio guards are 1e-9 to 1 kg/kg, not source limits.",
                        " Demands outside these guards are not represented;",
                        " low-load controller behavior, capacity and saturation can leave room targets unmet."
                    )
                } else {
                    ""
                }
            )
        },
        character(1L)
    )
}

# Convert a source supply RH lower bound at the final supply node into the
# existing humidifier's W setpoint. Fan heat changes RH but leaves W unchanged.
# Cache native room demand before overriding its node to avoid feedback drift.
hvac__refine_supply_humidity <- function(model, treatment, schedule_name) {
    if (treatment$supply_humidity_control != "ems_minimum_rh") {
        return(invisible(model))
    }
    system <- paste0("DeST AC_SYS ", treatment$system_id)
    prefix <- paste0("DeST_Supply_RH_", treatment$system_id)
    humidifier <- model$object(paste(system, "Humidifier"))
    fan <- model$object(paste(system, "Supply Fan"))
    inlet <- unname(unlist(fan$value("air_inlet_node_name")))
    outlet <- unname(unlist(fan$value("air_outlet_node_name")))
    checkmate::assert_true(identical(
        inlet,
        unname(unlist(humidifier$value("air_outlet_node_name")))
    ))
    # This is a change of moist-air coordinates, not a source solver or a
    # capacity override. Refresh during HVAC iteration as steam changes T.
    lines <- c(
        sprintf(
            "SET Fraction = %s_RH / %s",
            prefix,
            treatment$supply_rh$divisor
        ),
        sprintf("SET %s_Minimum = %s_RoomTarget", prefix, prefix),
        "IF Fraction > 0",
        sprintf(
            "SET RequiredW = @WFnTdbRhPb %s_T Fraction %s_P",
            prefix,
            prefix
        ),
        sprintf(
            "SET %s_Minimum = @MAX %s_RoomTarget RequiredW",
            prefix,
            prefix
        ),
        "ENDIF"
    )
    fields <- vapply(
        seq_along(lines) + 1L,
        function(i) {
            conv__idd_field_name(model, "EnergyManagementSystem:Program", i)
        },
        character(1L)
    )
    model$add(
        `EnergyManagementSystem:Sensor` = list(
            name = paste0(prefix, "_T"),
            output_variable_or_output_meter_index_key_name = outlet,
            output_variable_or_output_meter_name = "System Node Temperature"
        ),
        `EnergyManagementSystem:Sensor` = list(
            name = paste0(prefix, "_P"),
            output_variable_or_output_meter_index_key_name = "Environment",
            output_variable_or_output_meter_name = "Site Outdoor Air Barometric Pressure"
        ),
        `EnergyManagementSystem:Sensor` = list(
            name = paste0(prefix, "_RH"),
            output_variable_or_output_meter_index_key_name = schedule_name,
            output_variable_or_output_meter_name = "Schedule Value"
        ),
        `EnergyManagementSystem:Sensor` = list(
            name = paste0(prefix, "_RoomMinimum"),
            output_variable_or_output_meter_index_key_name = inlet,
            output_variable_or_output_meter_name = "System Node Setpoint Minimum Humidity Ratio"
        ),
        `EnergyManagementSystem:GlobalVariable` = list(
            erl_variable_1_name = paste0(prefix, "_RoomTarget")
        ),
        `EnergyManagementSystem:Actuator` = list(
            name = paste0(prefix, "_Minimum"),
            actuated_component_unique_name = inlet,
            actuated_component_type = "System Node Setpoint",
            actuated_component_control_type = "Humidity Ratio Minimum Setpoint"
        ),
        `EnergyManagementSystem:Program` = list(
            name = paste0(prefix, "_Cache"),
            program_line_1 = sprintf(
                "SET %s_RoomTarget = %s_RoomMinimum",
                prefix,
                prefix
            )
        ),
        `EnergyManagementSystem:Program` = c(
            list(name = paste0(prefix, "_Convert")),
            stats::setNames(as.list(lines), fields)
        ),
        `EnergyManagementSystem:ProgramCallingManager` = list(
            name = paste0(prefix, "_AfterManagers"),
            energyplus_model_calling_point = "AfterPredictorAfterHVACManagers",
            program_name_1 = paste0(prefix, "_Cache"),
            program_name_2 = paste0(prefix, "_Convert")
        ),
        `EnergyManagementSystem:ProgramCallingManager` = list(
            name = paste0(prefix, "_Iterations"),
            energyplus_model_calling_point = "InsideHVACSystemIterationLoop",
            program_name_1 = paste0(prefix, "_Convert")
        ),
        .default = FALSE
    )
    invisible(model)
}

# Map the room's paired RH schedules once for both native moisture controls.
# Cooling remains on the existing source-mapped coil and water availability.
hvac__refine_room_humidity <- function(model, treatment, schedules, zone_name) {
    cooling <- treatment$dehumidification == "native_cooling_coil"
    if (treatment$target_type == "None" && !cooling) {
        return(invisible(model))
    }
    system_name <- paste0("DeST AC_SYS ", treatment$system_id)
    # Schedule names retain the source-control row order: all lower bounds,
    # then all upper bounds. Do not let the air-loop zone ordering remap them.
    count <- length(zone_name)
    checkmate::assert_character(zone_name, any.missing = FALSE, unique = TRUE)
    checkmate::assert_character(schedules, len = 2L * count)
    humidistats <- lapply(seq_len(count), function(i) {
        list(
            name = if (count == 1L) {
                paste(system_name, "Humidistat")
            } else {
                paste(system_name, zone_name[[i]], "Humidistat")
            },
            zone_name = zone_name[[i]],
            humidifying_relative_humidity_setpoint_schedule_name = schedules[[
                i
            ]],
            dehumidifying_relative_humidity_setpoint_schedule_name = schedules[[
                count + i
            ]]
        )
    })
    do.call(
        model$add,
        c(
            stats::setNames(humidistats, rep("ZoneControl:Humidistat", count)),
            list(.default = FALSE)
        )
    )
    if (!cooling) {
        return(invisible(model))
    }
    connections <- model$to_table(
        class = "ZoneHVAC:EquipmentConnections",
        wide = TRUE
    )
    selected <- connections[connections[["Zone Name"]] %in% zone_name]
    checkmate::assert_data_table(selected, nrows = count)
    coil <- model$object(paste(system_name, "Cooling Coil"))
    controller <- model$object(paste(system_name, "Cooling Coil Controller"))
    outlet <- unname(unlist(coil$value("air_outlet_node_name")))
    # Verify the typed sensor and water actuator before changing control mode;
    # matching names alone cannot establish that this is the selected coil.
    checkmate::assert_true(identical(
        unname(unlist(controller$value("sensor_node_name"))),
        outlet
    ))
    checkmate::assert_true(identical(
        unname(unlist(controller$value("actuator_node_name"))),
        unname(unlist(coil$value("water_inlet_node_name")))
    ))
    manager <- list(
        name = paste(system_name, "Dehumidification Setpoint Manager"),
        setpoint_node_or_nodelist_name = outlet
    )
    if (count == 1L) {
        class <- "SetpointManager:SingleZone:Humidity:Maximum"
        manager$control_zone_air_node_name <- selected[["Zone Air Node Name"]][[
            1L
        ]]
    } else {
        # The native air-loop manager evaluates every served humidistat and
        # selects the most restrictive room demand for this shared coil.
        class <- "SetpointManager:MultiZone:Humidity:Maximum"
        manager$hvac_air_loop_name <- system_name
        manager$minimum_setpoint_humidity_ratio <- treatment$target_humidity_ratio_bounds[[
            "minimum"
        ]]
        manager$maximum_setpoint_humidity_ratio <- treatment$target_humidity_ratio_bounds[[
            "maximum"
        ]]
    }
    do.call(
        model$add,
        c(stats::setNames(list(manager), class), list(.default = FALSE))
    )
    controller$set(control_variable = "TemperatureAndHumidityRatio")
    controller$comment(
        c(
            "Room RH maximum: native EnergyPlus humidity override on the existing cooling coil.",
            "Source temperature targets remain, but may be superseded by humidity control.",
            "Unmet targets are possible; DeST AHU optimization is not reproduced."
        ),
        append = TRUE
    )
    invisible(model)
}

# Insert a native electric steam humidifier before the final supply fan. Keep
# the loop outlet node stable so existing temperature controls and zone links
# remain valid. The supported draw-through path reports humidifier conditions
# before the fan temperature rise in DeST's RESULT_AC_SYS.
hvac__refine_humidifier <- function(model, treatment, zone_name) {
    if (treatment$target_type == "None") {
        return(invisible(model))
    }
    system_name <- paste0("DeST AC_SYS ", treatment$system_id)
    name <- paste(system_name, "Humidifier")
    fan <- model$object(paste(system_name, "Supply Fan"))
    inlet <- unname(unlist(fan$value("air_inlet_node_name")))
    outlet <- paste(system_name, "Humidifier Outlet")
    loop_outlet <- unname(unlist(fan$value("air_outlet_node_name")))
    branch <- model$object(paste(system_name, "Main Branch"))
    # The supported CAV/VAV graphs place the supply fan in branch position five.
    # Assert ownership before changing nodes; do not rely on a loose name match.
    checkmate::assert_true(identical(
        unname(unlist(branch$value("component_5_name"))),
        fan$name()
    ))
    connections <- model$to_table(
        class = "ZoneHVAC:EquipmentConnections",
        wide = TRUE
    )
    selected <- connections[connections[["Zone Name"]] %in% zone_name]
    checkmate::assert_data_table(selected, nrows = length(zone_name))
    availability <- unname(unlist(fan$value("availability_schedule_name")))
    model$add(
        `Humidifier:Steam:Electric` = list(
            name = name,
            availability_schedule_name = availability,
            rated_capacity = "Autosize",
            rated_power = "Autosize",
            rated_fan_power = 0,
            standby_power = 0,
            air_inlet_node_name = inlet,
            air_outlet_node_name = outlet
        ),
        .default = FALSE
    )
    # Native critical-zone selection preserves every room's source demand;
    # numerical bounds are audited separately from equipment saturation/capacity.
    manager <- list(
        name = paste(system_name, "Humidification Setpoint Manager")
    )
    if (length(zone_name) == 1L) {
        class <- "SetpointManager:SingleZone:Humidity:Minimum"
        manager$setpoint_node_or_nodelist_name <- outlet
        manager$control_zone_air_node_name <- selected[["Zone Air Node Name"]][[
            1L
        ]]
    } else {
        class <- "SetpointManager:MultiZone:Humidity:Minimum"
        manager$hvac_air_loop_name <- system_name
        manager$minimum_setpoint_humidity_ratio <- treatment$target_humidity_ratio_bounds[[
            "minimum"
        ]]
        manager$maximum_setpoint_humidity_ratio <- treatment$target_humidity_ratio_bounds[[
            "maximum"
        ]]
        manager$setpoint_node_or_nodelist_name <- outlet
    }
    do.call(
        model$add,
        c(stats::setNames(list(manager), class), list(.default = FALSE))
    )
    # The humidity manager controls the humidifier outlet; downstream fan heat
    # changes dry-bulb temperature but does not add or remove water vapour.
    fan$set(air_inlet_node_name = outlet)
    branch$set(
        component_5_object_type = "Humidifier:Steam:Electric",
        component_5_name = name,
        component_5_inlet_node_name = inlet,
        component_5_outlet_node_name = outlet,
        component_6_object_type = fan$class_name(),
        component_6_name = fan$name(),
        component_6_inlet_node_name = outlet,
        component_6_outlet_node_name = loop_outlet
    )
    model$object(name)$comment(
        c(
            "DeST AHU.HUMIDIFIER=2 (electric).",
            "Capacity/power: EnergyPlus autosizing; source has no equipment rating.",
            "No separately specified blower or standby power: assumed zero.",
            "Room RH schedules come from effective ROOM_TYPE_DATA."
        ),
        append = TRUE
    )
    invisible(model)
}
