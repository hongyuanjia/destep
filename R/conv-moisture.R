# Bind an already normalized prescribed kg/h source to EnergyPlus latent gains.
# Owner modules select source fields, validate inputs and convert units. This
# helper only emits target objects; it does not infer people/equipment inputs.
# EnergyPlus divides latent watts by Hg(T) in its zone moisture balance.
# The EMS calling point uses the latest available temperature; rapid changes
# can still leave a one-zone-step residual, rather than fixed-enthalpy bias.
# People passes nominal occupant moisture; equipment passes its mass source.
# Neither source is adjusted to reproduce DeST's humidity cap or human model.
moisture__objects <- function(dest, ep, equipment, prefix_base, label) {
    max_hum <- equipment$MAX_HUM
    min_hum <- equipment$MIN_HUM
    max_hum[is.na(max_hum)] <- 0
    min_hum[is.na(min_hum)] <- 0
    rows <- which(max_hum > 0)
    if (!length(rows)) {
        return(NULL)
    }

    # The target must offer the pre-initialization calling point introduced in
    # 9.1. Check its actual IDD choices rather than inferring them from a number.
    calling_points <- unlist(
        ep$definition(
            "EnergyManagementSystem:ProgramCallingManager"
        )$field_choice("energyplus_model_calling_point"),
        use.names = FALSE
    )
    if (!"BeginZoneTimestepBeforeInitHeatBalance" %in% calling_points) {
        stop(
            paste(
                sprintf(
                    "Nonzero %s moisture requires EnergyPlus 9.1.0 or newer",
                    tolower(label)
                ),
                "for BeginZoneTimestepBeforeInitHeatBalance.",
                "Select a supported target version; moisture is never silently discarded."
            ),
            call. = FALSE
        )
    }

    classes <- c(
        "OtherEquipment",
        "EnergyManagementSystem:Sensor",
        "EnergyManagementSystem:InternalVariable",
        "EnergyManagementSystem:Actuator",
        "EnergyManagementSystem:Program",
        "EnergyManagementSystem:ProgramCallingManager"
    )
    zone_field <- conv__idd_field_name(ep, "OtherEquipment", 3L)
    area_field <- conv__idd_field_name(ep, "OtherEquipment", 7L)
    # The program has three lines, or four for a per-area source. Resolve the
    # shared extensible field labels once, rather than once for every room.
    program_fields <- vapply(
        seq_len(4L) + 1L,
        function(field) {
            conv__idd_field_name(ep, "EnergyManagementSystem:Program", field)
        },
        character(1L)
    )
    # Each room returns a fixed-size bundle of heterogeneous IDF objects.
    # Collect bundles once to avoid repeatedly copying growing class lists.
    values <- lapply(rows, function(i) {
        prefix <- paste0(prefix_base, i)
        name <- paste(equipment$NAME[[i]], "Moisture")
        per_area <- equipment$CALCULATION_BASIS[[i]] == 1L
        source <- list(
            name = name,
            fuel_type = "None",
            schedule_name = equipment$SCHEDULE_NAME[[i]],
            design_level_calculation_method = equipment$METHOD[[i]],
            fraction_latent = 1,
            fraction_radiant = 0,
            fraction_lost = 0,
            end_use_subcategory = paste("DeST", label, "Moisture")
        )
        source[[zone_field]] <- equipment$ROOM_NAME[[i]]
        # Nominal watts document the source size; EMS supplies actual power,
        # including nonzero minimum moisture when the equipment schedule is zero.
        source[[if (per_area) area_field else "design_level"]] <-
            max_hum[[i]] * 2500000 / 3600
        sensors <- list(
            list(
                name = paste0(prefix, "_Temperature"),
                output_variable_or_output_meter_index_key_name = equipment$ROOM_NAME[[
                    i
                ]],
                output_variable_or_output_meter_name = "Zone Air Temperature"
            ),
            list(
                name = paste0(prefix, "_Schedule"),
                output_variable_or_output_meter_index_key_name = equipment$SCHEDULE_NAME[[
                    i
                ]],
                output_variable_or_output_meter_name = "Schedule Value"
            )
        )
        area <- if (per_area) {
            list(list(
                name = paste0(prefix, "_Area"),
                internal_data_index_key_name = equipment$ROOM_NAME[[i]],
                internal_data_type = "Zone Floor Area"
            ))
        } else {
            list()
        }
        actuator <- list(list(
            name = paste0(prefix, "_Power"),
            actuated_component_unique_name = name,
            actuated_component_type = "OtherEquipment",
            actuated_component_control_type = "Power Level"
        ))
        # Use a local program variable for kg/s; no zone multiplier is applied
        # here because EnergyPlus applies it in the zone demand calculation.
        lines <- c(
            sprintf(
                "SET MassRate = %.17g + %.17g * %s_Schedule",
                min_hum[[i]] / 3600,
                (max_hum[[i]] - min_hum[[i]]) / 3600,
                prefix
            ),
            if (per_area) sprintf("SET MassRate = MassRate * %s_Area", prefix),
            sprintf(
                "SET VaporEnthalpy = @HgAirFnWTdb 0 %s_Temperature",
                prefix
            ),
            sprintf("SET %s_Power = MassRate * VaporEnthalpy", prefix)
        )
        program <- list(c(
            list(name = paste0(prefix, "_Control")),
            stats::setNames(as.list(lines), program_fields[seq_along(lines)])
        ))
        # Internal gains are evaluated during heat-balance initialization.
        # The latest available zone temperature removes fixed-enthalpy bias;
        # abrupt temperature changes can still cause a one-zone-step residual.
        manager <- list(list(
            name = paste0(prefix, "_Manager"),
            energyplus_model_calling_point = "BeginZoneTimestepBeforeInitHeatBalance",
            program_name_1 = paste0(prefix, "_Control")
        ))
        stats::setNames(
            list(list(source), sensors, area, actuator, program, manager),
            classes
        )
    })
    # Preserve class order and source-row order while flattening each class
    # only once. Keep an empty list for optional classes with no objects.
    conv__combine_outputs(lapply(classes, function(class) {
        objects <- as.list(unlist(
            lapply(values, `[[`, class),
            recursive = FALSE
        ))
        conv__add_objects(dest, ep, class, objects)
    }))
}
