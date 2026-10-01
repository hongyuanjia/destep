# ROOM.TYPE -> ROOM_TYPE_DATA equipment fields -> sensible and moisture sources.
equipment__convert <- function(dest, ep) {
    if (!internal_gains__has_room_type_data(dest)) {
        return(NULL)
    }

    equipment <- DBI::dbGetQuery(
        dest,
        "
        SELECT
            R.ID             AS ID,
            R.NAME || ' Equipment' AS NAME,
            R.ID             AS ROOM_ID,
            R.NAME           AS ROOM_NAME,
            R.TYPE           AS ROOM_TYPE_ID,
            T.ID             AS ROOM_TYPE_DATA_ID,
            T.E_SCHEDULE     AS SCHEDULE_ID,
            S.NAME           AS SCHEDULE_NAME,
            T.E_DIST_MODE    AS DIST_MODE_ID,
            T.E_PER_AREA     AS CALCULATION_BASIS,
            CASE WHEN T.E_MAXPOWER > 0 OR T.E_MINPOWER > 0 OR
                T.E_MAX_HUM != 0 OR T.E_MIN_HUM != 0
                THEN 1 ELSE 0 END AS ACTIVE,
            CASE
                WHEN T.E_PER_AREA = 1 THEN 'Watts/Area'
                ELSE 'EquipmentLevel'
            END              AS METHOD,
            CASE
                WHEN T.E_PER_AREA != 1 THEN T.E_MAXPOWER
                ELSE NULL
            END              AS DESIGN_LEVEL,
            CASE
                WHEN T.E_PER_AREA = 1 THEN T.E_MAXPOWER
                ELSE NULL
            END              AS WATTS_PER_AREA,
            CASE
                WHEN T.E_PER_AREA != 1 THEN T.E_MINPOWER
                ELSE NULL
            END              AS MIN_DESIGN_LEVEL,
            CASE
                WHEN T.E_PER_AREA = 1 THEN T.E_MINPOWER
                ELSE NULL
            END              AS MIN_WATTS_PER_AREA,
            T.E_MAX_HUM      AS MAX_HUM,
            T.E_MIN_HUM      AS MIN_HUM,
            ROUND(1.0 - DM.DIST_AIR, 3)
                             AS FRACTION_RADIANT
        FROM ROOM R
        LEFT JOIN ROOM_TYPE_DATA T
        ON R.TYPE = T.ID
        LEFT JOIN SCHEDULE_YEAR S
        ON T.E_SCHEDULE = S.SCHEDULE_ID
        LEFT JOIN DIST_MODE DM
        ON T.E_DIST_MODE = DM.DIST_MODE_ID
        ORDER BY R.ID
        "
    )
    data.table::setDT(equipment)
    internal_gains__assert_references(equipment, "equipment")
    equipment <- equipment[ACTIVE != 0L]
    if (nrow(equipment) == 0L) {
        return(NULL)
    }
    dt_force_numeric(
        equipment,
        c(
            "DESIGN_LEVEL",
            "WATTS_PER_AREA",
            "MIN_DESIGN_LEVEL",
            "MIN_WATTS_PER_AREA",
            "MAX_HUM",
            "MIN_HUM",
            "FRACTION_RADIANT"
        )
    )
    equipment__assert_moisture(equipment)
    watts_per_area_field <- conv__idd_field_name(ep, "ElectricEquipment", 6L)
    has_min <- any(
        internal_gains__has_positive_minimum(equipment$MIN_DESIGN_LEVEL) |
            internal_gains__has_positive_minimum(equipment$MIN_WATTS_PER_AREA)
    )

    # DeST equipment gains also store MINPOWER/MAXPOWER. Use the same
    # minimum-plus-variable representation as people and lights.
    always_on <- "Always On - DeST Minimum Equipment"
    zone_field_name <- internal_gains__zone_field_name(
        ep,
        "ElectricEquipment"
    )
    equipment_objects <- unlist(
        lapply(seq_len(nrow(equipment)), function(i) {
            equipment__values(
                equipment,
                i,
                watts_per_area_field,
                always_on,
                zone_field_name
            )
        }),
        recursive = FALSE
    )

    parts <- list()
    if (has_min) {
        parts$minimum_schedule <- internal_gains__always_on(
            dest,
            ep,
            always_on
        )
    }

    parts$equipment <- conv__add_objects(
        dest,
        ep,
        "ElectricEquipment",
        equipment_objects
    )
    parts$moisture <- equipment__moisture_objects(dest, ep, equipment)

    out <- conv__combine_outputs(parts, table = equipment)
    attr(out, "sources") <- equipment__source_specs(
        dest,
        equipment,
        equipment_objects,
        "design_level",
        watts_per_area_field
    )
    out
}

# Moisture generation must define a finite, nonnegative minimum/maximum pair.
# Access NULL retains the established zero-source convention.
equipment__assert_moisture <- function(equipment) {
    max_hum <- equipment$MAX_HUM
    min_hum <- equipment$MIN_HUM
    max_hum[is.na(max_hum) & !is.nan(max_hum)] <- 0
    min_hum[is.na(min_hum) & !is.nan(min_hum)] <- 0
    invalid <- !is.finite(max_hum) |
        !is.finite(min_hum) |
        max_hum < 0 |
        min_hum < 0 |
        min_hum > max_hum
    if (!any(invalid)) {
        return(invisible(NULL))
    }

    rows <- equipment[invalid]
    detail <- paste(
        sprintf(
            "%s: MIN_HUM=%s, MAX_HUM=%s",
            rows$NAME,
            rows$MIN_HUM,
            rows$MAX_HUM
        ),
        collapse = "; "
    )
    stop(
        sprintf(
            "Invalid ROOM_TYPE_DATA equipment moisture generation (require finite 0 <= MIN_HUM <= MAX_HUM): %s",
            detail
        ),
        call. = FALSE
    )
}

# Keep the DeST kg/h source independent of sensible gains and energy meters.
# Native DeST applies MIN_HUM + (MAX_HUM - MIN_HUM) * E_SCHEDULE in kg/h,
# multiplied by zone floor area only when E_PER_AREA = 1.
# EnergyPlus divides internal latent watts by PsyHgAirFnWTdb in its moisture
# balance, so use that same built-in function instead of a fixed 2500 kJ/kg.
equipment__moisture_objects <- function(dest, ep, equipment) {
    max_hum <- equipment$MAX_HUM
    min_hum <- equipment$MIN_HUM
    max_hum[is.na(max_hum)] <- 0
    min_hum[is.na(min_hum)] <- 0
    rows <- which(max_hum > 0)
    if (!length(rows)) {
        return(NULL)
    }

    # Earlier calling points cannot update moisture before current-step gains
    # are evaluated. Reject them instead of shifting the source schedule.
    if (
        numeric_version(as.character(ep$version())) < numeric_version("9.1.0")
    ) {
        stop(
            paste(
                "Nonzero equipment moisture requires EnergyPlus 9.1.0 or newer",
                "for BeginZoneTimestepBeforeInitHeatBalance.",
                "Use to_eplus(..., ver = '9.6.0', options = destep_opts(hvac = 'ideal_loads'))",
                "for the validated real-model path."
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
        prefix <- paste0("DeST_Moisture_", i)
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
            end_use_subcategory = "DeST Equipment Moisture"
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

# Split equipment power while retaining total or per-area source units.
equipment__values <- function(
    equipment,
    i,
    watts_per_area_field,
    always_on,
    zone_field_name = "Zone or ZoneList or Space or SpaceList Name"
) {
    if (equipment$METHOD[[i]] == "EquipmentLevel") {
        max_value <- equipment$DESIGN_LEVEL[[i]]
        min_value <- equipment$MIN_DESIGN_LEVEL[[i]]
        field <- "design_level"
    } else {
        max_value <- equipment$WATTS_PER_AREA[[i]]
        min_value <- equipment$MIN_WATTS_PER_AREA[[i]]
        field <- watts_per_area_field
    }

    value_factory <- function(gain, row, name, schedule) {
        equipment__value(
            gain,
            row,
            name,
            schedule,
            zone_field_name
        )
    }
    internal_gains__split_minimum(
        equipment,
        i,
        max_value,
        min_value,
        field,
        always_on,
        value_factory
    )
}

# Describe one sensible electric equipment object; moisture is separate.
equipment__value <- function(
    equipment,
    i,
    name,
    schedule,
    zone_field_name
) {
    value <- list(
        name = name,
        schedule_name = schedule,
        design_level_calculation_method = equipment$METHOD[[i]],
        design_level = NULL,
        watts_per_person = NULL,
        fraction_latent = 0,
        fraction_radiant = equipment$FRACTION_RADIANT[[i]],
        fraction_lost = 0,
        end_use_subcategory = "General"
    )
    value[[zone_field_name]] <- equipment$ROOM_NAME[[i]]
    value
}

# Keep equipment moisture restrictions with the owning equipment converter.
equipment__source_specs <- function(
    dest,
    equipment,
    values,
    total_field,
    area_field
) {
    # The existing prescribed projector supports sensible equipment only;
    # moisture still follows this module's independent mass-source conversion.
    decorate <- function(item, row) {
        if (
            any(
                c(equipment$MAX_HUM[[row]], equipment$MIN_HUM[[row]]) != 0,
                na.rm = TRUE
            )
        ) {
            item$unsupported_reason <- "Equipment moisture is not supported by the prescribed source projector."
        }
        item
    }
    internal_gains__source_specs(
        dest,
        equipment,
        values,
        "equipment",
        total_field,
        area_field,
        "schedule_name",
        decorate
    )
}
