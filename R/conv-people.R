# Collect source room-type gains under the explicitly selected people mode.
internal_gains__convert <- function(dest, ep, people_heat = "constant") {
    conv <- Filter(
        Negate(is.null),
        list(
            PEOPLE = internal_gains__convert_people(dest, ep, people_heat),
            LIGHTS = internal_gains__convert_lights(dest, ep),
            EQUIPMENT = internal_gains__convert_electric_equipment(dest, ep)
        )
    )

    out <- conv__combine_outputs(conv)
    if (is.null(out)) {
        return(NULL)
    }

    # All three object families are projections of the same room-type record.
    data.table::set(attr(out, "table"), NULL, "SOURCE_TABLE", "ROOM_TYPE_DATA")
    # Each owning converter supplies its source metadata alongside the objects.
    attr(out, "sources") <- unname(unlist(lapply(conv, attr, "sources"), recursive = FALSE))
    out
}

# Internal gains used by Calload exist only when rooms can select a room-type
# template; per-room drawing objects are not the authoritative numeric source.
internal_gains__has_room_type_data <- function(dest) {
    db_has_rows(dest, "ROOM") && db_has_rows(dest, "ROOM_TYPE_DATA")
}

# Fail on dangling room-type, schedule, or distribution references before an
# invalid or silently incomplete EnergyPlus gain object can be generated.
internal_gains__assert_references <- function(gain, label) {
    missing_type <- is.na(gain$ROOM_TYPE_DATA_ID)
    active <- !is.na(gain$ACTIVE) & gain$ACTIVE != 0L
    missing_schedule <- active &
        !missing_type &
        (is.na(gain$SCHEDULE_ID) |
            gain$SCHEDULE_ID == 0L |
            is.na(gain$SCHEDULE_NAME))
    missing_distribution <- active &
        !missing_type &
        (is.na(gain$DIST_MODE_ID) | is.na(gain$FRACTION_RADIANT))
    invalid_basis <- active &
        !missing_type &
        (is.na(gain$CALCULATION_BASIS) |
            !gain$CALCULATION_BASIS %in% c(0L, 1L))
    invalid <- missing_type |
        missing_schedule |
        missing_distribution |
        invalid_basis
    if (!any(invalid)) {
        return(invisible(NULL))
    }

    rows <- gain[invalid]
    detail <- paste(
        sprintf(
            paste0(
                "%s: TYPE=%s, SCHEDULE=%s, DIST_MODE=%s, ",
                "CALCULATION_BASIS=%s"
            ),
            rows$ROOM_NAME,
            rows$ROOM_TYPE_ID,
            rows$SCHEDULE_ID,
            rows$DIST_MODE_ID,
            rows$CALCULATION_BASIS
        ),
        collapse = "; "
    )
    stop(
        sprintf(
            "Cannot resolve supported ROOM_TYPE_DATA %s reference(s): %s",
            label,
            detail
        ),
        call. = FALSE
    )
}

# Split one DeST internal gain into a scheduled maximum-minus-minimum object and
# an optional always-on minimum object, with shared source-value validation.
internal_gains__split_minimum <- function(
    gain,
    i,
    max_value,
    min_value,
    field,
    always_on,
    value_factory
) {
    min_value <- internal_gains__zero_if_na(min_value)
    name <- gain$NAME[[i]]
    if (!is.finite(max_value) || !is.finite(min_value)) {
        stop(
            sprintf(
                "Internal gain '%s' has a non-finite minimum or maximum value.",
                name
            ),
            call. = FALSE
        )
    }
    if (min_value > max_value) {
        stop(
            sprintf(
                "Internal gain '%s' minimum (%s) exceeds maximum (%s).",
                name,
                min_value,
                max_value
            ),
            call. = FALSE
        )
    }

    variable_value <- max_value - min_value
    out <- list()
    if (variable_value > 0 || min_value <= 0) {
        value <- value_factory(
            gain,
            i,
            name,
            gain$SCHEDULE_NAME[[i]]
        )
        value[[field]] <- variable_value
        out <- c(out, list(value))
    }
    if (min_value > 0) {
        value <- value_factory(
            gain,
            i,
            paste(name, "Minimum"),
            always_on
        )
        value[[field]] <- min_value
        out <- c(out, list(value))
    }
    out
}

# Resolve the version-specific zone-reference label shared by internal-gain
# objects from field 2 of the selected target IDD class.
internal_gains__zone_field_name <- function(ep, class) {
    conv__idd_field_name(ep, class, 2L)
}

# Resolve the version-specific People design-level fields from the target IDD.
internal_gains__people_field_names <- function(ep) {
    stats::setNames(
        vapply(
            5:7,
            function(field) conv__idd_field_name(ep, "People", field),
            character(1L)
        ),
        c("number", "per_area", "area_per_person")
    )
}

# ROOM.TYPE -> ROOM_TYPE_DATA occupant fields -> People. The outdoor-air field
# is handled separately by outdoor_air__convert() so People remains focused on
# internal sensible and latent heat gains.
internal_gains__convert_people <- function(dest, ep, people_heat = "constant") {
    people_heat <- match.arg(people_heat, c("constant", "temperature_dependent"))
    if (!internal_gains__has_room_type_data(dest)) {
        return(NULL)
    }

    # NOTE: In DeST, the dehumidification load is calculated by the humidity
    # generated by people multiplied by the latent heat of vaporization (2500
    # kJ/kg, a fixed value). In EnergyPlus, the total heat generated by people
    # is input and latent heat is calculated from the Sensible Heat Fraction.
    # ROOM_TYPE_DATA stores the standard 68/109/184 g/h values despite its old
    # Access field comment saying kg/h; Calload serializes those integers
    # unchanged. Convert g/h to W with 2.5 kJ/g before adding sensible heat.
    people <- DBI::dbGetQuery(
        dest,
        "
        SELECT
            R.ID               AS ID,
            R.NAME || ' People' AS NAME,
            R.ID               AS ROOM_ID,
            R.NAME             AS ROOM_NAME,
            R.TYPE             AS ROOM_TYPE_ID,
            T.ID               AS ROOM_TYPE_DATA_ID,
            T.O_SCHEDULE       AS SCHEDULE_ID,
            S.NAME             AS SCHEDULE_NAME,
            T.O_DIST_MODE      AS DIST_MODE_ID,
            T.O_PER_AREA       AS CALCULATION_BASIS,
            CASE WHEN T.O_MAXNUMBER > 0 OR T.O_MINNUMBER > 0
                THEN 1 ELSE 0 END AS ACTIVE,
            CASE
                WHEN T.O_PER_AREA = 1 THEN 'People/Area'
                ELSE 'People'
            END                AS METHOD,
            CASE
                WHEN T.O_PER_AREA != 1 THEN T.O_MAXNUMBER
                ELSE NULL
            END                AS NUMBER_OF_PEOPLE,
            CASE
                WHEN T.O_PER_AREA = 1 THEN T.O_MAXNUMBER
                ELSE NULL
            END                AS PEOPLE_PER_AREA,
            CASE
                WHEN T.O_PER_AREA != 1 THEN T.O_MINNUMBER
                ELSE NULL
            END                AS MIN_NUMBER_OF_PEOPLE,
            CASE
                WHEN T.O_PER_AREA = 1 THEN T.O_MINNUMBER
                ELSE NULL
            END                AS MIN_PEOPLE_PER_AREA,
            T.O_HEAT_PER_PERSON + T.O_DAMP_PER_PERSON * 2.5 / 3.6
                               AS ACTIVITY_LEVEL,
            T.O_HEAT_PER_PERSON AS BASE_SENSIBLE_HEAT,
            CASE WHEN T.O_HEAT_PER_PERSON + T.O_DAMP_PER_PERSON = 0
                THEN 1 ELSE T.O_HEAT_PER_PERSON /
                (T.O_HEAT_PER_PERSON + T.O_DAMP_PER_PERSON * 2.5 / 3.6) END
                               AS SENSIBLE_HEAT_FRACTION,
            ROUND(1.0 - DM.DIST_AIR, 3)
                               AS FRACTION_RADIANT,
            T.O_MIN_REQUIRE_FRESH_AIR
                               AS MIN_FRESH_AIR
        FROM ROOM R
        LEFT JOIN ROOM_TYPE_DATA T
        ON R.TYPE = T.ID
        LEFT JOIN SCHEDULE_YEAR S
        ON T.O_SCHEDULE = S.SCHEDULE_ID
        LEFT JOIN DIST_MODE DM
        ON T.O_DIST_MODE = DM.DIST_MODE_ID
        ORDER BY R.ID
        "
    )
    data.table::setDT(people)
    internal_gains__assert_references(people, "occupant")
    people <- people[ACTIVE != 0L]
    if (nrow(people) == 0L) {
        return(NULL)
    }
    dt_force_numeric(
        people,
        c(
            "NUMBER_OF_PEOPLE",
            "PEOPLE_PER_AREA",
            "MIN_NUMBER_OF_PEOPLE",
            "MIN_PEOPLE_PER_AREA",
            "ACTIVITY_LEVEL",
            "BASE_SENSIBLE_HEAT",
            "SENSIBLE_HEAT_FRACTION",
            "FRACTION_RADIANT",
            "MIN_FRESH_AIR"
        )
    )
    data.table::set(
        people,
        NULL,
        "ACTIVITY_SCHEDULE_NAME",
        sprintf("Activity Level %.2f W", people$ACTIVITY_LEVEL)
    )

    activity <- unique(people[, .(ACTIVITY_SCHEDULE_NAME, ACTIVITY_LEVEL)])
    has_min <- any(
        internal_gains__has_positive_minimum(people$MIN_NUMBER_OF_PEOPLE) |
            internal_gains__has_positive_minimum(people$MIN_PEOPLE_PER_AREA)
    )

    # NOTE: In DeST, the actual people number is calculated via:
    # min_val + sch_val * (max_val - min_val)
    #
    # EnergyPlus People objects multiply the design level by a schedule. When
    # the DeST minimum is non-zero, represent the same profile with two People
    # objects: a constant minimum object plus a scheduled (max - min) object.
    always_on <- "Always On - DeST Minimum People"
    zone_field_name <- internal_gains__zone_field_name(ep, "People")
    field_names <- internal_gains__people_field_names(ep)
    people_objects <- unlist(
        lapply(seq_len(nrow(people)), function(i) {
            internal_gains__people_values(
                people,
                i,
                always_on,
                zone_field_name,
                field_names
            )
        }),
        recursive = FALSE
    )

    parts <- list(
        activity = conv__add(
            dest,
            ep,
            # TODO: handle the case when a generated activity-level schedule
            #       name already exists in the converted model.
            "Schedule:Constant" := list(
                name = activity$ACTIVITY_SCHEDULE_NAME,
                schedule_type_limits_name = NULL,
                hourly_value = activity$ACTIVITY_LEVEL
            )
        )
    )

    if (has_min) {
        parts$minimum_schedule <- internal_gains__always_on(
            dest,
            ep,
            always_on
        )
    }

    # NOTE: In EnergyPlus, the people activity level can be changed via
    # schedules. However, in DeST, it is a fixed value. So here we create a
    # constant activity-level schedule for each distinct activity level.
    parts$people <- conv__add_objects(dest, ep, "People", people_objects)
    # The bshell execution switch is absent from the source database. Preserve
    # the caller's choice in the saved People objects as well as the API call.
    data.table::set(parts$people$object, NULL, "comment",
        rep(list(paste0("DeST people_heat mode: ", people_heat)), nrow(parts$people$object)))
    if (people_heat == "temperature_dependent") {
        parts$temperature <- internal_gains__people_temperature(dest, ep, people)
    }

    out <- conv__combine_outputs(parts, table = people)
    attr(out, "sources") <- internal_gains__source_specs(dest, people,
        people_objects, "people", field_names[["number"]], field_names[["per_area"]],
        people_heat)
    out
}

# Generate the native sensible-heat relation in one place for both the people
# correction and any subsequent redistribution of that same sensible heat.
internal_gains__people_sensible_lines <- function(heat, temperature, variable) {
    c(sprintf("SET %s = %.17g + 5.536 * (26 - %s)", variable, heat, temperature),
        sprintf("SET %s = @MAX 0 %s", variable, variable))
}

# Preserve the native previous-temperature sensible source while leaving the
# independently represented People count and moisture input unchanged. Native
# hourly equipment replay and changing-occupancy free-float checks establish
# the source rule; radiant recipient fractions still follow EnergyPlus.
internal_gains__people_temperature <- function(dest, ep, people) {
    if (numeric_version(as.character(ep$version())) < numeric_version("9.1.0")) {
        stop("Temperature-dependent people heat requires EnergyPlus 9.1.0 or newer.",
            call. = FALSE)
    }
    if (any(!is.finite(people$BASE_SENSIBLE_HEAT) | people$BASE_SENSIBLE_HEAT < 0)) {
        stop("Temperature-dependent people heat requires non-negative finite sensible inputs.",
            call. = FALSE)
    }
    classes <- c("OtherEquipment", "EnergyManagementSystem:Sensor",
        "EnergyManagementSystem:InternalVariable", "EnergyManagementSystem:Actuator",
        "EnergyManagementSystem:Program", "EnergyManagementSystem:ProgramCallingManager")
    values <- stats::setNames(lapply(classes, function(class) list()), classes)
    always_on <- "Always On - DeST People Temperature"
    zone_field <- conv__idd_field_name(ep, "OtherEquipment", 3L)
    for (i in seq_len(nrow(people))) {
        prefix <- paste0("DeST_People_T_", i)
        name <- paste(people$NAME[[i]], "Temperature Correction")
        per_area <- people$CALCULATION_BASIS[[i]] == 1L
        maximum <- if (per_area) people$PEOPLE_PER_AREA[[i]] else people$NUMBER_OF_PEOPLE[[i]]
        minimum <- internal_gains__zero_if_na(if (per_area) people$MIN_PEOPLE_PER_AREA[[i]] else people$MIN_NUMBER_OF_PEOPLE[[i]])
        source <- list(name = name, fuel_type = "None", schedule_name = always_on,
            design_level_calculation_method = "EquipmentLevel", design_level = 0,
            fraction_latent = 0, fraction_radiant = people$FRACTION_RADIANT[[i]],
            fraction_lost = 0, end_use_subcategory = "DeST People Temperature Correction")
        source[[zone_field]] <- people$ROOM_NAME[[i]]
        values$OtherEquipment <- c(values$OtherEquipment, list(source))
        values[["EnergyManagementSystem:Sensor"]] <- c(values[["EnergyManagementSystem:Sensor"]], list(
            list(name = paste0(prefix, "_Temperature"),
                output_variable_or_output_meter_index_key_name = people$ROOM_NAME[[i]],
                output_variable_or_output_meter_name = "Zone Mean Air Temperature"),
            list(name = paste0(prefix, "_Schedule"),
                output_variable_or_output_meter_index_key_name = people$SCHEDULE_NAME[[i]],
                output_variable_or_output_meter_name = "Schedule Value")))
        if (per_area) {
            values[["EnergyManagementSystem:InternalVariable"]] <- c(values[["EnergyManagementSystem:InternalVariable"]],
                list(list(name = paste0(prefix, "_Area"),
                    internal_data_index_key_name = people$ROOM_NAME[[i]], internal_data_type = "Zone Floor Area")))
        }
        values[["EnergyManagementSystem:Actuator"]] <- c(values[["EnergyManagementSystem:Actuator"]],
            list(list(name = paste0(prefix, "_Power"), actuated_component_unique_name = name,
                actuated_component_type = "OtherEquipment", actuated_component_control_type = "Power Level")))
        # Apply minimum plus scheduled range once, before the zone multiplier.
        # Clamp the total native sensible power before subtracting the existing
        # constant People contribution, so high-temperature correction can be negative.
        lines <- c(
            sprintf("SET Count = %.17g + %.17g * %s_Schedule", minimum, maximum - minimum, prefix),
            if (per_area) sprintf("SET Count = Count * %s_Area", prefix),
            internal_gains__people_sensible_lines(people$BASE_SENSIBLE_HEAT[[i]],
                paste0(prefix, "_Temperature"), "Sensible"),
            sprintf("SET %s_Power = (Sensible - %.17g) * Count", prefix, people$BASE_SENSIBLE_HEAT[[i]]))
        fields <- vapply(seq_along(lines) + 1L,
            function(field) conv__idd_field_name(ep, "EnergyManagementSystem:Program", field), character(1L))
        values[["EnergyManagementSystem:Program"]] <- c(values[["EnergyManagementSystem:Program"]],
            list(c(list(name = paste0(prefix, "_Control")), stats::setNames(as.list(lines), fields))))
        # Gains are consumed during heat-balance initialization. Calling this
        # before the predictor leaves an extra, experimentally confirmed lag.
        values[["EnergyManagementSystem:ProgramCallingManager"]] <- c(values[["EnergyManagementSystem:ProgramCallingManager"]],
            list(list(name = paste0(prefix, "_Manager"),
                energyplus_model_calling_point = "BeginZoneTimestepBeforeInitHeatBalance",
                program_name_1 = paste0(prefix, "_Control"))))
    }
    conv__combine_outputs(c(list(internal_gains__always_on(dest, ep, always_on)),
        lapply(classes, function(class) conv__add_objects(dest, ep, class, values[[class]]))))
}

internal_gains__people_values <- function(
    people,
    i,
    always_on,
    zone_field_name = "Zone or ZoneList or Space or SpaceList Name",
    field_names = c(
        number = "number_of_people",
        per_area = "people_per_floor_area",
        area_per_person = "floor_area_per_person"
    )
) {
    if (people$METHOD[[i]] == "People") {
        max_value <- people$NUMBER_OF_PEOPLE[[i]]
        min_value <- people$MIN_NUMBER_OF_PEOPLE[[i]]
        field <- field_names[["number"]]
    } else {
        max_value <- people$PEOPLE_PER_AREA[[i]]
        min_value <- people$MIN_PEOPLE_PER_AREA[[i]]
        field <- field_names[["per_area"]]
    }

    value_factory <- function(gain, row, name, schedule) {
        internal_gains__people_value(
            gain,
            row,
            name,
            schedule,
            zone_field_name,
            field_names
        )
    }
    internal_gains__split_minimum(
        people,
        i,
        max_value,
        min_value,
        field,
        always_on,
        value_factory
    )
}

internal_gains__people_value <- function(
    people,
    i,
    name,
    schedule,
    zone_field_name,
    field_names
) {
    value <- list(
        name = name,
        number_of_people_schedule_name = schedule,
        number_of_people_calculation_method = people$METHOD[[i]],
        fraction_radiant = people$FRACTION_RADIANT[[i]],
        sensible_heat_fraction = people$SENSIBLE_HEAT_FRACTION[[i]],
        activity_level_schedule_name = people$ACTIVITY_SCHEDULE_NAME[[i]]
    )
    value[[zone_field_name]] <- people$ROOM_NAME[[i]]
    for (field in field_names) {
        value[[field]] <- NULL
    }
    value
}

# ROOM.TYPE -> ROOM_TYPE_DATA lighting fields -> Lights.
internal_gains__convert_lights <- function(dest, ep) {
    if (!internal_gains__has_room_type_data(dest)) {
        return(NULL)
    }

    lights <- DBI::dbGetQuery(
        dest,
        "
        SELECT
            R.ID             AS ID,
            R.NAME || ' Lights' AS NAME,
            R.ID             AS ROOM_ID,
            R.NAME           AS ROOM_NAME,
            R.TYPE           AS ROOM_TYPE_ID,
            T.ID             AS ROOM_TYPE_DATA_ID,
            R.AREA           AS ROOM_AREA,
            T.L_SCHEDULE     AS SCHEDULE_ID,
            S.NAME           AS SCHEDULE_NAME,
            T.L_DIST_MODE    AS DIST_MODE_ID,
            T.L_PER_AREA     AS CALCULATION_BASIS,
            CASE WHEN T.L_MAXPOWER > 0 OR T.L_MINPOWER > 0
                THEN 1 ELSE 0 END AS ACTIVE,
            CASE
                WHEN T.L_PER_AREA = 1 THEN 'Watts/Area'
                ELSE 'LightingLevel'
            END              AS METHOD,
            CASE
                WHEN T.L_PER_AREA != 1 THEN T.L_MAXPOWER
                ELSE NULL
            END              AS LIGHTING_LEVEL,
            CASE
                WHEN T.L_PER_AREA = 1 THEN T.L_MAXPOWER
                ELSE NULL
            END              AS WATTS_PER_AREA,
            CASE
                WHEN T.L_PER_AREA != 1 THEN T.L_MINPOWER
                ELSE NULL
            END              AS MIN_LIGHTING_LEVEL,
            CASE
                WHEN T.L_PER_AREA = 1 THEN T.L_MINPOWER
                ELSE NULL
            END              AS MIN_WATTS_PER_AREA,
            ROUND(1.0 - DM.DIST_AIR, 3)
                             AS FRACTION_RADIANT,
            T.L_HEAT_RATE    AS HEAT_TO_ELECTRIC_RATIO
        FROM ROOM R
        LEFT JOIN ROOM_TYPE_DATA T
        ON R.TYPE = T.ID
        LEFT JOIN SCHEDULE_YEAR S
        ON T.L_SCHEDULE = S.SCHEDULE_ID
        LEFT JOIN DIST_MODE DM
        ON T.L_DIST_MODE = DM.DIST_MODE_ID
        ORDER BY R.ID
        "
    )
    data.table::setDT(lights)
    internal_gains__assert_references(lights, "lighting")
    lights <- lights[ACTIVE != 0L]
    if (nrow(lights) == 0L) {
        return(NULL)
    }
    dt_force_numeric(
        lights,
        c(
            "LIGHTING_LEVEL",
            "WATTS_PER_AREA",
            "MIN_LIGHTING_LEVEL",
            "MIN_WATTS_PER_AREA",
            "ROOM_AREA",
            "FRACTION_RADIANT",
            "HEAT_TO_ELECTRIC_RATIO"
        )
    )
    watts_per_area_field <- conv__idd_field_name(ep, "Lights", 6L)
    has_min <- any(
        internal_gains__has_positive_minimum(lights$MIN_LIGHTING_LEVEL) |
            internal_gains__has_positive_minimum(lights$MIN_WATTS_PER_AREA)
    )

    # DeST light gains use the same min + schedule * (max - min) pattern as
    # people. Use a constant minimum Lights object plus a scheduled variable
    # object when MINPOWER is non-zero.
    always_on <- "Always On - DeST Minimum Lights"
    zone_field_name <- internal_gains__zone_field_name(ep, "Lights")
    light_objects <- unlist(
        lapply(seq_len(nrow(lights)), function(i) {
            internal_gains__light_values(
                lights,
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

    parts$lights <- conv__add_objects(dest, ep, "Lights", light_objects)
    # Native lighting heat equals scheduled electrical power times HEAT_RATE.
    # Keep Lights electricity unchanged and apply the thermal difference with
    # an unmetered source following the exact same minimum/variable schedules.
    heat_ratio_objects <- unlist(
        lapply(seq_len(nrow(lights)), function(i) {
            internal_gains__light_ratio_values(lights, i,
                watts_per_area_field, always_on, ep)
        }),
        recursive = FALSE
    )
    if (length(heat_ratio_objects)) {
        parts$heat_ratio <- conv__add_objects(dest, ep,
            "OtherEquipment", heat_ratio_objects)
    }

    out <- conv__combine_outputs(parts, table = lights)
    attr(out, "sources") <- internal_gains__source_specs(dest, lights,
        light_objects, "light", "lighting_level", watts_per_area_field)
    out
}

# Correct only room sensible heat; OtherEquipment supports signed design
# levels, so a constant heat ratio needs no EMS or additional timestep delay.
internal_gains__light_ratio_values <- function(lights, i, watts_per_area_field,
    always_on, ep) {
    ratio <- lights$HEAT_TO_ELECTRIC_RATIO[[i]]
    if (!is.finite(ratio) || ratio < 0) {
        stop(sprintf("Lighting heat-to-electricity ratio for '%s' must be finite and non-negative.",
            lights$NAME[[i]]), call. = FALSE)
    }
    if (ratio == 1) return(list())
    source <- internal_gains__light_values(lights, i, watts_per_area_field,
        always_on, internal_gains__zone_field_name(ep, "Lights"))
    zone_field <- conv__idd_field_name(ep, "OtherEquipment", 3L)
    lapply(source, function(light) {
        per_area <- lights$METHOD[[i]] == "Watts/Area"
        area <- if (per_area) lights$ROOM_AREA[[i]] else 1
        if (!is.finite(area) || area <= 0) {
            stop(sprintf("Lighting heat correction for '%s' requires a positive finite room area.",
                lights$NAME[[i]]), call. = FALSE)
        }
        value <- list(name = paste(light$name, "Heat Ratio Correction"),
            fuel_type = "None", schedule_name = light$schedule_name,
            design_level_calculation_method = "EquipmentLevel",
            fraction_latent = 0, fraction_radiant = light$fraction_radiant,
            fraction_lost = 0, end_use_subcategory = "DeST Lighting Heat Ratio")
        value[[zone_field]] <- lights$ROOM_NAME[[i]]
        # Sum of the Lights source and this correction is ratio * P, with
        # convective/radiant shares retained and zero change to electric power.
        # EnergyPlus accepts negative total design power, but rejects a
        # negative per-area input despite the IDD's general signed-input note.
        # ROOM.AREA is also copied without rounding to the EnergyPlus Zone.
        watts <- if (per_area) light[[watts_per_area_field]] * area else light$lighting_level
        value$design_level <- (ratio - 1) * watts
        value
    })
}

# Split the electrical lighting input into its constant minimum and scheduled
# range; the heat-ratio correction reuses these same object definitions.
internal_gains__light_values <- function(
    lights,
    i,
    watts_per_area_field,
    always_on,
    zone_field_name = "Zone or ZoneList or Space or SpaceList Name"
) {
    if (lights$METHOD[[i]] == "LightingLevel") {
        max_value <- lights$LIGHTING_LEVEL[[i]]
        min_value <- lights$MIN_LIGHTING_LEVEL[[i]]
        field <- "lighting_level"
    } else {
        max_value <- lights$WATTS_PER_AREA[[i]]
        min_value <- lights$MIN_WATTS_PER_AREA[[i]]
        field <- watts_per_area_field
    }

    value_factory <- function(gain, row, name, schedule) {
        internal_gains__light_value(
            gain,
            row,
            name,
            schedule,
            zone_field_name
        )
    }
    internal_gains__split_minimum(
        lights,
        i,
        max_value,
        min_value,
        field,
        always_on,
        value_factory
    )
}

internal_gains__light_value <- function(
    lights,
    i,
    name,
    schedule,
    zone_field_name
) {
    value <- list(
        name = name,
        schedule_name = schedule,
        design_level_calculation_method = lights$METHOD[[i]],
        lighting_level = NULL,
        watts_per_person = NULL,
        return_air_fraction = 0,
        fraction_radiant = lights$FRACTION_RADIANT[[i]],
        # DeST DIST_MODE already allocates the thermal lighting gain between
        # zone air and surfaces. A separate visible fraction would divert heat
        # into EnergyPlus' optical path and can lose it on zero-absorptance
        # surfaces, which is not part of the DeST heat-gain definition.
        fraction_visible = 0,
        # DeST L_HEAT_RATE is a heat-to-electricity ratio. It does not describe
        # the fraction eligible for EnergyPlus daylighting replacement.
        fraction_replaceable = 0,
        end_use_subcategory = "General"
    )
    value[[zone_field_name]] <- lights$ROOM_NAME[[i]]
    value
}

# ROOM.TYPE -> ROOM_TYPE_DATA equipment fields -> sensible and moisture sources.
internal_gains__convert_electric_equipment <- function(dest, ep) {
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
            internal_gains__equipment_values(
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
    attr(out, "sources") <- internal_gains__source_specs(dest, equipment,
        equipment_objects, "equipment", "design_level", watts_per_area_field)
    out
}

# Describe the exact named values just created by an internal-gain converter.
# This does not parse an IDF: object identity, minimum splits and design levels
# come from that converter's own value lists; source fractions come from DeST.
internal_gains__source_specs <- function(dest, gain, values, kind, total_field,
    area_field, people_heat = "constant") {
    columns <- c("DIST_MODE_ID", "DIST_AIR", "DIST_AROUND", "DIST_FLOOR", "DIST_ROOF")
    # Older partial schemas can still use the existing gain conversion, but
    # cannot supply a complete prescribed surface distribution. Never invent
    # missing fractions; the source projector rejects this absent inventory.
    if (!db_has_fields(dest, "DIST_MODE", columns)) return(NULL)
    distributions <- DBI::dbGetQuery(dest, paste("SELECT", paste(columns, collapse = ","), "FROM DIST_MODE"))
    rooms <- DBI::dbGetQuery(dest, "SELECT ID, AREA FROM ROOM")
    lapply(values, function(value) {
        row <- match(value$name, gain$NAME)
        if (is.na(row)) row <- match(value$name, paste(gain$NAME, "Minimum"))
        stopifnot(!is.na(row))
        per_area <- gain$CALCULATION_BASIS[[row]] == 1L
        area <- rooms$AREA[match(gain$ROOM_ID[[row]], rooms$ID)]
        power <- if (per_area) value[[area_field]] * area else value[[total_field]]
        mode <- distributions[match(gain$DIST_MODE_ID[[row]], distributions$DIST_MODE_ID), -1L]
        mode <- stats::setNames(as.list(mode), c("air", "wall", "floor", "roof"))
        item <- list(name = value$name, zone = gain$ROOM_NAME[[row]], kind = kind,
            design_power = power, schedule = if (kind == "people")
                value$number_of_people_schedule_name else value$schedule_name,
            mode = mode, existing_radiant = value$fraction_radiant,
            existing_air = 1 - value$fraction_radiant, companion_objects = character())
        if (kind == "people") {
            item$sensible_heat <- gain$BASE_SENSIBLE_HEAT[[row]]
            item$temperature_dependent <- people_heat == "temperature_dependent"
            # One temperature correction covers both the minimum and the
            # scheduled count; those two source streams share its identity.
            if (item$temperature_dependent) item$companion_objects <-
                paste(gain$NAME[[row]], "Temperature Correction")
        } else if (kind == "light") {
            ratio <- gain$HEAT_TO_ELECTRIC_RATIO[[row]]
            item$design_power <- power * ratio
            if (ratio != 1) item$companion_objects <- paste(value$name, "Heat Ratio Correction")
        } else if (any(c(gain$MAX_HUM[[row]], gain$MIN_HUM[[row]]) != 0, na.rm = TRUE)) {
            # Moisture still converts through its established owner. The
            # sensible-only projection must explicitly decline that extension.
            item$unsupported_reason <- "Equipment moisture is not supported by the prescribed source projector."
        }
        item
    })
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
                "Use to_eplus(..., ver = '9.6.0', hvac = 'ideal_loads')",
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
    values <- stats::setNames(lapply(classes, function(class) list()), classes)
    zone_field <- conv__idd_field_name(ep, "OtherEquipment", 3L)
    area_field <- conv__idd_field_name(ep, "OtherEquipment", 7L)
    for (i in rows) {
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
        values$OtherEquipment <- c(values$OtherEquipment, list(source))
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
        values[["EnergyManagementSystem:Sensor"]] <-
            c(values[["EnergyManagementSystem:Sensor"]], sensors)
        if (per_area) {
            values[["EnergyManagementSystem:InternalVariable"]] <- c(
                values[["EnergyManagementSystem:InternalVariable"]],
                list(list(
                    name = paste0(prefix, "_Area"),
                    internal_data_index_key_name = equipment$ROOM_NAME[[i]],
                    internal_data_type = "Zone Floor Area"
                ))
            )
        }
        values[["EnergyManagementSystem:Actuator"]] <- c(
            values[["EnergyManagementSystem:Actuator"]],
            list(list(
                name = paste0(prefix, "_Power"),
                actuated_component_unique_name = name,
                actuated_component_type = "OtherEquipment",
                actuated_component_control_type = "Power Level"
            ))
        )
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
        # Program lines are extensible; resolve all labels from the target IDD.
        fields <- vapply(
            seq_along(lines) + 1L,
            function(field) {
                conv__idd_field_name(
                    ep,
                    "EnergyManagementSystem:Program",
                    field
                )
            },
            character(1L)
        )
        values[["EnergyManagementSystem:Program"]] <- c(
            values[["EnergyManagementSystem:Program"]],
            list(c(
                list(name = paste0(prefix, "_Control")),
                stats::setNames(as.list(lines), fields)
            ))
        )
        # Internal gains are evaluated during heat-balance initialization.
        # The latest available zone temperature removes fixed-enthalpy bias;
        # abrupt temperature changes can still cause a one-zone-step residual.
        values[["EnergyManagementSystem:ProgramCallingManager"]] <- c(
            values[["EnergyManagementSystem:ProgramCallingManager"]],
            list(list(
                name = paste0(prefix, "_Manager"),
                energyplus_model_calling_point = "BeginZoneTimestepBeforeInitHeatBalance",
                program_name_1 = paste0(prefix, "_Control")
            ))
        )
    }
    conv__combine_outputs(lapply(classes, function(class) {
        conv__add_objects(dest, ep, class, values[[class]])
    }))
}

internal_gains__equipment_values <- function(
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
        internal_gains__equipment_value(
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

internal_gains__equipment_value <- function(
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

internal_gains__has_positive_minimum <- function(x) {
    !is.na(x) & x > 0
}

internal_gains__zero_if_na <- function(x) {
    if (is.na(x)) 0 else x
}

internal_gains__always_on <- function(dest, ep, name) {
    conv__add(
        dest,
        ep,
        "Schedule:Constant" := list(
            name = name,
            schedule_type_limits_name = NULL,
            hourly_value = 1
        )
    )
}
