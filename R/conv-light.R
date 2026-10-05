# ROOM.TYPE -> ROOM_TYPE_DATA lighting fields -> Lights.
light__convert <- function(dest, ep) {
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
            CASE WHEN T.L_MAXPOWER != 0 OR T.L_MINPOWER != 0
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
            1.0 - DM.DIST_AIR
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
    # Retain each room's minimum/variable pair for its heat-ratio correction;
    # rebuilding those objects would repeat both validation and allocation.
    light_by_room <- lapply(seq_len(nrow(lights)), function(i) {
        light__values(
            lights,
            i,
            watts_per_area_field,
            always_on,
            zone_field_name
        )
    })
    light_objects <- unlist(light_by_room, recursive = FALSE)

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
    # All correction objects use the same IDD field. Leave the unity-ratio
    # path free of this lookup; invalid ratios are still checked by the owner.
    correction_zone_field <- if (
        any(lights$HEAT_TO_ELECTRIC_RATIO != 1, na.rm = TRUE)
    ) {
        conv__idd_field_name(ep, "OtherEquipment", 3L)
    }
    heat_ratio_objects <- unlist(
        lapply(seq_len(nrow(lights)), function(i) {
            light__ratio_values(
                lights,
                i,
                light_by_room[[i]],
                watts_per_area_field,
                correction_zone_field
            )
        }),
        recursive = FALSE
    )
    if (length(heat_ratio_objects)) {
        parts$heat_ratio <- conv__add_objects(
            dest,
            ep,
            "OtherEquipment",
            heat_ratio_objects
        )
    }

    out <- conv__combine_outputs(parts, table = lights)
    out
}

# Correct only room sensible heat; OtherEquipment supports signed design
# levels, so a constant heat ratio needs no EMS or additional timestep delay.
light__ratio_values <- function(
    lights,
    i,
    source,
    watts_per_area_field,
    zone_field
) {
    ratio <- lights$HEAT_TO_ELECTRIC_RATIO[[i]]
    if (!is.finite(ratio) || ratio < 0) {
        stop(
            sprintf(
                "Lighting heat-to-electricity ratio for '%s' must be finite and non-negative.",
                lights$NAME[[i]]
            ),
            call. = FALSE
        )
    }
    if (ratio == 1) {
        return(list())
    }
    if (!length(source)) {
        return(list())
    }
    # Both minimum and variable objects share the source basis and room area.
    per_area <- lights$METHOD[[i]] == "Watts/Area"
    area <- if (per_area) lights$ROOM_AREA[[i]] else 1
    if (!is.finite(area) || area <= 0) {
        stop(
            sprintf(
                "Lighting heat correction for '%s' requires a positive finite room area.",
                lights$NAME[[i]]
            ),
            call. = FALSE
        )
    }
    lapply(source, function(light) {
        value <- list(
            name = paste(light$name, "Heat Ratio Correction"),
            fuel_type = "None",
            schedule_name = light$schedule_name,
            design_level_calculation_method = "EquipmentLevel",
            fraction_latent = 0,
            fraction_radiant = light$fraction_radiant,
            fraction_lost = 0,
            end_use_subcategory = "DeST Lighting Heat Ratio"
        )
        value[[zone_field]] <- lights$ROOM_NAME[[i]]
        # Sum of the Lights source and this correction is ratio * P, with
        # convective/radiant shares retained and zero change to electric power.
        # EnergyPlus accepts negative total design power, but rejects a
        # negative per-area input despite the IDD's general signed-input note.
        # ROOM.AREA is also copied without rounding to the EnergyPlus Zone.
        watts <- if (per_area) {
            light[[watts_per_area_field]] * area
        } else {
            light$lighting_level
        }
        value$design_level <- (ratio - 1) * watts
        value
    })
}

# Split the electrical lighting input into its constant minimum and scheduled
# range; the heat-ratio correction reuses these same object definitions.
light__values <- function(
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
        light__value(
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

# Describe one Lights object using the selected target zone field.
light__value <- function(
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
