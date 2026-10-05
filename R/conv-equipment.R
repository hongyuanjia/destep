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
            1.0 - DM.DIST_AIR
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

# Equipment owns its kg/h source fields and validation. The target binding is
# shared with other prescribed mass sources without coupling their source data.
# Native source = MIN_HUM + (MAX_HUM - MIN_HUM) * E_SCHEDULE; multiply
# by room floor area only for E_PER_AREA = 1. Sensible watts stay independent.
equipment__moisture_objects <- function(dest, ep, equipment) {
    moisture__objects(dest, ep, equipment, "DeST_Moisture_", "Equipment")
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
