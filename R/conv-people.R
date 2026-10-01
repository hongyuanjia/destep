# Resolve the version-specific People design-level fields from the target IDD.
people__field_names <- function(ep) {
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
people__convert <- function(dest, ep) {
    if (!internal_gains__has_room_type_data(dest)) {
        return(NULL)
    }

    # O_DAMP_PER_PERSON is g/h/person in the serialized native inputs, despite
    # old Access comments saying kg/h. It is a nominal input: native bshell's
    # --const_occupant preserves it, while default occupant moisture varies
    # with temperature. Convert the nominal source without that algorithm.
    # A fixed 2500 kJ/kg conversion changes the effective water source in E+.
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
            T.O_HEAT_PER_PERSON AS ACTIVITY_LEVEL,
            T.O_DAMP_PER_PERSON AS MOISTURE_GRAMS_PER_HOUR,
            T.O_HEAT_PER_PERSON AS BASE_SENSIBLE_HEAT,
            1.0                AS SENSIBLE_HEAT_FRACTION,
            1.0 - DM.DIST_AIR
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
            "MOISTURE_GRAMS_PER_HOUR",
            "SENSIBLE_HEAT_FRACTION",
            "FRACTION_RADIANT",
            "MIN_FRESH_AIR"
        )
    )
    data.table::set(
        people,
        NULL,
        "ACTIVITY_SCHEDULE_NAME",
        sprintf("People Sensible Heat %.17g W", people$ACTIVITY_LEVEL)
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
    field_names <- people__field_names(ep)
    people_objects <- unlist(
        lapply(seq_len(nrow(people)), function(i) {
            people__values(
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
    # Preserve nominal source inputs independently of the DeST execution mode.
    # Keep this boundary visible when the generated IDF is saved or shared.
    data.table::set(
        parts$people$object,
        NULL,
        "comment",
        rep(
            list(c(
                "People sensible heat and moisture preserve nominal source inputs; DeST occupant temperature feedback is not reproduced.",
                "People carries source sensible heat only; its activity is not a comfort-model metabolic input.",
                "Nominal O_DAMP_PER_PERSON is represented by the separate unmetered People Moisture source.",
                "Moisture follows the prescribed input (--const_occupant behavior); DeST's default temperature-dependent occupant moisture is not reproduced."
            )),
            nrow(parts$people$object)
        )
    )
    parts$moisture <- people__moisture_objects(dest, ep, people)
    out <- conv__combine_outputs(parts, table = people)
    attr(out, "sources") <- people__source_specs(
        dest,
        people,
        people_objects,
        field_names[["number"]],
        field_names[["per_area"]]
    )
    out
}

# Build the scheduled and minimum People objects using the source count basis.
people__values <- function(
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
        people__value(
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

# Describe one People object without changing its source heat fractions.
people__value <- function(
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

# Attach People heat metadata to the same object inventory used for conversion.
people__source_specs <- function(
    dest,
    people,
    values,
    total_field,
    area_field
) {
    # Redistributors receive the same nominal W/person as the People activity.
    # The independent moisture companion must remain represented exactly once.
    decorate <- function(item, row) {
        item$sensible_heat <- people$BASE_SENSIBLE_HEAT[[row]]
        if (
            !is.na(people$MOISTURE_GRAMS_PER_HOUR[[row]]) &&
                people$MOISTURE_GRAMS_PER_HOUR[[row]] > 0
        ) {
            item$companion_objects <- c(
                item$companion_objects,
                paste(people$NAME[[row]], "Moisture")
            )
        }
        item
    }
    internal_gains__source_specs(
        dest,
        people,
        values,
        "people",
        total_field,
        area_field,
        "number_of_people_schedule_name",
        decorate
    )
}

# Convert nominal g/h/person and min/max occupant counts to kg/h or kg/h/m2.
# People carries sensible heat only; the unmetered latent companion therefore
# preserves prescribed water input without a second sensible or electric source.
# This is not a reproduction of DeST's temperature-dependent occupant model.
people__moisture_objects <- function(dest, ep, people) {
    damp <- people$MOISTURE_GRAMS_PER_HOUR
    damp[is.na(damp) & !is.nan(damp)] <- 0
    invalid <- !is.finite(damp) | damp < 0
    if (any(invalid)) {
        stop(
            sprintf(
                "Invalid O_DAMP_PER_PERSON (require finite nonnegative g/h/person): %s",
                paste(people$NAME[invalid], collapse = "; ")
            ),
            call. = FALSE
        )
    }
    per_area <- people$CALCULATION_BASIS == 1L
    maximum <- data.table::fifelse(
        per_area,
        people$PEOPLE_PER_AREA,
        people$NUMBER_OF_PEOPLE
    )
    minimum <- data.table::fifelse(
        per_area,
        people$MIN_PEOPLE_PER_AREA,
        people$MIN_NUMBER_OF_PEOPLE
    )
    source <- data.table::data.table(
        NAME = people$NAME,
        ROOM_NAME = people$ROOM_NAME,
        SCHEDULE_NAME = people$SCHEDULE_NAME,
        CALCULATION_BASIS = people$CALCULATION_BASIS,
        METHOD = data.table::fifelse(per_area, "Watts/Area", "EquipmentLevel"),
        MAX_HUM = maximum * damp / 1000,
        MIN_HUM = minimum * damp / 1000
    )
    moisture__objects(dest, ep, source, "DeST_People_Moisture_", "People")
}
