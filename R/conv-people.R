destep_conv_internal_gains <- function(dest, ep) {
    conv <- Filter(Negate(is.null), list(
        OCCUPANT_GAINS = destep_conv_people(dest, ep),
        LIGHT_GAINS = destep_conv_lights(dest, ep),
        EQUIPMENT_GAINS = destep_conv_electric_equipment(dest, ep)
    ))

    destep_combine_outputs(conv)
}

# OCCUPANT_GAINS -> People
destep_conv_people <- function(dest, ep) {
    if (!destep_has_rows(dest, "OCCUPANT_GAINS")) return(NULL)

    people <- DBI::dbGetQuery(
        dest,
        "
        SELECT
            O.GAIN_ID          AS ID,
            O.NAME             AS NAME,
            R.NAME             AS ROOM_NAME,
            S.NAME             AS SCHEDULE_NAME,
            CASE
                WHEN O.PER_AREA = 1 THEN 'People/Area'
                ELSE 'People'
            END                AS METHOD,
            CASE
                WHEN O.PER_AREA != 1 THEN O.MAXNUMBER
                ELSE NULL
            END                AS NUMBER_OF_PEOPLE,
            CASE
                WHEN O.PER_AREA = 1 THEN O.MAXNUMBER
                ELSE NULL
            END                AS PEOPLE_PER_AREA,
            ROUND(O.HEAT_PER_PERSON + O.DAMP_PER_PERSON * 2500 / 3.6, 2)
                               AS ACTIVITY_LEVEL,
            ROUND(
                O.HEAT_PER_PERSON /
                    (O.HEAT_PER_PERSON + O.DAMP_PER_PERSON * 2500 / 3.6),
                2
            )                  AS SENSIBLE_HEAT_FRACTION,
            ROUND(1.0 - DM.DIST_AIR, 3)
                               AS FRACTION_RADIANT
        FROM OCCUPANT_GAINS O
        LEFT JOIN ROOM R
        ON O.OF_ROOM = R.ID
        LEFT JOIN SCHEDULE_YEAR S
        ON O.SCHEDULE = S.SCHEDULE_ID
        LEFT JOIN DIST_MODE DM
        ON O.DIST_MODE = DM.DIST_MODE_ID
        "
    )
    data.table::setDT(people)
    destep_force_numeric(people, c(
        "NUMBER_OF_PEOPLE", "PEOPLE_PER_AREA", "ACTIVITY_LEVEL",
        "SENSIBLE_HEAT_FRACTION", "FRACTION_RADIANT"
    ))
    data.table::set(
        people, NULL, "ACTIVITY_SCHEDULE_NAME",
        sprintf("Activity Level %.2f W", people$ACTIVITY_LEVEL)
    )

    activity <- unique(people[, .(ACTIVITY_SCHEDULE_NAME, ACTIVITY_LEVEL)])

    destep_combine_outputs(list(
        activity = destep_add(
            dest, ep,
            "Schedule:Constant" := list(
                name = activity$ACTIVITY_SCHEDULE_NAME,
                schedule_type_limits_name = NULL,
                hourly_value = activity$ACTIVITY_LEVEL
            )
        ),
        people = eval(as.call(c(
            destep_add, dest, ep,
            lapply(seq_len(nrow(people)), function(i) {
                val <- list(
                    name = people$NAME[[i]],
                    zone_or_zonelist_or_space_or_spacelist_name = people$ROOM_NAME[[i]],
                    number_of_people_schedule_name = people$SCHEDULE_NAME[[i]],
                    number_of_people_calculation_method = people$METHOD[[i]],
                    number_of_people = if (people$METHOD[[i]] == "People") people$NUMBER_OF_PEOPLE[[i]],
                    people_per_floor_area = if (people$METHOD[[i]] == "People/Area") people$PEOPLE_PER_AREA[[i]],
                    floor_area_per_person = NULL,
                    fraction_radiant = people$FRACTION_RADIANT[[i]],
                    sensible_heat_fraction = people$SENSIBLE_HEAT_FRACTION[[i]],
                    activity_level_schedule_name = people$ACTIVITY_SCHEDULE_NAME[[i]]
                )
                bquote("People" := .(val))
            })
        )))
    ), table = people)
}

# LIGHT_GAINS -> Lights
destep_conv_lights <- function(dest, ep) {
    if (!destep_has_rows(dest, "LIGHT_GAINS")) return(NULL)

    lights <- DBI::dbGetQuery(
        dest,
        "
        SELECT
            L.GAIN_ID        AS ID,
            L.NAME           AS NAME,
            R.NAME           AS ROOM_NAME,
            S.NAME           AS SCHEDULE_NAME,
            CASE
                WHEN L.PER_AREA = 1 THEN 'Watts/Area'
                ELSE 'LightingLevel'
            END              AS METHOD,
            CASE
                WHEN L.PER_AREA != 1 THEN L.MAXPOWER
                ELSE NULL
            END              AS LIGHTING_LEVEL,
            CASE
                WHEN L.PER_AREA = 1 THEN L.MAXPOWER
                ELSE NULL
            END              AS WATTS_PER_AREA,
            ROUND(1.0 - DM.DIST_AIR, 3)
                             AS FRACTION_RADIANT,
            L.HEAT_RATE      AS FRACTION_REPLACEABLE
        FROM LIGHT_GAINS L
        LEFT JOIN ROOM R
        ON L.OF_ROOM = R.ID
        LEFT JOIN SCHEDULE_YEAR S
        ON L.SCHEDULE = S.SCHEDULE_ID
        LEFT JOIN DIST_MODE DM
        ON L.DIST_MODE = DM.DIST_MODE_ID
        "
    )
    data.table::setDT(lights)
    destep_force_numeric(lights, c(
        "LIGHTING_LEVEL", "WATTS_PER_AREA", "FRACTION_RADIANT",
        "FRACTION_REPLACEABLE"
    ))
    watts_per_area_field <- destep_idd_field_name(ep, "Lights", 6L)

    out <- eval(as.call(c(
        destep_add, dest, ep,
        lapply(seq_len(nrow(lights)), function(i) {
            val <- list(
                name = lights$NAME[[i]],
                zone_or_zonelist_or_space_or_spacelist_name = lights$ROOM_NAME[[i]],
                schedule_name = lights$SCHEDULE_NAME[[i]],
                design_level_calculation_method = lights$METHOD[[i]],
                lighting_level = if (lights$METHOD[[i]] == "LightingLevel") lights$LIGHTING_LEVEL[[i]],
                watts_per_person = NULL,
                return_air_fraction = 0,
                fraction_radiant = lights$FRACTION_RADIANT[[i]],
                fraction_visible = 0.2,
                fraction_replaceable = lights$FRACTION_REPLACEABLE[[i]],
                end_use_subcategory = "General"
            )
            val[[watts_per_area_field]] <- if (lights$METHOD[[i]] == "Watts/Area") lights$WATTS_PER_AREA[[i]]
            bquote("Lights" := .(val))
        })
    )))
    attr(out, "table") <- lights

    out
}

# EQUIPMENT_GAINS -> ElectricEquipment
destep_conv_electric_equipment <- function(dest, ep) {
    if (!destep_has_rows(dest, "EQUIPMENT_GAINS")) return(NULL)

    equipment <- DBI::dbGetQuery(
        dest,
        "
        SELECT
            E.GAIN_ID        AS ID,
            E.NAME           AS NAME,
            R.NAME           AS ROOM_NAME,
            S.NAME           AS SCHEDULE_NAME,
            CASE
                WHEN E.PER_AREA = 1 THEN 'Watts/Area'
                ELSE 'EquipmentLevel'
            END              AS METHOD,
            CASE
                WHEN E.PER_AREA != 1 THEN E.MAXPOWER
                ELSE NULL
            END              AS DESIGN_LEVEL,
            CASE
                WHEN E.PER_AREA = 1 THEN E.MAXPOWER
                ELSE NULL
            END              AS WATTS_PER_AREA,
            ROUND(1.0 - DM.DIST_AIR, 3)
                             AS FRACTION_RADIANT
        FROM EQUIPMENT_GAINS E
        LEFT JOIN ROOM R
        ON E.OF_ROOM = R.ID
        LEFT JOIN SCHEDULE_YEAR S
        ON E.SCHEDULE = S.SCHEDULE_ID
        LEFT JOIN DIST_MODE DM
        ON E.DIST_MODE = DM.DIST_MODE_ID
        "
    )
    data.table::setDT(equipment)
    destep_force_numeric(equipment, c(
        "DESIGN_LEVEL", "WATTS_PER_AREA", "FRACTION_RADIANT"
    ))
    watts_per_area_field <- destep_idd_field_name(ep, "ElectricEquipment", 6L)

    out <- eval(as.call(c(
        destep_add, dest, ep,
        lapply(seq_len(nrow(equipment)), function(i) {
            val <- list(
                name = equipment$NAME[[i]],
                zone_or_zonelist_or_space_or_spacelist_name = equipment$ROOM_NAME[[i]],
                schedule_name = equipment$SCHEDULE_NAME[[i]],
                design_level_calculation_method = equipment$METHOD[[i]],
                design_level = if (equipment$METHOD[[i]] == "EquipmentLevel") equipment$DESIGN_LEVEL[[i]],
                watts_per_person = NULL,
                fraction_latent = 0,
                fraction_radiant = equipment$FRACTION_RADIANT[[i]],
                fraction_lost = 0,
                end_use_subcategory = "General"
            )
            val[[watts_per_area_field]] <- if (equipment$METHOD[[i]] == "Watts/Area") equipment$WATTS_PER_AREA[[i]]
            bquote("ElectricEquipment" := .(val))
        })
    )))
    attr(out, "table") <- equipment

    out
}

destep_has_rows <- function(dest, table) {
    table %in% DBI::dbListTables(dest) &&
        DBI::dbGetQuery(dest, sprintf("SELECT COUNT(*) AS N FROM `%s`", table))$N > 0L
}

destep_force_numeric <- function(dt, cols) {
    for (col in cols) {
        data.table::set(dt, NULL, col, as.numeric(dt[[col]]))
    }
    invisible(dt)
}

destep_combine_outputs <- function(outputs, table = NULL) {
    outputs <- Filter(Negate(is.null), outputs)
    if (length(outputs) == 0L) return(NULL)

    num_obj <- 0L
    for (i in seq_along(outputs)) {
        data.table::set(outputs[[i]]$object, NULL, "rleid", outputs[[i]]$object$rleid + num_obj)
        data.table::set(outputs[[i]]$value, NULL, "rleid", outputs[[i]]$value$rleid + num_obj)
        num_obj <- max(outputs[[i]]$object$rleid)
    }

    out <- list(
        object = data.table::rbindlist(lapply(outputs, .subset2, "object")),
        value = data.table::rbindlist(lapply(outputs, .subset2, "value"))
    )

    if (is.null(table)) {
        table <- data.table::rbindlist(
            lapply(names(outputs), function(name) {
                tbl <- attr(outputs[[name]], "table")
                if (is.null(tbl)) return(NULL)
                data.table::set(data.table::copy(tbl), NULL, "SOURCE_TABLE", name)
            }),
            fill = TRUE
        )
    }
    attr(out, "table") <- table

    out
}
