ENUM_SCH_DAYTYPE <- c(
    Sunday = 1L, Monday = 2L, Tuesday = 3L, Wednesday = 4L, Thursday = 5L, Friday = 6L, Saturday = 7L,
    Holiday = 8L, SummerDesignDay = 9L, WinterDesignDay = 10L,
    CustomDay1 = 11L, CustomDay2 = 12L,
    Weekday = 13L, Weekend = 14L, AllDay = 15L, AllOtherDay = 16L
)
ENUM_SCH_DAYTYPE_NORMAL <- ENUM_SCH_DAYTYPE[ENUM_SCH_DAYTYPE <= ENUM_SCH_DAYTYPE["CustomDay2"]]
ENUM_SCH_DAYTYPE_SPECIAL <- ENUM_SCH_DAYTYPE[ENUM_SCH_DAYTYPE > ENUM_SCH_DAYTYPE["CustomDay2"]]
ENUM_SCH_DAYTYPE_WEEKEND <- c(ENUM_SCH_DAYTYPE["Saturday"], ENUM_SCH_DAYTYPE["Sunday"])
ENUM_SCH_DAYTYPE_WEEKDAY <- ENUM_SCH_DAYTYPE[ENUM_SCH_DAYTYPE["Monday"]:ENUM_SCH_DAYTYPE["Friday"]]

# SCHEDULE_YEAR -> Schedule:Year -> Schedule:Week:Compact -> Schedule:Day:Interval -> ScheduleTypeLimits
destep_conv_schedule <- function(dest, ep) {
    # currently, schedules are used in the tables below:
    # - DOOR
    # - ENERGY_DEVICE
    # - ENERGY_HOTWATER
    # - ENERGY_LIFT_ESCALATOR
    # - ENERGY_PUMP_FAN
    # - ENERGY_PUMP_GROUP
    # - EQUIPMENT_GAINS
    # - HEATING_PIPE
    # - HEATING_SYSTEM
    # - LIGHT_GAINS
    # - OCCUPANT_GAINS
    # - ROOM
    # - ROOM_GROUP
    # - ROOM_TYPE_DATA
    # - WINDOW

    # get all schedules that are used in the model
    tbls <- DBI::dbListTables(dest)

    # find tables that reference schedules
    ids_ref <- unique(un_list(recursive = TRUE, lapply(tbls[tbls != "SCHEDULE_YEAR"], function(tbl) {
        # get the number of rows in the table
        n <- DBI::dbGetQuery(dest, sprintf("SELECT COUNT(*) as n FROM '%s'", tbl))$n
        if (n > 0L) {
            # get the column names that reference schedules
            col_ref <- grep("SCHEDULE", DBI::dbListFields(dest, tbl), value = TRUE)
            if (length(col_ref) > 0L) {
                # get the distinct values of the referenced schedules
                DBI::dbGetQuery(dest, sprintf("SELECT DISTINCT %s FROM '%s'", paste(col_ref, collapse = ", "), tbl))
            }
        }
    })))
    # TODO: what is the value of the default schedule with ID = 0 for WINDOW table?
    ids_ref <- ids_ref[ids_ref != 0L]

    schedule <- data.table::setDT(DBI::dbGetQuery(
        dest,
        sprintf("SELECT * FROM SCHEDULE_YEAR WHERE SCHEDULE_ID IN (%s)", paste(ids_ref, collapse = ", "))
    ))
    # In DeST, the actual schedule data is stored as doubles in a raw vector
    data.table::set(schedule, NULL, "DATA", lapply(schedule[["DATA"]], readBin, what = "double", n = 8760L))

    type_limits <- destep_conv_schedule_type_limits(dest, ep, schedule)
    days <- destep_conv_schedule_day(dest, ep, schedule, type_limits)
    weeks <- destep_conv_schedule_week(dest, ep, schedule, type_limits, days)
    years <- destep_conv_schedule_year(dest, ep, schedule, type_limits, weeks)

    # combine all data and load
    data.table::set(type_limits, NULL, "id", data.table::rleid(type_limits$id))
    data.table::set(days$data, NULL, "id", data.table::rleid(days$data$id) + type_limits$id[nrow(type_limits)])
    data.table::set(weeks$data, NULL, "id", data.table::rleid(weeks$data$id) + days$data$id[nrow(days$data)])
    data.table::set(years, NULL, "id", data.table::rleid(years$id) + weeks$data$id[nrow(weeks$data)])
    out <- destep_load(
        dest, ep,
        data.table::rbindlist(list(type_limits, days$data, weeks$data, years), use.names = TRUE)
    )

    # always attach the table to the output in case it is useful later
    attr(out, "table") <- schedule

    out
}

destep_conv_schedule_type_limits <- function(dest, ep, schedule) {
    types <- schedule$TYPE
    types[types == 5L] <- 4L
    types <- unique.default(types)
    type_limits <- data.table::fcase(
        types == 1L,
        list(list("Fraction", 0, 1, "Continuous")),
        types == 2L,
        list(list("On/Off", 0, 1, "Discrete")),
        types == 3L,
        list(list("Control Method", NA_real_, NA_real_, "Discrete")),
        types %in% c(4L, 5L),
        list(list("Any Number", NA_real_, NA_real_, "Continuous"))
    )
    type_limits <- data.table::rbindlist(type_limits)
    data.table::setnames(type_limits, c("Name", "Lower Limit Value", "Upper Limit Value", "Numeric Type"))
    data.table::set(type_limits, NULL, "Unit Type", "Dimensionless")
    data.table::set(type_limits, NULL, "id", types)
    data.table::set(type_limits, NULL, "class", "ScheduleTypeLimits")
    data.table::set(type_limits, NULL, "name", type_limits$Name)

    type_limits <- data.table::set(eplusr::dt_to_load(type_limits), NULL, "field", NULL)
    data.table::setcolorder(type_limits, c("id", "class", "name", "value"))

    type_limits
}

destep_conv_schedule_day <- function(dest, ep, schedule, type_limits, prefix = "Day-") {
    # the number of schedules to extract
    num_sch <- nrow(schedule)

    # combine all yearly schedule data into a single vector
    full_value <- data.table::data.table(
        rleid = rep(1L:(365L * num_sch), each = 24L),
        type = rep(schedule$TYPE, each = 365L * 24L),
        until = rep((1L:24L) * 60L, 365L * num_sch),
        value = un_list(schedule$DATA)
    )
    day_value <- unique_value(full_value)

    map_week <- data.table::setDT(attr(day_value, "map"))

    # compress the value
    grp <- collapse::groupv(day_value$rleid)
    changed <- collapse::fdiff.default(day_value$value, g = grp, fill = 1.0) != 0.0
    changed <- collapse::flag.default(changed, n = -1L, g = grp)
    collapse::replace_na(changed, TRUE, set = TRUE)
    day_value <- collapse::ss(day_value, changed)

    grp_day <- collapse::groupv(map_week$index_cur, starts = TRUE)

    # make unique names for each day schedule
    day_name <- rep(schedule$NAME, 365L * 24L)[
        map_week$index_ori[attr(grp_day, "starts", exact = TRUE)]
    ]
    day_name <- paste0(prefix, make_unique_name(day_name))

    type_day <- collapse::funique.data.frame(collapse::ss(day_value, j = c("rleid", "type")))
    data.table::setnames(type_day, "rleid", "id")

    grp_fld <- collapse::groupv(day_value$rleid, group.size = TRUE)
    num_val <- attr(grp_fld, "group.sizes", exact = TRUE)
    # *2 for time and value pairs
    # +3 for name, type limits, interpolate
    num_fld <- num_val * 2L + 3L

    sch_day <- destep_field(dest, ep, "Schedule:Day:Interval", num_fld)
    data.table::set(sch_day, NULL, "name", rep(day_name, num_fld))

    # field 1: name
    data.table::set(sch_day, collapse::whichv(sch_day$index, 1L), "value", day_name)

    # field 2: schedule type limits name
    data.table::set(
        sch_day, collapse::whichv(sch_day$index, 2L), "value",
        type_limits$name[collapse::fmatch(type_day$type, type_limits$id)]
    )

    # field 3: interpolate to timestep
    data.table::set(sch_day, collapse::whichv(sch_day$index, 3L), "value", "No")

    # field-sets
    data.table::set(
        sch_day, which(sch_day$index > 3L), "value",
        c(format_time(day_value$until), as.character(day_value$value))[
            collapse::radixorderv(rep(seq_along(day_value$until), 2L))
        ]
    )

    list(map = map_week, data = sch_day)
}

destep_conv_schedule_week <- function(dest, ep, schedule, type_limits, days, prefix = "Week-") {
    num_sch <- nrow(schedule)
    map <- data.table::copy(days$map)

    data.table::set(map, NULL, "rleid", rep(seq_len(num_sch), each = 365L))
    data.table::set(map, NULL, "week", rep(c(rep(1L:52L, each = 7L), 53L), num_sch))
    data.table::set(map, NULL, "rleid_week", rep((seq_len(num_sch) - 1L) * 53L, each = 365L) + map$week)

    # find the first monday that is the same as the one in the 53rd week
    ind_53 <- collapse::whichv(map$week, 53L)
    mth_53 <- collapse::fmatch(
        list(rleid = map$rleid[ind_53], index_cur = map$index_cur[ind_53]),
        list(rleid = map$rleid[-ind_53], index_cur = map$index_cur[-ind_53]),
        nomatch = 0L
    )

    # handle the case that there is no match for the 53rd week
    # in this case, we have to create a didicated week schedule for the 53rd
    # week
    if (any(mth_53 == 0L)) {
        stop("Not implemented yet")
    }

    week_53 <- collapse::ss(map, ind_53, c("index_ori", "rleid", "week", "rleid_week"))
    data.table::set(week_53, NULL, "week", map$week[-ind_53][mth_53])
    map <- data.table::rbindlist(list(
        collapse::ss(map, -ind_53),
        data.table::set(collapse::join(
            collapse::ss(map, j = c("index_cur", "rleid", "week"), check = FALSE),
            week_53,
            on = c("rleid", "week"), how = "right",
            verbose = FALSE, multiple = TRUE
        ), NULL, "week", 53L)
    ), use.names = TRUE)

    spl_week <- collapse::gsplit(map$index_cur, map$rleid_week)
    grp_week <- collapse::group(data.table::transpose(spl_week))
    grp_rleid <- collapse::groupv(rep(seq_len(num_sch), each = 53L))
    changed <- collapse::fdiff.default(grp_week, g = grp_rleid, fill = 1.0) != 0.0
    changed <- collapse::flag.default(changed, n = -1L, g = grp_rleid)
    collapse::replace_na(changed, TRUE, set = TRUE)

    week_name <- paste0(prefix, make_unique_name(rep(schedule$NAME, each = 53L)[changed]))

    # create the day types for each week schedule
    week_daytype <- collapse::ss(
        map,
        collapse::fmatch(map$rleid_week, which(changed), nomatch = 0L) != 0L,
        c("rleid_week", "index_cur"),
        check = FALSE
    )
    data.table::set(week_daytype, NULL, "daytype", rep(c(2L:7L, 1L), sum(changed)))
    data.table::setnames(week_daytype, "index_cur", "rleid_day")
    # NOTE: In DeST, there is no "SummerDesignDay", "WinterDesignDay",
    #       "Holiday", "CustomDay1" and "CustomDay2". So for "SummerDesignDay"
    #       and "WinterDesignDay", we use the value for "Monday". For
    #       "Holiday", "CustomDay1" and "CustomDay2", we use the value for
    #       "Sunday".
    # extract the value for "Monday"
    week_daytype_mon <- collapse::ss(
        week_daytype,
        collapse::whichv(week_daytype$daytype, ENUM_SCH_DAYTYPE[["Monday"]]),
        check = FALSE
    )
    week_daytype_designday <- data.table::rbindlist(list(
        data.table::set(data.table::copy(week_daytype_mon), NULL, "daytype", ENUM_SCH_DAYTYPE[["SummerDesignDay"]]),
        data.table::set(week_daytype_mon, NULL, "daytype", ENUM_SCH_DAYTYPE[["WinterDesignDay"]])
    ))
    # extract the value for "Sunday"
    week_daytype_sun <- collapse::ss(
        week_daytype,
        collapse::whichv(week_daytype$daytype, ENUM_SCH_DAYTYPE[["Sunday"]]),
        check = FALSE
    )
    week_daytype_specialday <- data.table::rbindlist(list(
        data.table::set(data.table::copy(week_daytype_sun), NULL, "daytype", ENUM_SCH_DAYTYPE[["Holiday"]]),
        data.table::set(data.table::copy(week_daytype_sun), NULL, "daytype", ENUM_SCH_DAYTYPE[["CustomDay1"]]),
        data.table::set(week_daytype_sun, NULL, "daytype", ENUM_SCH_DAYTYPE[["CustomDay2"]])
    ))
    # combine all
    week_daytype <- data.table::rbindlist(list(
        week_daytype,
        week_daytype_designday,
        week_daytype_specialday
    ))
    data.table::setorderv(week_daytype, c("rleid_week", "daytype"))
    data.table::setnames(week_daytype, "rleid_week", "rleid")

    # compress the day types
    other_days <- ENUM_SCH_DAYTYPE[c("Holiday", "CustomDay1", "CustomDay2")]
    grp_rleid <- collapse::groupv(week_daytype$rleid, starts = TRUE)

    pairs <- .mapply(
        function(rleid, daytypes, days) {
            # group by day schedules
            grp_days <- collapse::groupv(days, starts = TRUE, group.sizes = TRUE)
            rleid_day <- days[attr(grp_days, "starts", exact = TRUE)]
            len_day <- length(rleid_day)

            # if all day schedules are the same, return "AllDay"
            if (attr(grp_days, "N.groups", exact = TRUE) == 1L) {
                return(list(
                    rleid = rep(rleid, len_day),
                    daytype = rep(ENUM_SCH_DAYTYPE[["AllDay"]], len_day),
                    rleid_day = rleid_day
                ))
            }

            compacted <- lapply(
                collapse::gsplit(daytypes, grp_days),
                function(daytypes) {
                    out <- integer(0L)

                    m_weekday <- collapse::fmatch(ENUM_SCH_DAYTYPE_WEEKDAY, daytypes, 0L)
                    if (sum(m_weekday != 0L) == length(ENUM_SCH_DAYTYPE_WEEKDAY)) {
                        out <- c(ENUM_SCH_DAYTYPE[["Weekday"]])
                        daytypes <- daytypes[-m_weekday]
                    }

                    m_weekend <- collapse::fmatch(ENUM_SCH_DAYTYPE_WEEKEND, daytypes, 0L)
                    if (sum(m_weekend != 0L) == length(ENUM_SCH_DAYTYPE_WEEKEND)) {
                        out <- c(out, ENUM_SCH_DAYTYPE[["Weekend"]])
                        daytypes <- daytypes[-m_weekend]
                    }

                    c(out, daytypes)
                }
            )

            if (length(other_days) > 0L) {
                num_others <- vapply(compacted, function(daytypes) {
                    sum(collapse::fmatch(daytypes, other_days, 0L) != 0L)
                }, integer(1L))
                ind_others <- collapse::radixorderv(num_others, decreasing = TRUE)[num_others > 0L]
                if (length(ind_others) > 0L) {
                    ind_others <- ind_others[[1L]]
                    compacted[[ind_others]] <- c(
                        # only keep the special daytypes
                        compacted[[ind_others]][
                            collapse::fmatch(compacted[[ind_others]], ENUM_SCH_DAYTYPE_SPECIAL, 0L) != 0L
                        ],
                        ENUM_SCH_DAYTYPE[["AllOtherDay"]]
                    )
                }
            }

            len_daytype <- collapse::vlengths(compacted, use.names = FALSE)
            list(
                rleid = rep(rleid, sum(len_daytype)),
                daytype = un_list(compacted),
                rleid_day = rep(rleid_day, len_daytype)
            )
        },
        list(
            rleid = week_daytype$rleid[attr(grp_rleid, "starts", exact = TRUE)],
            daytypes = collapse::gsplit(week_daytype$daytype, grp_rleid),
            days = collapse::gsplit(week_daytype$rleid_day, grp_rleid)
        ),
        NULL
    )
    week_daytype <- data.table::rbindlist(pairs)

    grp_day <- collapse::group(
        list(week_daytype$rleid, week_daytype$rleid_day),
        group.sizes = TRUE, starts = TRUE
    )
    num_fld <- attr(collapse::groupv(
        week_daytype$rleid[attr(grp_day, "starts", exact = TRUE)],
        group.sizes = TRUE
    ), "group.sizes", exact = TRUE)
    num_fld <- num_fld * 2L + 1L

    fld_daytype <- vapply(
        collapse::gsplit(names(ENUM_SCH_DAYTYPE)[week_daytype$daytype], grp_day),
        paste,
        collapse = " ", FUN.VALUE = character(1L)
    )
    fld_daytype <- paste("For:", fld_daytype)

    fld_day <- days$name[
        collapse::fmatch(
            week_daytype$rleid_day[attr(grp_day, "starts", exact = TRUE)],
            days$id
        )
    ]

    sch_week <- destep_field(dest, ep, "Schedule:Week:Compact", num_fld)
    data.table::set(sch_week, NULL, "name", rep(week_name, num_fld))
    # keep the original rleid
    data.table::set(sch_week, NULL, "id", rep(unique(week_daytype$rleid), num_fld))

    # field 1: name
    data.table::set(sch_week, collapse::whichv(sch_week$index, 1L), "value", week_name)

    data.table::set(
        sch_week, which(sch_week$index > 1L), "value",
        c(fld_daytype, fld_day)[collapse::radixorderv(rep(seq_along(fld_daytype), 2L))]
    )

    list(map = map, changed = changed, data = sch_week)
}

destep_conv_schedule_year <- function(dest, ep, schedule, type_limits, weeks) {
    num_sch <- nrow(schedule)

    grp_rleid <- collapse::groupv(rep(seq_len(num_sch), each = 53L))

    year_span <- data.table::data.table(
        rleid = grp_rleid[weeks$changed],
        ordinal = rep(1L:53L, num_sch)[weeks$changed] * 7L,
        rleid_week = seq(1L, 53L * num_sch)[weeks$changed]
    )
    ind_371 <- collapse::whichv(year_span$ordinal, 371L)
    if (length(ind_371) > 0L) {
        data.table::set(year_span, ind_371, "ordinal", 365L)
    }

    # get the start and end date of each span
    grp_span <- collapse::groupv(year_span$rleid, starts = TRUE, group.sizes = TRUE)
    year_span_start <- collapse::flag.default(year_span$ordinal, n = 1L, g = grp_span, fill = NA_integer_) + 1L
    collapse::replace_na(year_span_start, 1L, set = TRUE)
    year_span_start <- lubridate::make_date(2025L, 12L, 31L) + lubridate::days(year_span_start)
    year_span_end <- lubridate::make_date(2025L, 12L, 31L) + lubridate::days(year_span$ordinal)
    data.table::set(
        year_span, NULL,
        c("start_month", "start_day", "end_month", "end_day"),
        list(
            as.integer(lubridate::month(year_span_start)),
            lubridate::mday(year_span_start),
            as.integer(lubridate::month(year_span_end)),
            lubridate::mday(year_span_end)
        )
    )

    # make unique names for each year schedule
    year_name <- make_unique_name(schedule$NAME)

    grp_week <- collapse::groupv(year_span$rleid, group.sizes = TRUE, starts = TRUE)
    num_fld <- attr(grp_week, "group.sizes", exact = TRUE)
    # *5 for week, start month, start day, end month, end day
    # +2 for name, type limits
    num_fld <- num_fld * 5L + 2L

    sch_year <- destep_field(dest, ep, "Schedule:Year", num_fld)
    data.table::set(sch_year, NULL, "name", rep(year_name, num_fld))

    # field 1: name
    data.table::set(sch_year, collapse::whichv(sch_year$index, 1L), "value", year_name)

    # field 2: schedule type limits name
    data.table::set(
        sch_year, collapse::whichv(sch_year$index, 2L), "value",
        type_limits$name[collapse::fmatch(schedule$TYPE, type_limits$id)]
    )

    # field-sets
    data.table::set(
        sch_year, which(sch_year$index > 2L), "value",
        c(
            weeks$data$name[collapse::fmatch(year_span$rleid_week, weeks$data$id)],
            as.character(year_span$start_month),
            as.character(year_span$start_day),
            as.character(year_span$end_month),
            as.character(year_span$end_day)
        )[collapse::radixorderv(rep(seq_along(year_span$rleid_week), 5L))]
    )

    sch_year
}

assert_unique_name <- function(names, type) {
    if (anyDuplicated(names)) {
        stop(sprintf(
            "Duplicated %s names found: [%s]. This should already be handled when updating the names.",
            type, paste(unique(names[duplicated(names)]), collapse = ", ")
        ))
    }
}

make_unique_name <- function(name) {
    spl_name <- collapse::gsplit(name, name)
    spl_name <- .mapply(
        function(name, len) if (len == 1L) name else sprintf("%s (%d)", name, seq_len(len)),
        list(name = spl_name, len = collapse::vlengths(spl_name)),
        NULL
    )
    collapse::greorder(un_list(spl_name), name)
}

unique_value <- function(value, cols = NULL, full = TRUE) {
    len <- collapse::groupv(value$rleid, starts = TRUE, group.sizes = TRUE)
    grp_len <- collapse::groupv(attr(len, "group.sizes", exact = TRUE))
    if (is.null(cols)) {
        # all columns except rleid
        col_data <- colnames(value)[-1L]
    } else {
        col_data <- cols
    }

    args <- c(
        list(index = collapse::gsplit(collapse::ss(value$rleid, attr(len, "starts")), grp_len)),
        data.table::setattr(
            lapply(col_data, function(col) {
                collapse::gsplit(collapse::gsplit(value[[col]], len), grp_len)
            }),
            "names",
            col_data
        )
    )
    paired <- .mapply(
        function(...) {
            input <- list(...)
            # transpose and combine
            trans <- lapply(input[-1L], data.table::transpose)
            pair <- un_list(trans)

            # get the group id
            grp <- collapse::groupv(pair, starts = TRUE)

            # use 'fsubset.data.frame' instead of 'funique.data.frame' since we
            # already have the group info
            pair <- collapse:::fsubset.data.frame(pair, attr(grp, "starts"))

            n_grp <- attr(grp, "N.groups", exact = TRUE)
            out <- list(
                index = input$index,
                group = grp,
                n_grp = n_grp
            )

            if (full) {
                out$rleid <- rep(seq_len(n_grp), each = length(trans[[2L]]))

                offset <- 0L
                for (col in col_data) {
                    out[[col]] <- un_list(data.table::transpose(
                        collapse::fsubset.default(pair, seq_along(trans[[col]]) + offset)
                    ))
                    offset <- offset + length(trans[[col]])
                }
            }
            out
        },
        args,
        NULL
    )

    offset <- c(
        0L,
        # accumulate the group size except the last
        cumsum(vapply(paired, .subset2, integer(1L), "n_grp")[-length(paired)])
    )

    map <- data.table::data.table(
        index_ori = un_list(lapply(paired, .subset2, "index")),
        index_cur = un_list(.mapply(
            function(group, offset) group + offset,
            list(group = lapply(paired, .subset2, "group"), offset = offset),
            NULL
        ))
    )

    if (!full) {
        return(map)
    }

    value <- data.table::setDT(c(
        list(
            rleid = un_list(.mapply(
                function(rleid, offset) rleid + offset,
                list(rleid = lapply(paired, .subset2, "rleid"), offset = offset),
                NULL
            ))
        ),
        data.table::setattr(lapply(col_data, function(col) {
            un_list(lapply(paired, .subset2, col))
        }), "names", col_data)
    ))
    data.table::setattr(value, "map", map)

    value
}

un_list <- function(lst, recursive = FALSE, use.names = FALSE) {
    unlist(lst, recursive = recursive, use.names = use.names)
}

format_time <- function(x) {
    hours <- x %/% 60L
    mins <- x - hours * 60L
    sprintf("%02i:%02i", hours, mins)
}
