# List every source field that references SCHEDULE_YEAR, including AC_SYS
# supply-temperature fields whose legacy names omit the word SCHEDULE.
schedule__reference_fields <- function(dest, table) {
    checkmate::assert_class(dest, "DBIConnection")
    checkmate::assert_string(table, min.chars = 1L)

    fields <- DBI::dbListFields(dest, table)
    references <- grep("SCHEDULE", fields, value = TRUE)
    if (identical(table, "AC_SYS")) {
        references <- union(
            references,
            intersect(c("SUPPLY_T_MIN", "SUPPLY_T_MAX"), fields)
        )
    }
    references
}

# Convert referenced hourly inputs to date-based schedules in either format.
schedule__convert <- function(dest, ep, format = "compact", directory = NULL) {
    checkmate::assert_choice(format, c("compact", "file"))
    # currently, schedules are used in the tables below:
    # - AC_SYS, including the nonstandard SUPPLY_T_MIN/MAX references
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
    ids_ref <- unique(un_list(
        recursive = TRUE,
        lapply(tbls[tbls != "SCHEDULE_YEAR"], function(tbl) {
            # get the number of rows in the table
            n <- DBI::dbGetQuery(
                dest,
                sprintf("SELECT COUNT(*) as n FROM '%s'", tbl)
            )$n
            if (n > 0L) {
                # get the column names that reference schedules
                col_ref <- schedule__reference_fields(dest, tbl)
                if (length(col_ref) > 0L) {
                    # get the distinct values of the referenced schedules
                    DBI::dbGetQuery(
                        dest,
                        sprintf(
                            "SELECT DISTINCT %s FROM '%s'",
                            paste(col_ref, collapse = ", "),
                            tbl
                        )
                    )
                }
            }
        })
    ))
    # NULL means that the optional reference is not assigned, while zero is
    # DeST's sentinel for a default or unused schedule. Neither value names a
    # SCHEDULE_YEAR row, and retaining NA would emit it as a SQL identifier.
    ids_ref <- ids_ref[!is.na(ids_ref) & ids_ref != 0L]
    if (length(ids_ref) > 0L) {
        schedule <- data.table::setDT(DBI::dbGetQuery(
            dest,
            sprintf(
                paste(
                    "SELECT * FROM SCHEDULE_YEAR",
                    "WHERE SCHEDULE_ID IN (%s) ORDER BY SCHEDULE_ID"
                ),
                paste(ids_ref, collapse = ", ")
            )
        ))
        # This broad scan includes disabled controls and unused room types.
        # Owning converters validate active references; an inactive unresolved
        # reference must not prevent conversion of the schedules actually used.
        if (anyDuplicated(schedule$SCHEDULE_ID)) {
            stop("Duplicate SCHEDULE_YEAR IDs.", call. = FALSE)
        }
        data.table::set(
            schedule,
            NULL,
            "DATA",
            Map(
                schedule__decode,
                schedule$DATA,
                schedule$NAME
            )
        )
    } else {
        schedule <- data.table::data.table()
    }

    # Range ventilation needs a normalized max-minus-min schedule because an
    # EnergyPlus ventilation availability schedule is a fraction, not an ACH
    # value. Generate those hourly values alongside source rows.
    derived <- ventilation__range_schedule_rows(dest)
    schedule <- data.table::rbindlist(
        list(schedule, derived),
        use.names = TRUE,
        fill = TRUE
    )
    schedule <- schedule__scale_relative_humidity(dest, schedule)
    if (nrow(schedule) == 0L) {
        return(NULL)
    }

    schedule__validate(schedule)
    type_limits <- schedule__convert_type_limits(dest, ep, schedule)
    data.table::set(
        type_limits,
        NULL,
        "index",
        data.table::rowid(type_limits$id)
    )
    limits <- c(
        "Fraction",
        "On/Off",
        "Control Method",
        "Any Number",
        "Any Number"
    )[schedule$TYPE]
    files <- character()
    if (format == "compact") {
        fields <- Map(
            schedule__compact_values,
            schedule$DATA,
            schedule$NAME,
            limits
        )
        class <- "Schedule:Compact"
    } else {
        checkmate::assert_choice(format, c("compact", "file"))
        files <- schedule__write_csv(schedule, directory)
        fields <- lapply(seq_len(nrow(schedule)), function(i) {
            # The first nine fields exist in every supported target version.
            values <- c(
                schedule$NAME[[i]],
                limits[[i]],
                files,
                i,
                0L,
                8760L,
                "Comma",
                "No",
                60L
            )
            if (
                numeric_version(as.character(ep$version())) >=
                    numeric_version("22.1")
            ) {
                values <- c(values, "No")
            }
            values
        })
        class <- "Schedule:File"
    }
    # Expand variable-length records in one batch without per-field IDD queries.
    records <- data.table::data.table(
        id = rep(seq_along(fields), lengths(fields)),
        class = class,
        name = rep(schedule$NAME, lengths(fields)),
        index = sequence(lengths(fields)),
        value = unlist(fields, use.names = FALSE)
    )
    out <- conv__combine_outputs(
        list(
            conv__load(dest, ep, type_limits),
            conv__load(dest, ep, records)
        ),
        table = schedule
    )
    attr(out, "files") <- files
    out
}

# Access stores a non-leap year's 8760 IEEE doubles in little-endian order.
# Reject truncated or extra data rather than silently reading only a prefix.
schedule__decode <- function(bytes, name) {
    if (!is.raw(bytes) || length(bytes) != 8760L * 8L) {
        stop(
            "Schedule '",
            name,
            "' must contain exactly 8760 binary doubles.",
            call. = FALSE
        )
    }
    readBin(bytes, what = "double", n = 8760L, size = 8L, endian = "little")
}

# Validate both decoded and derived schedules before formatting or writing files.
schedule__validate <- function(schedule) {
    if (anyNA(schedule$TYPE) || any(!schedule$TYPE %in% 1:5)) {
        stop(
            "Unsupported SCHEDULE_YEAR TYPE; expected 1 through 5.",
            call. = FALSE
        )
    }
    if (
        anyNA(schedule$NAME) ||
            any(!nzchar(schedule$NAME)) ||
            anyDuplicated(tolower(schedule$NAME))
    ) {
        stop("Schedule names must be nonempty and unique.", call. = FALSE)
    }
    valid <- vapply(
        schedule$DATA,
        function(x) {
            is.numeric(x) && length(x) == 8760L && all(is.finite(x))
        },
        logical(1L)
    )
    if (!all(valid)) {
        stop(
            "Schedules require 8760 finite hourly values: ",
            paste(schedule$NAME[!valid], collapse = ", "),
            call. = FALSE
        )
    }
}

# Encode exact runs of equal daily profiles and equal hourly values. Comparison
# uses the original doubles, without rounded hashes or tolerance-based merging.
schedule__compact_values <- function(values, name, limits) {
    days <- matrix(values, nrow = 24L)
    changed <- colSums(
        days[, -1L, drop = FALSE] != days[, -365L, drop = FALSE]
    ) !=
        0L
    starts <- c(1L, which(changed) + 1L)
    ends <- c(starts[-1L] - 1L, 365L)
    dates <- format(as.Date("2001-01-01") + ends - 1L, "%m/%d")
    # Each date block has variable length; allocate its slots once, then flatten.
    blocks <- lapply(seq_along(starts), function(i) {
        day <- days[, starts[[i]]]
        until <- c(which(day[-24L] != day[-1L]), 24L)
        pairs <- as.vector(rbind(
            sprintf("Until: %02d:00", until),
            sprintf("%.17g", day[until])
        ))
        c(
            paste0("Through: ", dates[[i]]),
            "For: AllDays",
            "Interpolate: No",
            pairs
        )
    })
    c(name, limits, unlist(blocks, use.names = FALSE))
}

# Write one persistent CSV, with one column per schedule and no header. Decimal
# strings retain round-trip double precision; a unique filename protects earlier
# conversions. The caller owns these files after successful conversion.
schedule__write_csv <- function(schedule, directory) {
    checkmate::assert_string(directory, min.chars = 1L)
    if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE)) {
        stop("Cannot create schedule_directory: ", directory, call. = FALSE)
    }
    directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
    if (grepl("[,;!\\r\\n]", directory, perl = TRUE)) {
        stop("schedule_directory contains IDF delimiters.", call. = FALSE)
    }
    path <- tempfile("destep-schedules-", tmpdir = directory, fileext = ".csv")
    columns <- lapply(schedule$DATA, sprintf, fmt = "%.17g")
    data.table::fwrite(
        data.table::as.data.table(columns),
        path,
        col.names = FALSE,
        quote = FALSE,
        sep = ",",
        eol = "\n"
    )
    path
}

# Map optional one-based DeST simulation days to a fixed non-leap calendar.
# The inspected database does not establish a saved simulation range, so the
# default is explicitly annual. Neither format depends on the weekday label.
schedule__run_period <- function(ep, days) {
    dates <- as.Date("2001-01-01") + days - 1L
    weekdays <- c(
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
        "Sunday"
    )
    values <- list(
        name = if (identical(as.integer(days), c(1L, 365L))) {
            "Annual"
        } else {
            "DeST Day Range"
        },
        begin_month = as.integer(format(dates[[1L]], "%m")),
        begin_day_of_month = as.integer(format(dates[[1L]], "%d")),
        end_month = as.integer(format(dates[[2L]], "%m")),
        end_day_of_month = as.integer(format(dates[[2L]], "%d")),
        day_of_week_for_start_day = weekdays[[(days[[1L]] - 1L) %% 7L + 1L]],
        use_weather_file_holidays_and_special_days = "No",
        use_weather_file_daylight_saving_period = "No",
        apply_weekend_holiday_rule = "No",
        use_weather_file_rain_indicators = "Yes",
        use_weather_file_snow_indicators = "Yes"
    )
    if (numeric_version(as.character(ep$version())) >= numeric_version("9.0")) {
        values$begin_year <- 2001L
        values$end_year <- 2001L
    }
    do.call(ep$add, list(RunPeriod = values))
    invisible(NULL)
}

# Collect the schedule IDs used specifically as ROOM_GROUP or ROOM_TYPE_DATA
# relative-humidity limits so their fractional DeST values can be converted.
schedule__relative_humidity_ids <- function(dest) {
    tables <- intersect(
        c("ROOM_GROUP", "ROOM_TYPE_DATA"),
        DBI::dbListTables(dest)
    )
    columns <- c("SET_RH_MIN_SCHEDULE", "SET_RH_MAX_SCHEDULE")

    ids <- un_list(lapply(tables, function(table) {
        fields <- intersect(columns, DBI::dbListFields(dest, table))
        if (length(fields) == 0L || !db_has_rows(dest, table)) {
            return(NULL)
        }
        un_list(DBI::dbGetQuery(
            dest,
            sprintf(
                "SELECT DISTINCT %s FROM `%s`",
                paste(sprintf("`%s`", fields), collapse = ", "),
                table
            )
        ))
    }))

    unique(ids[!is.na(ids) & ids != 0L])
}

# Collect every schedule reference except the two humidity-limit fields.
schedule__nonhumidity_reference_ids <- function(dest) {
    ids <- un_list(
        recursive = TRUE,
        lapply(
            setdiff(DBI::dbListTables(dest), "SCHEDULE_YEAR"),
            function(table) {
                if (!db_has_rows(dest, table)) {
                    return(NULL)
                }
                fields <- schedule__reference_fields(dest, table)
                fields <- setdiff(
                    fields,
                    c("SET_RH_MIN_SCHEDULE", "SET_RH_MAX_SCHEDULE")
                )
                if (length(fields) == 0L) {
                    return(NULL)
                }
                DBI::dbGetQuery(
                    dest,
                    sprintf(
                        "SELECT DISTINCT %s FROM `%s`",
                        paste(sprintf("`%s`", fields), collapse = ", "),
                        table
                    )
                )
            }
        )
    )

    unique(ids[!is.na(ids) & ids != 0L])
}

# Return humidity schedules that also need to retain their original unit use.
schedule__shared_relative_humidity_ids <- function(dest) {
    intersect(
        schedule__relative_humidity_ids(dest),
        schedule__nonhumidity_reference_ids(dest)
    )
}

# Build deterministic names for percent copies of shared humidity schedules.
schedule__relative_humidity_name_map <- function(dest) {
    shared_ids <- schedule__shared_relative_humidity_ids(dest)
    if (length(shared_ids) == 0L) {
        return(data.table::data.table())
    }

    source <- data.table::as.data.table(DBI::dbGetQuery(
        dest,
        sprintf(
            paste(
                "SELECT SCHEDULE_ID, NAME FROM SCHEDULE_YEAR",
                "WHERE SCHEDULE_ID IN (%s) ORDER BY SCHEDULE_ID"
            ),
            paste(shared_ids, collapse = ", ")
        )
    ))
    unresolved <- setdiff(shared_ids, source$SCHEDULE_ID)
    if (length(unresolved) > 0L) {
        stop(
            sprintf(
                "Cannot resolve relative-humidity schedule ID(s): [%s].",
                paste(unresolved, collapse = ", ")
            ),
            call. = FALSE
        )
    }

    existing_names <- DBI::dbGetQuery(
        dest,
        "SELECT NAME FROM SCHEDULE_YEAR ORDER BY SCHEDULE_ID"
    )$NAME
    candidates <- paste(source$NAME, "[Relative Humidity Percent]")
    reserved <- make_unique_name(c(existing_names, candidates))
    source[, NAME := utils::tail(reserved, .N)]
    source[]
}

# Substitute derived percent-copy names only for shared humidity references.
schedule__relative_humidity_reference_names <- function(dest, ids, names) {
    mapping <- schedule__relative_humidity_name_map(dest)
    if (nrow(mapping) == 0L) {
        return(names)
    }

    index <- match(ids, mapping$SCHEDULE_ID)
    replace <- !is.na(index)
    names[replace] <- mapping$NAME[index[replace]]
    names
}

# Convert DeST relative-humidity fractions to the percent values required by
# EnergyPlus schedules while preserving all non-humidity schedules unchanged.
schedule__scale_relative_humidity <- function(dest, schedule) {
    humidity_ids <- schedule__relative_humidity_ids(dest)
    if (length(humidity_ids) == 0L) {
        return(schedule)
    }

    rows <- which(schedule$SCHEDULE_ID %in% humidity_ids)
    unresolved <- setdiff(humidity_ids, schedule$SCHEDULE_ID[rows])
    if (length(unresolved) > 0L) {
        stop(
            sprintf(
                "Cannot resolve relative-humidity schedule ID(s): [%s].",
                paste(unresolved, collapse = ", ")
            ),
            call. = FALSE
        )
    }

    for (row in rows) {
        values <- schedule$DATA[[row]]
        # H0 froze DeST's supported representation as a 0--1 fraction. Reject
        # other encodings instead of guessing whether they are already percent.
        if (
            length(values) != 8760L ||
                any(!is.finite(values)) ||
                any(values < 0 | values > 1)
        ) {
            stop(
                sprintf(
                    paste(
                        "Relative-humidity schedule %s must contain 8760 finite",
                        "DeST fraction values in [0, 1]."
                    ),
                    schedule$SCHEDULE_ID[[row]]
                ),
                call. = FALSE
            )
        }
    }

    schedule__assert_relative_humidity_bounds(dest, schedule)
    # Scaling changes target units; keep the caller's decoded source untouched.
    schedule <- data.table::copy(schedule)
    shared_ids <- schedule__shared_relative_humidity_ids(dest)
    direct_rows <- which(
        schedule$SCHEDULE_ID %in% setdiff(humidity_ids, shared_ids)
    )
    for (row in direct_rows) {
        values <- schedule$DATA[[row]]
        data.table::set(schedule, row, "DATA", list(list(values * 100)))
    }

    # Percent RH must not retain Fraction or On/Off limits of 0--1. Values
    # have already passed the physical 0--100 percent check above.
    data.table::set(schedule, direct_rows, "TYPE", 4L)

    # A shared DeST schedule has two physical roles with different units.
    # Preserve its original values and add a percent copy for Humidistat.
    mapping <- schedule__relative_humidity_name_map(dest)
    if (nrow(mapping) > 0L) {
        duplicate_rows <- match(mapping$SCHEDULE_ID, schedule$SCHEDULE_ID)
        duplicate <- data.table::copy(schedule[duplicate_rows])
        first_id <- min(c(0L, schedule$SCHEDULE_ID), na.rm = TRUE) -
            nrow(duplicate)
        data.table::set(
            duplicate,
            NULL,
            "SCHEDULE_ID",
            seq.int(
                first_id,
                length.out = nrow(duplicate)
            )
        )
        data.table::set(duplicate, NULL, "NAME", mapping$NAME)
        data.table::set(
            duplicate,
            NULL,
            "DATA",
            lapply(
                duplicate$DATA,
                function(values) values * 100
            )
        )
        data.table::set(duplicate, NULL, "TYPE", 4L)
        schedule <- data.table::rbindlist(
            list(schedule, duplicate),
            use.names = TRUE,
            fill = TRUE
        )
    }

    schedule
}

# Check every complete ROOM_GROUP or ROOM_TYPE_DATA humidity pair hour by hour
# before scaling; an inverted lower/upper bound is invalid in both simulators.
schedule__assert_relative_humidity_bounds <- function(dest, schedule) {
    required <- c("SET_RH_MIN_SCHEDULE", "SET_RH_MAX_SCHEDULE")
    tables <- intersect(
        c("ROOM_GROUP", "ROOM_TYPE_DATA"),
        DBI::dbListTables(dest)
    )
    pairs <- data.table::rbindlist(lapply(tables, function(table) {
        fields <- DBI::dbListFields(dest, table)
        if (!all(required %in% fields) || !db_has_rows(dest, table)) {
            return(NULL)
        }
        data.table::as.data.table(DBI::dbGetQuery(
            dest,
            sprintf(
                paste(
                    "SELECT DISTINCT SET_RH_MIN_SCHEDULE AS MIN_ID,",
                    "SET_RH_MAX_SCHEDULE AS MAX_ID FROM `%s`"
                ),
                table
            )
        ))
    }))
    if (nrow(pairs) == 0L) {
        return(invisible(NULL))
    }
    pairs <- unique(pairs)
    keep <- !is.na(pairs$MIN_ID) &
        pairs$MIN_ID != 0L &
        !is.na(pairs$MAX_ID) &
        pairs$MAX_ID != 0L
    pairs <- pairs[keep]
    for (i in seq_len(nrow(pairs))) {
        min_values <- schedule$DATA[[match(
            pairs$MIN_ID[[i]],
            schedule$SCHEDULE_ID
        )]]
        max_values <- schedule$DATA[[match(
            pairs$MAX_ID[[i]],
            schedule$SCHEDULE_ID
        )]]
        inverted <- which(min_values > max_values)
        if (length(inverted) > 0L) {
            stop(
                sprintf(
                    paste(
                        "Relative-humidity lower schedule %s exceeds upper",
                        "schedule %s at DeST hour index %s."
                    ),
                    pairs$MIN_ID[[i]],
                    pairs$MAX_ID[[i]],
                    inverted[[1L]] - 1L
                ),
                call. = FALSE
            )
        }
    }

    invisible(NULL)
}

# Retain the DeST schedule type checks in the target object references.
schedule__convert_type_limits <- function(dest, ep, schedule) {
    types <- schedule$TYPE
    types[types == 5L] <- 4L
    types <- unique.default(types)
    type_limits <- data.table::fcase(
        types == 1L                                                  ,
        list(list("Fraction", 0, 1, "Continuous"))                   ,
        types == 2L                                                  ,
        list(list("On/Off", 0, 1, "Discrete"))                       ,
        types == 3L                                                  ,
        list(list("Control Method", NA_real_, NA_real_, "Discrete")) ,
        types %in% c(4L, 5L)                                         ,
        list(list("Any Number", NA_real_, NA_real_, "Continuous"))
    )
    type_limits <- data.table::rbindlist(type_limits)
    data.table::setnames(
        type_limits,
        c("Name", "Lower Limit Value", "Upper Limit Value", "Numeric Type")
    )
    data.table::set(type_limits, NULL, "Unit Type", "Dimensionless")
    data.table::set(type_limits, NULL, "id", types)
    data.table::set(type_limits, NULL, "class", "ScheduleTypeLimits")
    data.table::set(type_limits, NULL, "name", type_limits$Name)

    type_limits <- data.table::set(
        eplusr::dt_to_load(type_limits),
        NULL,
        "field",
        NULL
    )
    data.table::setcolorder(type_limits, c("id", "class", "name", "value"))

    type_limits
}
