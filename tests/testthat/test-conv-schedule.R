test_that("Compact merges identical consecutive days and equal hourly intervals", {
    values <- rep(c(rep(0, 8L), rep(1, 8L), rep(0, 8L)), 365L)
    fields <- schedule__compact_values(values, "daily", "Fraction")
    expect_equal(
        fields,
        c(
            "daily",
            "Fraction",
            "Through: 12/31",
            "For: AllDays",
            "Interpolate: No",
            "Until: 08:00",
            "0",
            "Until: 16:00",
            "1",
            "Until: 24:00",
            "0"
        )
    )
    expect_identical(destep_test_expand_compact(fields), values)
    constant <- schedule__compact_values(
        rep(0.35, 8760L),
        "constant",
        "Any Number"
    )
    expect_length(constant, 7L)
    expect_identical(destep_test_expand_compact(constant), rep(0.35, 8760L))
})

test_that("Compact preserves irregular profiles and tiny differences across dates", {
    set.seed(42)
    cases <- list(
        rep(as.double(1:365), each = 24L),
        rep(c(rep(1, 120L), rep(0, 48L)), length.out = 8760L),
        runif(8760L),
        replace(
            rep(1, 8760L),
            c(24L, 31L * 24L + 1L, 8760L),
            c(1 + .Machine$double.eps, 1 - .Machine$double.eps, 2)
        )
    )
    for (values in cases) {
        fields <- schedule__compact_values(values, "irregular", "Any Number")
        # R's decimal parser can differ by a few floating-point ulps; grouping
        # itself is exact, and serialization retains 17 significant digits.
        expect_lte(
            max(abs(destep_test_expand_compact(fields) - values)),
            2 * .Machine$double.eps * max(1, max(abs(values)))
        )
        expect_false(any(grepl("Weekday|Weekend|Monday", fields)))
    }
    # A run crossing January/February is represented by one end date.
    fields <- schedule__compact_values(
        c(rep(1, 40L * 24L), rep(2, 325L * 24L)),
        "cross-month",
        "Any Number"
    )
    expect_equal(
        fields[startsWith(fields, "Through:")],
        c("Through: 02/09", "Through: 12/31")
    )
})

test_that("binary schedules reject missing, nonfinite and malformed input", {
    ep <- eplusr::empty_idf(23.1)
    dest <- destep_test_schedule_db(list(rep(1, 8760L)))
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    for (values in list(
        numeric(),
        rep(1, 24L),
        rep(1, 8784L),
        rep(NA_real_, 8760L),
        replace(rep(1, 8760L), 10L, NaN),
        replace(rep(1, 8760L), 10L, Inf)
    )) {
        DBI::dbExecute(
            dest,
            "UPDATE SCHEDULE_YEAR SET DATA = ?",
            params = list(list(destep_test_schedule_blob(values)))
        )
        expect_error(schedule__convert(dest, ep), "8760")
    }
})

test_that("File retains full precision and never overwrites an earlier conversion", {
    set.seed(43)
    values <- list(runif(8760L), rep(0.35, 8760L))
    dest <- destep_test_schedule_db(values)
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    ep <- eplusr::empty_idf(23.1)
    directory <- withr::local_tempdir()
    out <- schedule__convert(dest, ep, "file", directory)
    path <- attr(out, "files")
    data <- read.csv(path, header = FALSE)
    expect_lte(max(abs(data[[1L]] - values[[1L]])), 2 * .Machine$double.eps)
    expect_identical(data[[2L]], values[[2L]])
    expect_identical(
        readLines(path),
        paste(
            sprintf("%.17g", values[[1L]]),
            sprintf("%.17g", values[[2L]]),
            sep = ","
        )
    )
    expect_equal(nrow(data), 8760L)
    expect_equal(
        out$value$value_num[out$value$field_name == "Column Number"],
        c(1, 2)
    )
    expect_true(all(
        out$value$value_chr[out$value$field_name == "File Name"] == path
    ))
    expect_equal(
        out$value$value_chr[
            out$value$field_name == "Adjust Schedule for Daylight Savings"
        ],
        c("No", "No")
    )
    again <- schedule__convert(dest, ep, "file", directory)
    expect_false(identical(attr(again, "files"), path))
    expect_true(file.exists(path))
})

test_that("unused references are ignored while duplicate identities and invalid types fail", {
    ep <- eplusr::empty_idf(23.1)
    dest <- destep_test_schedule_db(list(rep(1, 8760L)))
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    DBI::dbExecute(dest, "UPDATE SCHEDULE_USAGE SET SCHEDULE_ID = 99")
    expect_null(schedule__convert(dest, ep))
    DBI::dbExecute(dest, "UPDATE SCHEDULE_USAGE SET SCHEDULE_ID = 1")
    DBI::dbExecute(dest, "UPDATE SCHEDULE_YEAR SET TYPE = 99")
    expect_error(schedule__convert(dest, ep), "TYPE")
    DBI::dbExecute(dest, "UPDATE SCHEDULE_YEAR SET TYPE = 4")
    DBI::dbExecute(
        dest,
        "INSERT INTO SCHEDULE_YEAR SELECT * FROM SCHEDULE_YEAR"
    )
    expect_error(schedule__convert(dest, ep), "Duplicate")
})

test_that("schedule options validate format, files and inclusive source days", {
    expect_identical(destep_opts()$schedule_format, "compact")
    expect_error(destep_opts(schedule_format = "other"), "schedule_format")
    expect_error(destep_opts(schedule_format = "file"), "schedule_directory")
    expect_error(destep_opts(schedule_directory = "unused"), "requires")
    for (days in list(
        c(0, 365),
        c(1, 366),
        c(10, 9),
        c(1.5, 3),
        NA,
        c(1, NA)
    )) {
        expect_error(destep_opts(run_period = days))
    }
    opts <- destep_opts(run_period = c(59, 61))
    ep <- eplusr::empty_idf(23.1)
    schedule__run_period(ep, opts$run_period)
    tab <- ep$to_table(class = "RunPeriod")
    expect_equal(
        as.numeric(tab$value[
            tab$field %in%
                c(
                    "Begin Month",
                    "Begin Day of Month",
                    "End Month",
                    "End Day of Month"
                )
        ]),
        c(2, 28, 3, 2)
    )
    expect_equal(
        tab$value[tab$field == "Use Weather File Daylight Saving Period"],
        "No"
    )
    expect_equal(tab$value[tab$field == "Begin Year"], "2001")
})

test_that("File and run periods respect the older target schema", {
    skip_if_not("9.0.1" %in% eplusr::avail_eplus())
    ep <- eplusr::empty_idf("9.0.1")
    dest <- destep_test_schedule_db(list(rep(1, 8760L)))
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    out <- schedule__convert(dest, ep, "file", withr::local_tempdir())
    expect_false(
        "Adjust Schedule for Daylight Savings" %in% out$value$field_name
    )
    expect_equal(
        out$value$value_num[out$value$field_name == "Minutes per Item"],
        60
    )
    schedule__run_period(ep, c(365L, 365L))
    period <- ep$to_table(class = "RunPeriod")
    expect_equal(period$value[period$field == "Begin Day of Month"], "31")
    expect_equal(
        period$value[period$field == "Use Weather File Daylight Saving Period"],
        "No"
    )
})

test_that("schedule conversion ignores missing and zero references", {
    ep <- eplusr::empty_idf(23.1)
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(
        dest,
        "SCHEDULE_YEAR",
        data.frame(
            SCHEDULE_ID = 10L,
            NAME = "Always On",
            TYPE = 1L,
            DATA = I(list(destep_test_schedule_blob(rep(1, 8760L))))
        )
    )
    DBI::dbWriteTable(
        dest,
        "DOOR",
        data.frame(
            ID = 1:2,
            SCHEDULE = c(NA_integer_, NA_integer_)
        )
    )
    DBI::dbWriteTable(
        dest,
        "SCHEDULE_USAGE",
        data.frame(
            ID = 1:3,
            SCHEDULE_ID = c(10L, 0L, NA_integer_)
        )
    )

    schedule <- schedule__convert(dest, ep)

    expect_type(schedule, "list")
    expect_equal(attr(schedule, "table")$SCHEDULE_ID, 10L)
})

test_that("relative-humidity schedules convert DeST fractions to percent", {
    ep <- eplusr::empty_idf(23.1)
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(
        dest,
        "SCHEDULE_YEAR",
        data.frame(
            SCHEDULE_ID = c(10L, 20L),
            NAME = c("RH Minimum", "RH Maximum"),
            TYPE = c(4L, 4L),
            DATA = I(list(
                destep_test_schedule_blob(rep(0.35, 8760L)),
                destep_test_schedule_blob(rep(0.60, 8760L))
            ))
        )
    )
    destep_test_humidity_usage(dest, 10L, 20L)

    schedule <- schedule__convert(dest, ep)
    table <- attr(schedule, "table")

    expect_equal(unique(table[SCHEDULE_ID == 10L]$DATA[[1L]]), 35)
    expect_equal(unique(table[SCHEDULE_ID == 20L]$DATA[[1L]]), 60)
})

test_that("relative-humidity schedule conversion duplicates shared units", {
    ep <- eplusr::empty_idf(23.1)
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(
        dest,
        "SCHEDULE_YEAR",
        data.frame(
            SCHEDULE_ID = 10L,
            NAME = "Shared Schedule",
            TYPE = 4L,
            DATA = I(list(destep_test_schedule_blob(rep(0.35, 8760L))))
        )
    )
    destep_test_humidity_usage(dest, 10L, 10L, 10L)

    schedule <- schedule__convert(dest, ep)
    table <- attr(schedule, "table")

    expect_equal(nrow(table), 2L)
    expect_equal(
        unique(table[NAME == "Shared Schedule"]$DATA[[1L]]),
        0.35
    )
    expect_equal(
        unique(
            table[
                NAME == "Shared Schedule [Relative Humidity Percent]"
            ]$DATA[[1L]]
        ),
        35
    )
    expect_equal(
        schedule__relative_humidity_reference_names(
            dest,
            10L,
            "Shared Schedule"
        ),
        "Shared Schedule [Relative Humidity Percent]"
    )
})

test_that("relative-humidity schedule conversion rejects unsupported units", {
    ep <- eplusr::empty_idf(23.1)
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(
        dest,
        "SCHEDULE_YEAR",
        data.frame(
            SCHEDULE_ID = 10L,
            NAME = "RH Percent",
            TYPE = 4L,
            DATA = I(list(destep_test_schedule_blob(rep(35, 8760L))))
        )
    )
    destep_test_humidity_usage(dest, 10L, 10L)

    expect_error(
        schedule__convert(dest, ep),
        "fraction values in \\[0, 1\\]"
    )
})

test_that("relative-humidity schedule conversion rejects inverted bounds", {
    ep <- eplusr::empty_idf(23.1)
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(
        dest,
        "SCHEDULE_YEAR",
        data.frame(
            SCHEDULE_ID = c(10L, 20L),
            NAME = c("RH Minimum", "RH Maximum"),
            TYPE = c(4L, 4L),
            DATA = I(list(
                destep_test_schedule_blob(rep(0.70, 8760L)),
                destep_test_schedule_blob(rep(0.60, 8760L))
            ))
        )
    )
    destep_test_humidity_usage(dest, 10L, 20L)

    expect_error(
        schedule__convert(dest, ep),
        "lower schedule 10 exceeds upper schedule 20"
    )
})

test_that("schedule conversion returns null without valid references", {
    ep <- eplusr::empty_idf(23.1)
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(
        dest,
        "SCHEDULE_YEAR",
        data.frame(
            SCHEDULE_ID = 10L,
            NAME = "Always On",
            TYPE = 1L,
            DATA = I(list(destep_test_schedule_blob(rep(1, 8760L))))
        )
    )
    DBI::dbWriteTable(
        dest,
        "DOOR",
        data.frame(
            ID = 1:2,
            SCHEDULE = c(NA_integer_, NA_integer_)
        )
    )
    DBI::dbWriteTable(
        dest,
        "WINDOW",
        data.frame(
            ID = 1L,
            SCHEDULE = 0L
        )
    )

    expect_null(schedule__convert(dest, ep))
})

test_that("real model uses valid date-based schedules in both formats", {
    skip_on_cran()
    src <- ensure_dest_sqlite_file()
    on.exit(DBI::dbDisconnect(src), add = TRUE)
    directory <- withr::local_tempdir()
    for (format in c("compact", "file")) {
        opts <- destep_opts(
            schedule_format = format,
            schedule_directory = if (format == "file") directory else NULL,
            run_period = c(59L, 61L)
        )
        idf <- to_eplus(src, 23.1, options = opts)
        expect_true(idf$is_valid())
        class <- if (format == "compact") {
            "Schedule:Compact"
        } else {
            "Schedule:File"
        }
        expect_gt(nrow(idf$to_table(class = class)), 0L)
        expect_identical(
            attr(idf, "conversion")$schedules$run_period,
            c(59L, 61L)
        )
        files <- attr(idf, "conversion")$schedules$files
        expect_true(all(file.exists(files)))
    }
})

# Percent conversion must update the target limits while shared fraction uses
# retain their original 0--1 values, in both supported schedule formats.
test_that("fractional RH inputs use percent-compatible target limits", {
    connections <- list()
    directories <- character()
    on.exit(lapply(connections, DBI::dbDisconnect), add = TRUE)
    on.exit(unlink(directories, recursive = TRUE), add = TRUE)
    for (shared in c(FALSE, TRUE)) {
        dest <- destep_test_schedule_db(list(rep(0.4, 8760L)))
        connections[[length(connections) + 1L]] <- dest
        DBI::dbExecute(dest, "UPDATE SCHEDULE_YEAR SET TYPE=1")
        if (!shared) {
            DBI::dbRemoveTable(dest, "SCHEDULE_USAGE")
        }
        destep_test_humidity_usage(dest, 1L, 1L)
        directory <- tempfile("destep-rh-limits-")
        directories <- c(directories, directory)
        for (format in c("compact", "file")) {
            ep <- eplusr::empty_idf("23.1")
            result <- schedule__convert(dest, ep, format, directory)
            schedules <- attr(result, "table")
            name <- schedule__relative_humidity_reference_names(
                dest,
                1L,
                "source 1"
            )
            row <- which(schedules$NAME == name)
            expect_identical(schedules$DATA[[row]], rep(40, 8760L))
            values <- result$value
            id <- values$rleid[
                values$field_index == 1L & values$value_chr == name
            ]
            limit <- values$value_chr[
                values$rleid == id & values$field_index == 2L
            ]
            expect_identical(limit, "Any Number")
            if (shared) {
                expect_identical(schedules$DATA[[1L]], rep(0.4, 8760L))
                expect_identical(schedules$TYPE[[1L]], 1L)
            }
        }
    }
})

# Catalogue rows and legacy group controls are not effective room RH inputs.
# They must neither block conversion nor change the units of another use.
test_that("only effective room humidity references affect schedules", {
    dest <- destep_test_schedule_db(list(rep(0.4, 8760L)))
    on.exit(DBI::dbDisconnect(dest))
    DBI::dbRemoveTable(dest, "SCHEDULE_USAGE")
    destep_test_humidity_usage(dest, 1L, 1L)
    DBI::dbExecute(
        dest,
        "ALTER TABLE ROOM_GROUP ADD COLUMN SET_RH_MIN_SCHEDULE INTEGER"
    )
    DBI::dbExecute(
        dest,
        "ALTER TABLE ROOM_GROUP ADD COLUMN SET_RH_MAX_SCHEDULE INTEGER"
    )
    DBI::dbExecute(
        dest,
        "UPDATE ROOM_GROUP SET SET_RH_MIN_SCHEDULE=777, SET_RH_MAX_SCHEDULE=778"
    )
    DBI::dbExecute(dest, "INSERT INTO ROOM_TYPE_DATA VALUES (2, 99, 888, 889)")
    ep <- eplusr::empty_idf("23.1")
    result <- schedule__convert(dest, ep)
    expect_identical(attr(result, "table")$DATA[[1L]], rep(40, 8760L))
    # The same unused references become errors when an active room selects them.
    DBI::dbExecute(dest, "UPDATE ROOM SET TYPE=2")
    expect_error(
        schedule__convert(dest, ep),
        "Cannot resolve relative-humidity"
    )
    DBI::dbExecute(dest, "UPDATE ROOM_GROUP SET IS_AC_ROOM=0")
    expect_null(schedule__convert(dest, ep))
    DBI::dbExecute(dest, "UPDATE ROOM_GROUP SET IS_AC_ROOM=1")
    DBI::dbExecute(
        dest,
        "UPDATE ROOM_TYPE_DATA SET AC_SCHEDULE_ID=0 WHERE ID=2"
    )
    expect_null(schedule__convert(dest, ep))
})
