# Store DeST hourly schedule data in the BLOB shape that readBin() expects when
# `destep_conv_schedule()` reads SCHEDULE_YEAR.DATA from SQLite.
destep_test_schedule_blob <- function(values) {
    writeBin(as.double(values), raw(), size = 8L)
}

test_that("schedule conversion writes resolvable week day references", {
    ep <- ensure_empty_idf()
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(dest, "SCHEDULE_YEAR", data.frame(
        SCHEDULE_ID = 10L,
        NAME = "Always On",
        TYPE = 1L,
        DATA = I(list(destep_test_schedule_blob(rep(1, 8760L))))
    ))
    DBI::dbWriteTable(dest, "SCHEDULE_USAGE", data.frame(
        ID = 1L,
        SCHEDULE_ID = 10L
    ))

    schedule <- destep_conv_schedule(dest, ep)
    value <- schedule$value

    week_day_names <- value$value_chr[
        value$class_name == "Schedule:Week:Compact" &
            grepl("^Schedule:Day Name", value$field_name)
    ]
    day_names <- value$value_chr[
        value$class_name == "Schedule:Day:Interval" &
            value$field_name == "Name"
    ]

    expect_false(anyNA(week_day_names))
    expect_true(all(unique(week_day_names) %in% day_names))
})

test_that("real model schedule week and year references are resolvable", {
    skip_on_cran()

    src <- ensure_dest_sqlite_file()
    on.exit(DBI::dbDisconnect(src), add = TRUE)

    idf <- to_eplus(src, 23.1)
    day <- idf$to_table(class = "Schedule:Day:Interval", all = TRUE)
    week <- idf$to_table(class = "Schedule:Week:Compact", all = TRUE)
    year <- idf$to_table(class = "Schedule:Year", all = TRUE)

    week_day_names <- week$value_chr[grepl("^Schedule:Day Name", week$field_name)]
    day_names <- day$value_chr[day$field_name == "Name"]
    year_week_names <- year$value_chr[grepl("^Schedule:Week Name", year$field_name)]
    week_names <- week$value_chr[week$field_name == "Name"]

    expect_false(anyNA(week_day_names))
    expect_true(all(unique(week_day_names) %in% day_names))
    expect_false(anyNA(year_week_names))
    expect_true(all(unique(year_week_names) %in% week_names))

    validity <- idf$validate()
    expect_equal(
        nrow(validity$missing_value[
            validity$missing_value$class_name == "Schedule:Week:Compact"
        ]),
        0L
    )
    expect_equal(
        nrow(validity$incomplete_extensible[
            validity$incomplete_extensible$class_name == "Schedule:Week:Compact"
        ]),
        0L
    )

    # AllOtherDays matches every still-unassigned day type, so EnergyPlus
    # requires it to follow the explicit Schedule:Week:Compact assignments.
    for (week_name in unique(week$name)) {
        daytypes <- week[
            name == week_name & grepl("^DayType List", field) & !is.na(value),
            value
        ]
        all_other <- which(daytypes == "For: AllOtherDays")
        if (length(all_other)) expect_equal(all_other, length(daytypes))
    }
})
