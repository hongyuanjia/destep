# Create shared active pairs plus inactive and unused room types. ROOM_GROUP
# deliberately disagrees with ROOM_TYPE_DATA so the source owner is tested.
temperature_test__db <- function(heating, cooling) {
    dest <- destep_test_schedule_db(list(heating, cooling))
    DBI::dbWriteTable(
        dest,
        "ROOM",
        data.frame(
            ID = 1:4,
            NAME = paste("Room", 1:4),
            TYPE = c(1L, 1L, 2L, 3L),
            OF_ROOM_GROUP = 1:4
        )
    )
    DBI::dbWriteTable(
        dest,
        "ROOM_GROUP",
        data.frame(
            ROOM_GROUP_ID = 1:4,
            IS_AC_ROOM = c(1L, 1L, 0L, 1L),
            SET_T_MIN_SCHEDULE = 2L,
            SET_T_MAX_SCHEDULE = 1L
        )
    )
    DBI::dbWriteTable(
        dest,
        "ROOM_TYPE_DATA",
        data.frame(
            ID = 1:4,
            AC_SCHEDULE_ID = c(1L, 1L, 0L, 1L),
            SET_T_MIN_SCHEDULE = 1L,
            SET_T_MAX_SCHEDULE = 2L
        )
    )
    dest
}

test_that("temperature conflicts warn once per effective pair and preserve values", {
    heating <- replace(rep(18, 8760), 7667L, 21)
    cooling <- replace(rep(26, 8760), 7667L, 18)
    dest <- temperature_test__db(heating, cooling)
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    before <- DBI::dbReadTable(dest, "SCHEDULE_YEAR")
    warnings <- list()
    out <- withCallingHandlers(
        schedule__convert(dest, eplusr::empty_idf(23.1)),
        destep_thermostat_conflict = function(w) {
            warnings[[length(warnings) + 1L]] <<- w
            invokeRestart("muffleWarning")
        }
    )
    expect_length(warnings, 1L)
    expect_match(conditionMessage(warnings[[1L]]), "11-16 10:00-11:00")
    conflicts <- attr(out, "temperature_conflicts")
    expect_identical(conflicts, warnings[[1L]]$conflicts)
    expect_equal(nrow(conflicts), 1L)
    expect_equal(conflicts$ROOM_IDS, list(1:2))
    expect_equal(conflicts$FIRST_HOUR_INDEX, 7666L)
    expect_equal(conflicts$HOURS, 1L)
    expect_equal(conflicts$HEATING_C, 21)
    expect_equal(conflicts$COOLING_C, 18)
    expect_identical(attr(out, "table")$DATA, list(heating, cooling))
    expect_identical(DBI::dbReadTable(dest, "SCHEDULE_YEAR"), before)
})

test_that("temperature diagnostics accept equal bounds and handle full-year conflicts", {
    dest <- temperature_test__db(rep(20, 8760), rep(20, 8760))
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    data <- data.table::data.table(
        SCHEDULE_ID = 1:2,
        DATA = list(rep(20, 8760), rep(20, 8760))
    )
    expect_equal(nrow(schedule__temperature_conflicts(dest, data)), 0L)
    data.table::set(data, 1L, "DATA", list(list(rep(21, 8760))))
    conflicts <- schedule__temperature_conflicts(dest, data)
    expect_equal(conflicts$HOURS, 8760L)
    expect_equal(conflicts$FIRST_HOUR_INDEX, 0L)
    # Unresolved IDs are not converted into zeros by the supplementary check.
    expect_equal(nrow(schedule__temperature_conflicts(dest, data[1L])), 0L)
    expect_equal(nrow(schedule__temperature_conflicts(dest, data[0L])), 0L)
    DBI::dbExecute(dest, "DELETE FROM ROOM")
    expect_equal(nrow(schedule__temperature_conflicts(dest, data)), 0L)
})

test_that("unused and disabled pairs do not emit temperature conflict warnings", {
    dest <- temperature_test__db(rep(21, 8760), rep(18, 8760))
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    DBI::dbExecute(dest, "UPDATE ROOM_GROUP SET IS_AC_ROOM = 0")
    expect_warning(
        out <- schedule__convert(dest, eplusr::empty_idf(23.1)),
        NA
    )
    expect_equal(nrow(attr(out, "temperature_conflicts")), 0L)
})
