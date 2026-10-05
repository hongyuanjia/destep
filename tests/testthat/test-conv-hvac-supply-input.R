# Isolate numeric foreign keys and binary schedule integrity from HVAC topology.
hvac_test__supply_database <- function() {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbWriteTable(
        con,
        "AC_SYS",
        data.frame(AC_SYS_ID = 1L, SUPPLY_T_MIN = 50, SUPPLY_T_MAX = 51)
    )
    DBI::dbWriteTable(
        con,
        "SCHEDULE_YEAR",
        data.frame(
            SCHEDULE_ID = 50:51,
            NAME = c("Supply Minimum", "Supply Maximum"),
            DATA = I(rep(list(destep_test_schedule_blob(rep(12, 8760L))), 2L))
        )
    )
    con
}

# Invalid foreign keys must fail before any lossy integer conversion occurs.
test_that("supply temperature references are validated before coercion", {
    con <- hvac_test__supply_database()
    on.exit(DBI::dbDisconnect(con))
    expect_equal(hvac__single_supply_bounds(con, 1L), c(12, 12))
    for (id in c(50.5, 50 + 1e-9, NA_real_, Inf, .Machine$integer.max + 1)) {
        DBI::dbExecute(
            con,
            "UPDATE AC_SYS SET SUPPLY_T_MIN=?",
            params = list(id)
        )
        source <- data.table::as.data.table(DBI::dbReadTable(con, "AC_SYS"))
        expect_error(hvac__single_supply_bounds(con, 1L), "references")
        expect_error(hvac__supply_temperature_source(con, source), "references")
    }
})

# Both source paths must enforce the complete non-leap payload, not only read
# its first 8760 doubles, and reject missing hourly values or duplicate IDs.
test_that("supply temperatures require complete unambiguous hourly input", {
    con <- hvac_test__supply_database()
    on.exit(DBI::dbDisconnect(con))
    source <- data.table::as.data.table(DBI::dbReadTable(con, "AC_SYS"))
    expect_equal(
        hvac__supply_temperature_source(
            con,
            source
        )$cooling_design_supply_temperature_c,
        12
    )
    for (hours in list(rep(12, 8759L), rep(12, 8761L), rep(NA_real_, 8760L))) {
        DBI::dbExecute(
            con,
            "UPDATE SCHEDULE_YEAR SET DATA=?",
            params = list(list(destep_test_schedule_blob(hours)))
        )
        expect_error(hvac__single_supply_bounds(con, 1L))
        expect_error(hvac__supply_temperature_source(con, source))
    }
    DBI::dbExecute(
        con,
        "UPDATE SCHEDULE_YEAR SET DATA=?",
        params = list(list(destep_test_schedule_blob(rep(12, 8760L))))
    )
    DBI::dbExecute(
        con,
        "INSERT INTO SCHEDULE_YEAR SELECT * FROM SCHEDULE_YEAR WHERE SCHEDULE_ID=50"
    )
    expect_error(hvac__single_supply_bounds(con, 1L))
    expect_error(
        hvac__supply_temperature_source(con, source),
        class = "destep_invalid_hvac_source_key"
    )
})
