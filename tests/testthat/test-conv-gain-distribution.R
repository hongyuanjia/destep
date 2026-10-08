# A small source fixture isolates effective room-type references from unused
# distribution records and avoids invoking geometry or EnergyPlus simulation.
gains_test__distribution_source <- function() {
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbWriteTable(dest, "ROOM", data.frame(ID = 1:2, TYPE = 1L))
    DBI::dbWriteTable(
        dest,
        "ROOM_TYPE_DATA",
        data.frame(
            ID = 1:2,
            O_DIST_MODE = 2L,
            O_MINNUMBER = 0,
            O_MAXNUMBER = 1,
            O_HEAT_PER_PERSON = 62,
            L_DIST_MODE = 3L,
            L_MINPOWER = 0,
            L_MAXPOWER = 10,
            L_HEAT_RATE = 0.9,
            E_DIST_MODE = 4L,
            E_MINPOWER = 0,
            E_MAXPOWER = 20
        )
    )
    DBI::dbWriteTable(
        dest,
        "DIST_MODE",
        data.frame(
            DIST_MODE_ID = 2:5,
            DIST_AIR = c(0.5, 0.3, 0.7, 0),
            DIST_AROUND = c(0.25, 0.28, 0.1, 0),
            DIST_FLOOR = c(0.05, 0.35, 0.1, 1),
            DIST_ROOF = c(0.2, 0.07, 0.1, 0)
        )
    )
    dest
}

test_that("gain distribution diagnostics deduplicate effective source modes", {
    dest <- gains_test__distribution_source()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    before <- DBI::dbReadTable(dest, "DIST_MODE")
    seen <- NULL
    expect_true(withCallingHandlers(
        internal_gains__warn_surface_distribution(dest),
        destep_unsupported_gain_distribution = function(w) {
            seen <<- w
            invokeRestart("muffleWarning")
        }
    ))
    expect_s3_class(seen, "destep_unsupported_gain_distribution")
    expect_match(
        conditionMessage(seen),
        "DIST_AROUND, DIST_FLOOR and DIST_ROOF"
    )
    expect_equal(seen$distributions$GAIN_TYPE, c("E", "L", "O"))
    expect_equal(seen$distributions$DIST_MODE_ID, c(4L, 3L, 2L))
    expect_equal(nrow(seen$distributions), 3L)
    expect_equal(DBI::dbReadTable(dest, "DIST_MODE"), before)
})

test_that("unused and zero-sensible gain sources do not warn", {
    dest <- gains_test__distribution_source()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    # Type 2 retains positive gains, but no room selects it. People with no
    # sensible activity and lighting with zero thermal ratio are also excluded.
    DBI::dbExecute(
        dest,
        paste(
            "UPDATE ROOM_TYPE_DATA SET O_HEAT_PER_PERSON=0, L_HEAT_RATE=0,",
            "E_MAXPOWER=0 WHERE ID=1"
        )
    )
    expect_silent(expect_false(internal_gains__warn_surface_distribution(dest)))
    DBI::dbExecute(dest, "DELETE FROM ROOM")
    expect_silent(expect_false(internal_gains__warn_surface_distribution(dest)))
})

test_that("all-convective and absent surface-family schemas do not warn", {
    dest <- gains_test__distribution_source()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    DBI::dbExecute(
        dest,
        paste(
            "UPDATE DIST_MODE SET DIST_AIR=1, DIST_AROUND=0, DIST_FLOOR=0,",
            "DIST_ROOF=0 WHERE DIST_MODE_ID IN (2,3,4)"
        )
    )
    expect_silent(expect_false(internal_gains__warn_surface_distribution(dest)))
    DBI::dbExecute(dest, "ALTER TABLE DIST_MODE DROP COLUMN DIST_FLOOR")
    expect_silent(expect_false(internal_gains__warn_surface_distribution(dest)))
})

test_that("reduced gain-family schemas retain the applicable diagnostic", {
    dest <- gains_test__distribution_source()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    DBI::dbExecute(dest, "ALTER TABLE ROOM_TYPE_DATA DROP COLUMN L_HEAT_RATE")
    DBI::dbExecute(
        dest,
        "ALTER TABLE ROOM_TYPE_DATA DROP COLUMN O_HEAT_PER_PERSON"
    )
    expect_warning(
        internal_gains__warn_surface_distribution(dest),
        "mode\\(s\\) \\[4\\]",
        class = "destep_unsupported_gain_distribution"
    )
})
