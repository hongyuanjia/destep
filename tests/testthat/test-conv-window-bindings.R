# Minimal relational model with shuffled IDs and intentionally different face
# coefficients: restoration may change ownership and type, but no physics fields.
window_bindings__fixture <- function() {
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbWriteTable(
        dest,
        "SURFACE",
        data.frame(
            SURFACE_ID = c(21L, 10L, 11L, 20L),
            OF_ROOM = c(-1L, 8L, -1L, 7L),
            TYPE = c(0L, 1L, 0L, 0L),
            AZIMUTH = c(0, 180, 180, 0),
            VENTILATION_COEF = c(3, 21, 17, 4),
            GEOMETRY = c(201L, 100L, 101L, 200L)
        )
    )
    DBI::dbWriteTable(
        dest,
        "WINDOW",
        data.frame(
            ID = 200L,
            OF_ENCLOSURE = 100L,
            SIDE1 = 11L,
            SIDE2 = 21L
        )
    )
    DBI::dbWriteTable(
        dest,
        "MAIN_ENCLOSURE",
        data.frame(
            ID = 100L,
            SIDE1 = 10L,
            SIDE2 = 20L
        )
    )
    DBI::dbWriteTable(dest, "ROOM", data.frame(ID = 7L))
    DBI::dbWriteTable(dest, "OUTSIDE", data.frame(OUTSIDE_ID = 8L))
    DBI::dbWriteTable(dest, "GROUND", data.frame(GROUND_ID = 9L))
    dest
}

test_that("window bindings follow same-side hosts and retain every other field", {
    dest <- window_bindings__fixture()
    on.exit(DBI::dbDisconnect(dest))
    tables <- DBI::dbListTables(dest)
    before <- lapply(tables, DBI::dbReadTable, conn = dest)
    names(before) <- tables
    condition <- NULL
    audit <- withCallingHandlers(
        window__restore_bindings(dest),
        warning = function(w) {
            condition <<- w
            invokeRestart("muffleWarning")
        }
    )
    expect_s3_class(condition, "destep_restored_window_bindings")
    expect_equal(condition$bindings, audit)
    expect_equal(audit$surface_id, c(11L, 21L))
    expect_equal(audit$host_surface_id, c(10L, 20L))
    expect_equal(audit$restored_of_room, c(8L, 7L))
    expect_equal(audit$restored_type, c(1L, 0L))
    expected <- before$SURFACE
    expected$OF_ROOM <- c(7L, 8L, 8L, 7L)
    expected$TYPE <- c(0L, 1L, 1L, 0L)
    expect_identical(DBI::dbReadTable(dest, "SURFACE"), expected)
    for (table in setdiff(tables, "SURFACE")) {
        expect_identical(DBI::dbReadTable(dest, table), before[[table]])
    }
    expect_setequal(DBI::dbListTables(dest), tables)
    expect_silent(second <- window__restore_bindings(dest))
    expect_equal(nrow(second), 0L)
    expect_equal(names(second), names(audit))
    expect_match(window__binding_comments(audit)[1L], "SURFACE 11 from host 10")
    expect_length(window__binding_comments(second), 0L)
})

test_that("absent and empty windows need no restoration", {
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest))
    expect_equal(nrow(window__restore_bindings(dest)), 0L)
    DBI::dbWriteTable(dest, "WINDOW", data.frame(ID = integer()))
    expect_equal(nrow(window__restore_bindings(dest)), 0L)
})

test_that("one missing side is restored without changing the bound side", {
    dest <- window_bindings__fixture()
    on.exit(DBI::dbDisconnect(dest))
    DBI::dbExecute(dest, "UPDATE SURFACE SET OF_ROOM=7 WHERE SURFACE_ID=21")
    expect_warning(
        audit <- window__restore_bindings(dest),
        class = "destep_restored_window_bindings"
    )
    expect_equal(audit$surface_id, 11L)
})

test_that("ambiguous or incomplete window relations fail before any mutation", {
    # Each change breaks a different independent source-evidence prerequisite.
    mutations <- c(
        "INSERT INTO WINDOW VALUES (201,100,11,21)",
        "UPDATE WINDOW SET SIDE2=SIDE1",
        "INSERT INTO WINDOW SELECT * FROM WINDOW",
        "INSERT INTO MAIN_ENCLOSURE SELECT * FROM MAIN_ENCLOSURE",
        "INSERT INTO SURFACE SELECT * FROM SURFACE WHERE SURFACE_ID=11",
        "UPDATE WINDOW SET OF_ENCLOSURE=999",
        "UPDATE WINDOW SET OF_ENCLOSURE=NULL",
        "UPDATE MAIN_ENCLOSURE SET SIDE1=999",
        "UPDATE MAIN_ENCLOSURE SET SIDE1=11",
        "UPDATE SURFACE SET OF_ROOM=-1 WHERE SURFACE_ID=10",
        "UPDATE SURFACE SET OF_ROOM=NULL WHERE SURFACE_ID=10",
        "UPDATE SURFACE SET OF_ROOM=999 WHERE SURFACE_ID=10",
        "UPDATE SURFACE SET TYPE=4 WHERE SURFACE_ID=10",
        "UPDATE SURFACE SET TYPE=NULL WHERE SURFACE_ID=10",
        "UPDATE SURFACE SET OF_ROOM=8 WHERE SURFACE_ID=21",
        "UPDATE SURFACE SET OF_ROOM=NULL WHERE SURFACE_ID=21",
        "UPDATE WINDOW SET SIDE2=999",
        "INSERT INTO OUTSIDE VALUES (8)",
        "INSERT INTO SURFACE VALUES (99,-1,0,0,0,99)",
        "CREATE TABLE DOOR AS SELECT 11 AS SIDE1, 21 AS SIDE2",
        "DROP TABLE MAIN_ENCLOSURE",
        "ALTER TABLE WINDOW RENAME COLUMN OF_ENCLOSURE TO BROKEN"
    )
    for (mutation in mutations) {
        dest <- window_bindings__fixture()
        tryCatch(
            {
                DBI::dbExecute(dest, mutation)
                before <- DBI::dbReadTable(dest, "SURFACE")
                expect_error(
                    window__restore_bindings(dest),
                    class = "destep_invalid_window_bindings",
                    info = mutation
                )
                expect_identical(
                    DBI::dbReadTable(dest, "SURFACE"),
                    before,
                    info = mutation
                )
            },
            finally = DBI::dbDisconnect(dest)
        )
    }
})

test_that("restoration checks owner namespaces and supports explicit ground hosts", {
    dest <- window_bindings__fixture()
    on.exit(DBI::dbDisconnect(dest))
    DBI::dbExecute(dest, "UPDATE SURFACE SET OF_ROOM=7 WHERE SURFACE_ID=10")
    expect_error(window__restore_bindings(dest), "declared boundary table")
    DBI::dbExecute(
        dest,
        "UPDATE SURFACE SET OF_ROOM=9, TYPE=2 WHERE SURFACE_ID=10"
    )
    expect_warning(
        audit <- window__restore_bindings(dest),
        class = "destep_restored_window_bindings"
    )
    expect_equal(audit$restored_of_room, c(9L, 7L))
    expect_equal(audit$restored_type, c(2L, 0L))
})

test_that("an update failure rolls back every restored binding", {
    dest <- window_bindings__fixture()
    on.exit(DBI::dbDisconnect(dest))
    before <- DBI::dbReadTable(dest, "SURFACE")
    tables <- DBI::dbListTables(dest)
    DBI::dbExecute(
        dest,
        "CREATE TRIGGER refuse_binding BEFORE UPDATE ON SURFACE WHEN NEW.SURFACE_ID=11 BEGIN SELECT RAISE(ABORT, 'test refusal'); END"
    )
    expect_error(window__restore_bindings(dest), "test refusal")
    expect_identical(DBI::dbReadTable(dest, "SURFACE"), before)
    expect_setequal(DBI::dbListTables(dest), tables)
})

test_that("warnings promoted to errors roll back a restoration", {
    dest <- window_bindings__fixture()
    on.exit(DBI::dbDisconnect(dest))
    before <- DBI::dbReadTable(dest, "SURFACE")
    withr::local_options(warn = 2L)
    expect_error(window__restore_bindings(dest), "Restored")
    expect_identical(DBI::dbReadTable(dest, "SURFACE"), before)
})
