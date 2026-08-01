test_that("Access backends reject empty table selections consistently", {
    expect_error(
        read__normalize_tables(character()),
        class = "error_no_tables_to_convert"
    )
    expect_error(
        read__normalize_tables(c("ROOM", NA_character_)),
        class = "error_invalid_table_names"
    )
    expect_equal(
        read__normalize_tables(c("ROOM", "ROOM", "MSysObjects")),
        "ROOM"
    )
})

test_that("target SQLite connections close only after failed conversion", {
    captured <- new.env(parent = emptyenv())
    expect_error(
        read__with_sqlite_target(":memory:", function(conn) {
            captured$conn <- conn
            stop("injected conversion failure")
        }),
        "injected conversion failure"
    )
    expect_false(DBI::dbIsValid(captured$conn))

    conn <- read__with_sqlite_target(":memory:", function(conn) {
        DBI::dbExecute(conn, "CREATE TABLE converted (id INTEGER)")
    })
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
    expect_true(DBI::dbIsValid(conn))
    expect_true("converted" %in% DBI::dbListTables(conn))
})

test_that("read_dest()", {
    skip_on_cran()
    path <- ensure_dest_test_file()

    # can stop if invalid file input
    expect_error(dest <- read_dest(1), "single file path")

    # can stop if file does not exist
    expect_error(dest <- read_dest(tempfile()), "did not exist")

    # can read specific tables
    expect_s4_class(dest <- read_dest(path, "WINDOW"), "SQLiteConnection")
    expect_equal(DBI::dbListTables(dest), "WINDOW")
    DBI::dbDisconnect(dest)

    # can read all tables
    expect_message(expect_s4_class(dest <- read_dest(path, verbose = TRUE), "SQLiteConnection"))
    expect_true("WINDOW" %in% DBI::dbListTables(dest))
    DBI::dbDisconnect(dest)
})
