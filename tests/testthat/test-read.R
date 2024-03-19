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
