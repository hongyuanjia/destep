ensure_dest_test_file <- function(clean = FALSE) {
    p <- destep_test_fixture_file("CoA_Chongqin_2015.accdb", "DESTEP_TEST_ACCDB")

    # directly return the path if the file exists
    if (!clean && file.exists(p)) return(p)
    if (file.exists(p)) return(p)

    testthat::skip(
        paste(
            "Real DeST Access fixture is not available.",
            "Set DESTEP_TEST_FIXTURE_DIR or DESTEP_TEST_ACCDB."
        )
    )
}

ensure_dest_sqlite_file <- function(clean = FALSE) {
    path_sql <- destep_test_fixture_file("CoA_Chongqin_2015.sql", "DESTEP_TEST_SQLITE")
    path_accdb <- destep_test_fixture_file("CoA_Chongqin_2015.accdb", "DESTEP_TEST_ACCDB")
    if (!clean && file.exists(path_sql)) {
        DBI::dbConnect(RSQLite::SQLite(), path_sql)
    } else {
        if (!file.exists(path_accdb)) {
            if (file.exists(path_sql)) {
                return(DBI::dbConnect(RSQLite::SQLite(), path_sql))
            }
            testthat::skip(
                paste(
                    "Real DeST SQLite fixture is not available.",
                    "Set DESTEP_TEST_FIXTURE_DIR or DESTEP_TEST_SQLITE."
                )
            )
        }
        read_dest(path_accdb, sqlite = path_sql, verbose = TRUE, drop = TRUE)
    }
}

destep_test_fixture_file <- function(file, envvar) {
    path <- Sys.getenv(envvar, unset = "")
    if (nzchar(path)) return(path)

    dir <- Sys.getenv("DESTEP_TEST_FIXTURE_DIR", unset = "")
    if (nzchar(dir)) return(file.path(dir, file))

    testthat::test_path("fixture", file)
}
