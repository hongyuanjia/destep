ensure_dest_test_file <- function(clean = FALSE) {
    p <- destep_test_fixture_file(
        "CoA_Chongqin_2015.accdb",
        "DESTEP_TEST_ACCDB"
    )

    # directly return the path if the file exists
    if (!clean && file.exists(p)) {
        return(p)
    }
    if (file.exists(p)) {
        return(p)
    }

    testthat::skip(
        paste(
            "Real DeST Access fixture is not available.",
            "Set DESTEP_TEST_FIXTURE_DIR or DESTEP_TEST_ACCDB."
        )
    )
}

ensure_dest_sqlite_file <- function(clean = FALSE) {
    path_sql <- destep_test_fixture_file(
        "CoA_Chongqin_2015.sql",
        "DESTEP_TEST_SQLITE"
    )
    path_accdb <- destep_test_fixture_file(
        "CoA_Chongqin_2015.accdb",
        "DESTEP_TEST_ACCDB"
    )
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
        # R CMD check excludes large fixtures from the source tarball. When the
        # ACCDB is supplied via DESTEP_TEST_ACCDB, write the derived SQLite file
        # to tempdir() instead of a non-existent package fixture directory.
        if (!dir.exists(dirname(path_sql))) {
            path_sql <- tempfile("CoA_Chongqin_2015-", fileext = ".sql")
        }
        read_dest(path_accdb, sqlite = path_sql, verbose = TRUE, drop = TRUE)
    }
}

destep_test_fixture_file <- function(file, envvar) {
    path <- Sys.getenv(envvar, unset = "")
    if (nzchar(path)) {
        return(path)
    }

    dir <- Sys.getenv("DESTEP_TEST_FIXTURE_DIR", unset = "")
    if (nzchar(dir)) {
        return(file.path(dir, file))
    }

    testthat::test_path("fixture", file)
}

# Return an explicitly unmultiplied in-memory variant for integration tests of
# unrelated subsystems. This avoids unequal-multiplier diagnostics in tests
# that do not exercise source boundary handling; retain the original database.
destep_test__unmultiplied_fixture <- function() {
    source <- ensure_dest_sqlite_file()
    on.exit(DBI::dbDisconnect(source))
    copy <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    RSQLite::sqliteCopyDatabase(source, copy)
    DBI::dbExecute(copy, "UPDATE STOREY SET MULTIPLE = 1")
    copy
}
