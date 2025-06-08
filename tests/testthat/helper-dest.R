ensure_dest_test_file <- function(clean = FALSE) {
    p <- testthat::test_path("fixture", "CoA_Chongqin_2015.accdb")

    # directly return the path if the file exists
    if (!clean && file.exists(p)) return(p)

    # otherwise download a typical building model from the DeST website
    download_dest_model(
        "Commercial office A", "Chongqin", 2015,
        testthat::test_path("fixture")
    )
}

ensure_dest_sqlite_file <- function(clean = FALSE) {
    path_sql <- testthat::test_path("fixture", "CoA_Chongqin_2015.sql")
    path_accdb <- testthat::test_path("fixture", "CoA_Chongqin_2015.accdb")
    if (!clean && file.exists(path_sql)) {
        DBI::dbConnect(RSQLite::SQLite(), path_sql)
    } else {
        if (!file.exists(path_accdb)) {
            ensure_dest_test_file()
        }
        read_dest(path_accdb, sqlite = path_sql, verbose = TRUE, drop = TRUE)
    }
}
