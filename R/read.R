#' Read tables from a DeST model and convert to SQLite
#'
#' @param file \[string\] Path to the DeST model file.
#'
#' @param tables \[character\] Vector of table names to read from the DeST
#'        model. If `NULL`, which is the default, all tables will be read.
#'
#' @param sqlite \[string\] Path to the SQLite database file. If `:memory:`,
#'        which is the default, a temporary in-memory database will be created.
#'
#' @return \[DBIConnection\] A SQLite database connection with all specified tables..
#'
#' @note Tables with the same name will be overwritten in the SQLite database.
#'
#' @export
read_dest <- function(file, tables = NULL, sqlite = ":memory:") {
    if (!(length(file) == 1L && is.character(file) && !is.na(file))) {
        stop("'file' should be a single file path string")
    }

    # make sure that the file exists before attempting to connect
    if (!file.exists(file)) {
        stop(sprintf("DB file does not exist at '%s'", file))
    }

    file <- normalizePath(file)

    # use Microsoft Access Driver on Windows
    if (.Platform$OS.type == "windows") {
        access_to_sqlite_dbi(file, sqlite, tables, drop = TRUE)
    } else {
        access_to_sqlite_mdbtools(file, sqlite, tables, drop = TRUE)
    }
}

#' Convert Microsoft Access database to SQLite using DBI
#'
#' @param accdb \[string\] Path to the Microsoft Access database file.
#'
#' @param sqlite \[string\] Path to the SQLite database file. If `:memory:`,
#'        which is the default, a temporary in-memory database will be created.
#'
#' @param tables \[character\] Vector of table names to convert from the
#'        Microsoft Access database to the SQLite database. If `NULL`, which
#'        is the default, all tables will be converted.
#'
#' @param drop \[logical\] Whether to drop tables in the SQLite database if
#'        they already exist. Default is `TRUE`.
#'
#' @return \[DBIConnection\] A SQLite database connection.
#'
#' @note This only works on Windows. For macOS and Linux, use
#'       [access_to_sqlite_mdbtools()] instead.
#'
#' @keywords internal
access_to_sqlite_dbi <- function(accdb, sqlite = ":memory:", tables = NULL, drop = TRUE) {
    if (!file.exists(accdb)) {
        stop(sprintf("Input 'accdb' file '%s' did not exists", accdb))
    }

    conn_accdb <- DBI::dbConnect(odbc::odbc(),
        driver = "Microsoft Access Driver (*.mdb, *.accdb)",
        dbq = accdb
    )
    on.exit(DBI::dbDisconnect(conn_accdb), add = TRUE)

    if (is.null(tables)) {
        tables <- DBI::dbListTables(conn_accdb)
    } else {
        tables <- unique(tables)
    }

    # exclude MSys* tables
    tables <- setdiff(
        tables,
        c(
            "MSysAccessStorage",          "MSysAccessXML",
            "MSysACEs",                   "MSysComplexColumns",
            "MSysNavPaneGroupCategories", "MSysNavPaneGroups",
            "MSysNavPaneGroupToObjects",  "MSysNavPaneObjectIDs",
            "MSysObjects",                "MSysQueries",
            "MSysRelationships",          "MSysResources"
        )
    )

    conn_sql <- DBI::dbConnect(RSQLite::SQLite(), sqlite)

    DBI::dbWithTransaction(conn_sql, {
        for (tbl in tables) {
            # drop table if exists
            if (drop) DBI::dbExecute(conn_sql, sprintf("DROP TABLE IF EXISTS %s", tbl))

            # TODO: get the schema

            message(sprintf("Importing table '%s' to SQLite database...", tbl))
            DBI::dbWriteTable(conn_sql, tbl, DBI::dbReadTable(conn_accdb, tbl))
        }
    })

    conn_sql
}

#' Convert Microsoft Access database to SQLite using mdbtools
#'
#' @param accdb \[string\] Path to the Microsoft Access database file.
#'
#' @param sqlite \[string\] Path to the SQLite database file. If `:memory:`,
#'        which is the default, a temporary in-memory database will be created.
#'
#' @param tables \[character\] Vector of table names to convert from the
#'        Microsoft Access database to the SQLite database. If `NULL`, which
#'        is the default, all tables will be converted.
#'
#' @param drop \[logical\] Whether to drop tables in the SQLite database if
#'        they already exist. Default is `TRUE`.
#'
#' @return \[DBIConnection\] A SQLite database connection.
#'
#' @note This function requires 'mdbtools' installed and can be found in PATH,
#'       which means that it only works on macOS and Linux. For Windows, use
#'       [access_to_sqlite_dbi()] instead.
#'
#' @keywords internal
access_to_sqlite_mdbtools <- function(accdb, sqlite = ":memory:", tables = NULL, drop = TRUE) {
    if (!file.exists(accdb)) {
        stop(sprintf("Input 'accdb' file '%s' did not exists", accdb))
    }

    # use mdbtools on macOS and Linux
    mdbtools <- c(Sys.which("mdb-tables"), Sys.which("mdb-schema"), Sys.which("mdb-export"))

    if (any(miss <- mdbtools == "")) {
        stop(sprintf(
            paste(
                "'%s' executable was not found on system PATH.",
                "Please install 'mdbtools' and try again."
            ),
            names(miss)[miss][[1L]]
        ))
    }

    if (!is.null(tables)) {
        tables <- unique(tables)
    } else {
        tables <- system2(
            mdbtools["mdb-tables"], args = c("-1", accdb),
            stdout = TRUE, stderr = TRUE
        )
        if (!is.null(attr(tables, "status"))) {
            stop(sprintf("Failed to list tables from '%s': %s", accdb, tables))
        }
    }

    # create a SQLite database connection
    conn <- DBI::dbConnect(RSQLite::SQLite(), sqlite)

    DBI::dbWithTransaction(conn, {
        for (tbl in tables) {
            # drop table if exists
            if (drop) DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s", tbl))

            # dump the table schema in SQLite format
            schema <- system2(
                mdbtools["mdb-schema"], c("-T", tbl, accdb, "sqlite"),
                stdout = TRUE, stderr = TRUE
            )
            if (!is.null(attr(schema, "status"))) {
                stop(sprintf("Failed to dump schema of table '%s' from '%s': %s", tbl, accdb, schema))
            }

            # create the empty table
            DBI::dbExecute(conn, paste0(schema, collapse = "\n"))

            # export table data in SQLite format
            data <- system2(
                mdbtools["mdb-export"],
                c("-I", "sqlite", "-b", "octal", "-D", "%F", "-T", shQuote("%F %H:%M:%S"), accdb, tbl),
                stdout = TRUE, stderr = TRUE
            )
            if (!is.null(attr(data, "status"))) {
                stop(sprintf("Failed to export data of table '%s' from '%s': %s", tbl, accdb, data))
            }

            # skip if empty table
            if (length(data) == 0L) next()

            # NOTE: 'mdb-export' trunks the data by 1000. 'dbExecute()'
            #       could not handle multiple statements.
            #       See: https://github.com/r-dbi/RSQLite/issues/313
            #
            #       Export data in some table, e.g. "ROOM_TYPE_DATA", is
            #       split at wired places.
            #       Here we first concatenate all data into a single
            #       string and then split
            data <- unlist(lapply(
                strsplit(
                    paste0(data, collapse = ""), ";INSERT INTO",
                    fixed = TRUE, useBytes = TRUE
                ),
                function(stmt) {
                    if ((len <- length(stmt)) > 1L) {
                        stmt[1L] <- paste0(stmt[1L], ";")
                        stmt[-1L] <- paste0("INSERT INTO", stmt[-1L])
                        stmt[-c(1L, len)] <- paste0(stmt[-c(1L, len)], ";")
                    }
                    stmt
                }
            ))

            # issue warnings immediately when they occur
            old <- getOption("warn")
            options("warn" = 1L)
            on.exit(options(warn = old), add = TRUE)

            message(sprintf("Importing table '%s' to SQLite database...", tbl))

            # insert data to the table if not empty
            for (d in data) DBI::dbExecute(conn, paste0(d, collapse = "\n"))
        }
    })

    conn
}
