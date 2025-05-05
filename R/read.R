#' Read tables from a DeST model and convert to SQLite
#'
#' @param accdb \[string\] Path to the DeST model file. Usually a Microsoft
#'       Access database file with `.accdb` extension.
#'
#' @param tables \[character\] Vector of table names to read from the DeST
#'        model. If `NULL`, which is the default, all tables will be read.
#'
#' @param sqlite \[string\] Path to the SQLite database file. If `:memory:`,
#'        which is the default, a temporary in-memory database will be created.
#'
#' @param verbose \[logical\] Whether to print information about the conversion
#'       process. Default is `FALSE`.
#'
#' @param drop \[logical\] Whether to drop tables in the SQLite database if
#'        they already exist. Default is `TRUE`.
#'
#' @return \[DBIConnection\] A SQLite database connection with all specified tables.
#'
#' @note Tables with the same name will be overwritten in the SQLite database.
#'
#' @details
#' The function converts a Microsoft Access database (`.accdb` or `.mdb`) to SQLite
#' using the following approach:
#'
#' 1. **Driver Selection**: The function tries to use ODBC drivers first:
#'    - On Windows: Primarily uses "`Microsoft Access Driver (*.mdb, *.accdb)`"
#'    - On macOS/Linux: Tries to find available drivers like "`MDBTools Driver`"
#'
#' 2. **Fallback Mechanism**: If ODBC connection fails, on non-Windows systems,
#'    the function falls back to using MDBTools command-line utilities
#'
#' 3. **Table Selection**:
#'    - If `tables = NULL`, all tables are read except for system tables (MSys*)
#'    - Otherwise, only the specified tables are processed
#'
#' 4. **Data Transfer**: For each table:
#'    - Optionally drops existing table in target SQLite database
#'    - Reads the entire table from Access
#'    - Writes the data to SQLite
#'
#' 5. **Schema Handling**: Table structure (columns and types) is automatically
#'    preserved during the transfer process
#'
#' For systems without proper ODBC drivers (especially some Linux distributions),
#' consider installing one of the following:
#' - `MDBTools`: Open-source utilities for reading Access databases
#' - `LibreOffice Base`: Provides ODBC drivers for Access
#' - Commercial drivers: `Actual Technologies` or `Easysoft ODBC drivers`
#'
#' @export
read_dest <- function(accdb, tables = NULL, sqlite = ":memory:", verbose = FALSE, drop = TRUE) {
    # TODO: support DBIConnection input
    if (!is_string(accdb)) {
        abort("'accdb' should be a single file path string", "error_invalid_accdb_path")
    }

    # make sure that the file exists before attempting to connect
    if (!file.exists(accdb)) {
        abort(sprintf("Input 'accdb' file '%s' did not exist", accdb), "error_accdb_file_not_found")
    }

    if (!is.null(tables) && !is_character(tables)) {
        abort("'tables' should be a character vector", "error_invalid_argument")
    }

    if (!is_flag(drop)) {
        abort("'drop' should be a single logical value of 'TRUE' or 'FALSE'", "error_invalid_argument")
    }

    if (!is_flag(verbose)) {
        abort("'verbose' should be a single logical value of 'TRUE' or 'FALSE'", "error_invalid_argument")
    }

    # Try to use ODBC first
    if (.Platform$OS.type == "windows") {
        # On Windows, we only use ODBC
        access_to_sqlite_odbc(accdb, sqlite, tables, drop, verbose)
    } else {
        # On non-Windows, we try ODBC first, then MDBTools
        fall_back <- function(e) {
            if (verbose) {
                cli::cli_alert_warning(paste0("ODBC connection failed with error: ", e$message))
                cli::cli_alert_info("Falling back to MDBTools command-line utilities...")
            }
            access_to_sqlite_mdbtools(accdb, sqlite, tables, drop, verbose)
        }
        tryCatch(
            access_to_sqlite_odbc(accdb, sqlite, tables, drop, verbose),
            error_odbc_driver_missing = fall_back,
            error_odbc_connection_failed = fall_back,
            error_list_tables_failed = fall_back,
            error_invalid_attribute_option_identifier = fall_back
        )
    }
}

#' Connect to Microsoft Access database using ODBC
#'
#' @inheritParams read_dest
#'
#' @keywords internal
access_to_sqlite_odbc <- function(accdb, sqlite = ":memory:", tables = NULL, drop = TRUE, verbose = FALSE) {
    # check for available ODBC drivers
    odbc_drivers <- odbc::odbcListDrivers()

    # select the driver based on current platform
    if (.Platform$OS.type == "windows") {
        possible_drivers <- c(
            "Microsoft Access Driver (*.mdb, *.accdb)",
            "Microsoft Access Driver (*.mdb)",
            "Microsoft Access Driver (*.accdb)",
            "Actual Access Driver",
            "Easysoft Access ODBC Driver",
            "LibreOffice Base Driver"
        )
    } else {
        # try to find a UnixODBC driver
        possible_drivers <- c(
            "MDBTools Driver",
            "Actual Access Driver",
            "Easysoft Access ODBC Driver",
            "LibreOffice Base Driver"
        )
    }
    driver <- intersect(possible_drivers, odbc_drivers$name)
    if (length(driver) == 0L) {
        msg <- "No suitable ODBC driver for Access found."
        if (.Platform$OS.type == "windows") {
            msg <- paste(msg, "Please install Microsoft Access or another Access-compatible ODBC driver.")
        } else {
            msg <- paste(msg, "Please install mdbtools or another Access-compatible ODBC driver.")
            if (Sys.info()["sysname"] == "Darwin") {
                msg <- paste(
                    msg, "You can also install 'mdbtools' with ODBC driver support via 'brew':\n",
                    "    brew install hongyuanjia/mdbtools-odbc/mdbtools"
                )
            } else {
                msg <- paste(msg, "You can also install 'mdbtools' ODBC driver via your package manager.")
                msg <- paste(msg, "For Debian/Ubuntu:\n    sudo apt-get install odbc-mdbtools")
            }
        }
        abort(msg, "error_odbc_driver_missing")
    }

    if (verbose) cli::cli_alert_info(sprintf("Using ODBC driver: '%s'", driver[1L]))

    # connect to the Access database
    if (verbose) cli::cli_progress_step("Connecting to the Access database")
    conn_accdb <- tryCatch(
        DBI::dbConnect(odbc::odbc(), driver = driver[1L], dbq = accdb),
        error = function(e) {
            abort(
                paste0("Failed to connect using ODBC: ", e$message),
                class = "error_odbc_connection_failed"
            )
        }
    )

    on.exit(DBI::dbDisconnect(conn_accdb), add = TRUE)

    if (is.null(tables)) {
        tables <- tryCatch(
            DBI::dbListTables(conn_accdb),
            error = function(e) {
                abort(
                    paste0("Failed to list tables from Access database: ", e$message),
                    class = "error_list_tables_failed"
                )
            }
        )
    } else {
        if (!is.character(tables) || anyNA(tables)) {
            abort("'tables' should be a character vector with no missing values", "error_invalid_table_names")
        }
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
    if (length(tables) == 0L) {
        abort("No tables to convert", "error_no_tables_to_convert")
    }

    conn_sql <- DBI::dbConnect(RSQLite::SQLite(), sqlite)

    tryCatch(
        DBI::dbWithTransaction(conn_sql, {
            if (verbose) {
                i <- 0L
                n <- length(tables)
                tbl <- tables[[1L]]
                step <- cli::cli_progress_step(
                    "Converting Microsoft Access database to SQLite [{i}/{n} table{?s}]: {.code {tbl}} ",
                    "Converting Microsoft Access database to SQLite [{i}/{n} table{?s}]",
                    spinner = TRUE
                )
            }
            for (tbl in tables) {
                # drop table if exists
                if (drop) DBI::dbExecute(conn_sql, sprintf("DROP TABLE IF EXISTS `%s`", tbl))

                # TODO: get the schema

                DBI::dbWriteTable(conn_sql, tbl, DBI::dbReadTable(conn_accdb, tbl))
                if (verbose) {
                    i <- i + 1L
                    cli::cli_progress_update(id = step)
                }
            }
        }),
        error = function(e) {
            if (grepl("Invalid attribute/option identifier", e$message)) {
                abort(
                    sprintf("Failed to convert Microsoft Access database to SQLite: %s", e$message),
                    "error_invalid_attribute_option_identifier"
                )
            } else {
                stop(e)
            }
        }
    )

    conn_sql
}

#' Convert Microsoft Access database to SQLite using mdbtools
#'
#' @inheritParams read_dest
#'
#' @note This function requires 'mdbtools' installed and can be found in PATH,
#'       which means that it only works on macOS and Linux. For Windows, use
#'       [access_to_sqlite_odbc()] instead.
#'
#' @keywords internal
access_to_sqlite_mdbtools <- function(accdb, sqlite = ":memory:", tables = NULL, drop = TRUE, verbose = FALSE) {
    # use mdbtools on macOS and Linux
    mdbtools <- c(Sys.which("mdb-tables"), Sys.which("mdb-schema"), Sys.which("mdb-export"))

    if (any(miss <- mdbtools == "")) {
        abort(sprintf(
            paste(
                "'%s' executable was not found on system PATH.",
                "Please install 'mdbtools' and try again."
            ),
            names(miss)[miss][[1L]]
        ), "error_mdbtools_not_found")
    }

    # NOTE: In mdbtools v1.0.1, index and primary key output to the SQLite
    # schema writer backend was added. The feature is enabled by default which
    # complex the schema extraction logic. So here we explicitly disable the
    # feature.
    # See: https://github.com/mdbtools/mdbtools/pull/402
    mdb_schema_ver <- system2(mdbtools["mdb-schema"], "--version", stdout = TRUE, stderr = TRUE)
    if (!is.null(attr(mdb_schema_ver, "status"))) {
        abort("Failed to get the version of 'mdb-schema'.", "error_mdbtools_schema_version_failed")
    }
    mdb_schema_ver <- numeric_version(sub("mdbtools v", "", mdb_schema_ver, fixed = TRUE))
    if (mdb_schema_ver > "1.0.0") {
        mdb_schema_args <- c(
            "--no-drop-table",
            "--no-not-null",
            "--no-default-values",
            "--no-not_empty",
            "--no-comments",
            "--no-relations",
            "--no-indexes"
        )
    } else {
        mdb_schema_args <- c(
            "--no-drop-table",
            "--no-not-null",
            "--no-default-values",
            "--no-not_empty",
            "--no-comments"
        )
    }

    if (!is.null(tables)) {
        tables <- unique(tables)
    } else {
        if (verbose) cli::cli_progress_message("Listing tables from Microsoft Access database...")
        tables <- system2(
            mdbtools["mdb-tables"],
            args = c("-1", shQuote(accdb)),
            stdout = TRUE, stderr = TRUE
        )

        if (!is.null(attr(tables, "status"))) {
            abort(sprintf("Failed to list tables from '%s': %s", accdb, tables), "error_mdb_tables_failed")
        }
    }

    # create a SQLite database connection
    conn <- DBI::dbConnect(RSQLite::SQLite(), sqlite)

    # issue warnings immediately when they occur
    old <- getOption("warn")
    options("warn" = 1L)
    on.exit(options(warn = old), add = TRUE)

    DBI::dbWithTransaction(conn, {
        if (verbose) {
            i <- 0L
            n <- length(tables)
            tbl <- tables[[1L]]
            step <- cli::cli_progress_step(
                "Converting Microsoft Access database to SQLite [{i}/{n} table{?s}]: {.code {tbl}} ",
                "Converting Microsoft Access database to SQLite [{i}/{n} table{?s}]",
                spinner = TRUE
            )
        }
        for (tbl in tables) {
            if (verbose) {
                i <- i + 1L
                cli::cli_progress_update(id = step)
            }

            # drop table if exists
            if (drop) DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS `%s`", tbl))

            # dump the table schema in SQLite format
            schema <- system2(
                mdbtools["mdb-schema"], c(mdb_schema_args, "-T", shQuote(tbl), shQuote(accdb), "sqlite"),
                stdout = TRUE, stderr = TRUE
            )
            if (!is.null(attr(schema, "status"))) {
                abort(
                    sprintf("Failed to dump schema of table '%s' from '%s': %s", tbl, accdb, schema),
                    "error_mdb_schema_failed"
                )
            }
            # remove empty lines
            schema_clean <- schema[nzchar(schema)]
            # remove comments
            schema_clean <- schema_clean[!startsWith(schema_clean, "--")]
            DBI::dbExecute(conn, paste0(schema_clean, collapse = "\n"))

            # export table data in SQLite format
            data <- system2(
                mdbtools["mdb-export"],
                c("-I", "sqlite", "-b", "hex", "-D", "%F", "-T", shQuote("%F %H:%M:%S"), shQuote(accdb), shQuote(tbl)),
                stdout = TRUE, stderr = TRUE
            )
            if (!is.null(attr(data, "status"))) {
                abort(
                    sprintf("Failed to export data of table '%s' from '%s': %s", tbl, accdb, data),
                    "error_mdb_export_failed"
                )
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

            # insert data to the table if not empty
            for (d in data) DBI::dbExecute(conn, paste0(d, collapse = "\n"))
        }
    })

    conn
}
