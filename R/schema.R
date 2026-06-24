#' Summarize DeST schema coverage for a SQLite model
#'
#' `destep_schema_coverage()` compares the tables and fields in a DeST SQLite
#' database against the schema catalog shipped with destep. It is intended as a
#' development diagnostic for tracking which DeST tables are cataloged, which
#' non-empty tables are not cataloged yet, and whether cataloged fields drift
#' from real models.
#'
#' @param dest A DBI connection or a path to a SQLite database produced from a
#'   DeST model. Access `.accdb` and `.mdb` files are not read directly; convert
#'   them first with [read_dest()].
#'
#' @return A named list with two data frames:
#' \describe{
#'   \item{`tables`}{Table-level coverage, with one row for each table in the
#'     database or `tables.tsv`.}
#'   \item{`fields`}{Field-level coverage, with one row for each field in the
#'     database or `fields.tsv`.}
#' }
#' @export
destep_schema_coverage <- function(dest) {
    conn <- destep_schema_connection(dest)
    con <- conn$con
    if (isTRUE(conn$disconnect)) {
        on.exit(DBI::dbDisconnect(con), add = TRUE)
    }

    list(
        tables = destep_schema_coverage_tables(con),
        fields = destep_schema_coverage_fields(con)
    )
}

destep_schema_path <- function(file = NULL) {
    if (is.null(file)) {
        return(system.file("schema", package = "destep", mustWork = TRUE))
    }

    system.file("schema", file, package = "destep", mustWork = TRUE)
}

destep_schema_catalog <- function() {
    list(
        tables = destep_schema_tables(),
        fields = destep_schema_fields(),
        observations = destep_schema_observations()
    )
}

destep_schema_tables <- function() {
    destep_read_schema_tsv("tables.tsv")
}

destep_schema_fields <- function() {
    destep_read_schema_tsv("fields.tsv")
}

destep_schema_observations <- function() {
    destep_read_schema_tsv("observations.tsv")
}

destep_read_schema_tsv <- function(file) {
    utils::read.delim(
        destep_schema_path(file),
        sep = "\t",
        quote = "",
        comment.char = "",
        na.strings = character(),
        colClasses = "character",
        stringsAsFactors = FALSE,
        check.names = FALSE
    )
}

destep_schema_connection <- function(dest) {
    if (inherits(dest, "DBIConnection")) {
        if (!DBI::dbIsValid(dest)) {
            stop("`dest` is not a valid DBI connection.", call. = FALSE)
        }

        return(list(con = dest, disconnect = FALSE))
    }

    if (!is.character(dest) || length(dest) != 1L || is.na(dest)) {
        stop(
            "`dest` must be a DBI connection or a path to a SQLite database.",
            call. = FALSE
        )
    }

    ext <- tolower(tools::file_ext(dest))
    if (ext %in% c("accdb", "mdb")) {
        stop(
            paste(
                "`dest` must be a DBI connection or a SQLite database.",
                "Convert Access files with read_dest() first."
            ),
            call. = FALSE
        )
    }

    if (!file.exists(dest)) {
        stop("SQLite database does not exist: ", dest, call. = FALSE)
    }

    list(con = DBI::dbConnect(RSQLite::SQLite(), dest), disconnect = TRUE)
}

destep_schema_coverage_tables <- function(con) {
    catalog_tables <- destep_schema_tables()
    db_tables <- sort(DBI::dbListTables(con))
    db_rows <- vapply(
        db_tables,
        function(table) destep_schema_count_rows(con, table),
        integer(1)
    )

    table_key <- data.frame(
        table = sort(unique(c(catalog_tables$table, db_tables))),
        stringsAsFactors = FALSE
    )
    db_info <- data.frame(
        table = db_tables,
        in_database = rep(TRUE, length(db_tables)),
        rows = unname(db_rows),
        stringsAsFactors = FALSE
    )

    coverage <- merge(table_key, db_info, by = "table", all.x = TRUE, sort = FALSE)
    coverage <- merge(
        coverage,
        catalog_tables,
        by = "table",
        all.x = TRUE,
        sort = FALSE
    )

    coverage$in_database[is.na(coverage$in_database)] <- FALSE
    coverage$in_catalog <- coverage$table %in% catalog_tables$table
    coverage$is_non_empty <- !is.na(coverage$rows) & coverage$rows > 0L
    coverage$is_converted <- !is.na(coverage$status) &
        coverage$status == "converted"

    coverage <- coverage[order(coverage$table), c(
        "table", "in_database", "rows", "is_non_empty", "in_catalog",
        "status", "category", "converter", "used_by", "energyplus_classes",
        "is_converted", "notes"
    )]
    row.names(coverage) <- NULL
    coverage
}

destep_schema_coverage_fields <- function(con) {
    catalog_fields <- destep_schema_fields()
    catalog_fields$ordinal <- as.integer(catalog_fields$ordinal)

    db_fields <- destep_schema_database_fields(con)
    field_key <- unique(rbind(
        catalog_fields[c("table", "field")],
        db_fields[c("table", "field")]
    ))

    coverage <- merge(
        field_key,
        db_fields,
        by = c("table", "field"),
        all.x = TRUE,
        sort = FALSE
    )
    coverage <- merge(
        coverage,
        catalog_fields,
        by = c("table", "field"),
        all.x = TRUE,
        sort = FALSE
    )

    coverage$in_database <- destep_schema_field_key(coverage) %in%
        destep_schema_field_key(db_fields)
    coverage$in_catalog <- destep_schema_field_key(coverage) %in%
        destep_schema_field_key(catalog_fields)
    coverage$ordinal <- ifelse(
        !is.na(coverage$ordinal),
        coverage$ordinal,
        coverage$db_ordinal
    )
    coverage$ordinal <- as.integer(coverage$ordinal)

    coverage <- coverage[order(
        coverage$table,
        is.na(coverage$ordinal),
        coverage$ordinal,
        coverage$field
    ), c(
        "table", "field", "ordinal", "in_database", "in_catalog",
        "role", "semantics", "notes"
    )]
    row.names(coverage) <- NULL
    coverage
}

destep_schema_count_rows <- function(con, table) {
    table <- as.character(DBI::dbQuoteIdentifier(con, table))
    as.integer(DBI::dbGetQuery(con, paste0(
        "SELECT COUNT(*) AS N FROM ", table
    ))$N[[1L]])
}

destep_schema_database_fields <- function(con) {
    db_tables <- sort(DBI::dbListTables(con))
    fields <- lapply(db_tables, function(table) {
        names <- DBI::dbListFields(con, table)
        if (!length(names)) {
            return(data.frame(
                table = character(),
                field = character(),
                db_ordinal = integer(),
                stringsAsFactors = FALSE
            ))
        }

        data.frame(
            table = rep(table, length(names)),
            field = names,
            db_ordinal = seq_along(names),
            stringsAsFactors = FALSE
        )
    })

    fields <- Filter(nrow, fields)
    if (!length(fields)) {
        return(data.frame(
            table = character(),
            field = character(),
            db_ordinal = integer(),
            stringsAsFactors = FALSE
        ))
    }

    do.call(rbind, fields)
}

destep_schema_field_key <- function(x) {
    paste(x$table, x$field, sep = "\r")
}
