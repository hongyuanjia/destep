#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
sqlite <- if (length(args)) args[[1L]] else Sys.getenv("DESTEP_SCHEMA_SQLITE", "")

if (!nzchar(sqlite) || !file.exists(sqlite)) {
    stop(
        "Provide a DeST SQLite file as the first argument or via DESTEP_SCHEMA_SQLITE.",
        call. = FALSE
    )
}

table_path <- file.path("inst", "schema", "tables.tsv")
field_path <- file.path("inst", "schema", "fields.tsv")
observation_path <- file.path("inst", "schema", "observations.tsv")
if (!file.exists(table_path) || !file.exists(field_path) || !file.exists(observation_path)) {
    stop("Run this script from the package root.", call. = FALSE)
}

read_schema_tsv <- function(path) {
    utils::read.delim(
        path,
        sep = "\t",
        quote = "",
        comment.char = "",
        na.strings = character(),
        colClasses = "character",
        stringsAsFactors = FALSE,
        check.names = FALSE
    )
}

tables <- read_schema_tsv(table_path)
fields <- read_schema_tsv(field_path)
observations <- read_schema_tsv(observation_path)
con <- DBI::dbConnect(RSQLite::SQLite(), sqlite)
on.exit(DBI::dbDisconnect(con), add = TRUE)

db_tables <- sort(DBI::dbListTables(con))
db_rows <- vapply(db_tables, function(table) {
    DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS N FROM `%s`", table))$N[[1L]]
}, numeric(1))

catalog_tables <- tables$table
missing_tables <- setdiff(catalog_tables, db_tables)
uncataloged_nonempty <- setdiff(db_tables[db_rows > 0], catalog_tables)

cat("Database tables:  ", length(db_tables), "\n", sep = "")
cat("Non-empty tables: ", sum(db_rows > 0), "\n", sep = "")
cat("Catalog tables:   ", length(catalog_tables), "\n", sep = "")

if (length(missing_tables)) {
    cat("\nCatalog tables missing from database:\n")
    cat(paste0("  - ", missing_tables), sep = "\n")
    cat("\n")
}

if (length(uncataloged_nonempty)) {
    cat("\nNon-empty database tables not yet cataloged:\n")
    cat(paste0("  - ", uncataloged_nonempty), sep = "\n")
    cat("\n")
}

column_mismatches <- list()
for (table in intersect(catalog_tables, db_tables)) {
    expected <- fields$field[fields$table == table]
    observed <- DBI::dbListFields(con, table)
    missing_columns <- setdiff(expected, observed)
    new_columns <- setdiff(observed, expected)
    if (length(missing_columns) || length(new_columns)) {
        column_mismatches[[table]] <- list(
            missing_columns = missing_columns,
            new_columns = new_columns
        )
    }
}

row_mismatches <- data.frame(
    source_model = character(),
    table = character(),
    expected_rows = character(),
    actual_rows = character(),
    stringsAsFactors = FALSE
)
for (i in seq_len(nrow(observations))) {
    table <- observations$table[[i]]
    if (!table %in% db_tables || !nzchar(observations$rows[[i]])) next

    count <- db_rows[[table]]
    expected <- as.integer(observations$rows[[i]])
    if (is.na(expected) || count != expected) {
        row_mismatches <- rbind(row_mismatches, data.frame(
            source_model = observations$source_model[[i]],
            table = observations$table[[i]],
            expected_rows = observations$rows[[i]],
            actual_rows = as.character(count),
            stringsAsFactors = FALSE
        ))
    }
}

if (length(column_mismatches)) {
    cat("\nColumn mismatches:\n")
    for (table in names(column_mismatches)) {
        cat("  - ", table, "\n", sep = "")
        if (length(column_mismatches[[table]]$missing_columns)) {
            cat("    missing in database: ",
                paste(column_mismatches[[table]]$missing_columns, collapse = ", "),
                "\n",
                sep = ""
            )
        }
        if (length(column_mismatches[[table]]$new_columns)) {
            cat("    not in catalog: ",
                paste(column_mismatches[[table]]$new_columns, collapse = ", "),
                "\n",
                sep = ""
            )
        }
    }
}

if (nrow(row_mismatches)) {
    cat("\nRow-count mismatches:\n")
    for (i in seq_len(nrow(row_mismatches))) {
        cat(
            "  - ", row_mismatches$table[[i]],
            " (", row_mismatches$source_model[[i]], "): expected ",
            row_mismatches$expected_rows[[i]], ", found ",
            row_mismatches$actual_rows[[i]], "\n",
            sep = ""
        )
    }
}

invisible(list(
    missing_tables = missing_tables,
    uncataloged_nonempty = uncataloged_nonempty,
    column_mismatches = column_mismatches,
    row_mismatches = row_mismatches
))
