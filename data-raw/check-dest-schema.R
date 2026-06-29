#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
sqlite <- if (length(args)) args[[1L]] else Sys.getenv("DESTEP_SCHEMA_SQLITE", "")

if (!nzchar(sqlite) || !file.exists(sqlite)) {
    stop(
        "Provide a DeST SQLite file as the first argument or via DESTEP_SCHEMA_SQLITE.",
        call. = FALSE
    )
}

required_files <- c(
    file.path("R", "schema.R"),
    file.path("inst", "schema", "tables.tsv"),
    file.path("inst", "schema", "fields.tsv"),
    file.path("inst", "schema", "observations.tsv")
)
if (!all(file.exists(required_files))) {
    stop("Run this script from the package root.", call. = FALSE)
}

schema_env <- new.env(parent = globalenv())
source(file.path("R", "schema.R"), local = schema_env)
schema_env$destep_schema_path <- function(file = NULL) {
    if (is.null(file)) {
        return(file.path("inst", "schema"))
    }

    file.path("inst", "schema", file)
}

write_field_groups <- function(title, x) {
    if (!nrow(x)) return(invisible())

    cat("\n", title, ":\n", sep = "")
    fields_by_table <- split(x$field, x$table)
    for (table in names(fields_by_table)) {
        prefix <- paste0("  - ", table, ": ")
        lines <- strwrap(
            paste(fields_by_table[[table]], collapse = ", "),
            width = 78,
            initial = prefix,
            prefix = strrep(" ", nchar(prefix))
        )
        cat(lines, sep = "\n")
        cat("\n")
    }
}

coverage <- schema_env$destep_schema_coverage(sqlite)
tables <- coverage$tables
fields <- coverage$fields

missing_tables <- tables$table[tables$in_catalog & !tables$in_database]
uncataloged_nonempty <- tables$table[!tables$in_catalog & tables$is_non_empty]
missing_fields <- fields[fields$in_catalog & !fields$in_database, ]
new_fields <- fields[fields$in_database & !fields$in_catalog, ]

cat("Database tables:  ", sum(tables$in_database), "\n", sep = "")
cat("Non-empty tables: ", sum(tables$in_database & tables$is_non_empty), "\n", sep = "")
cat("Catalog tables:   ", sum(tables$in_catalog), "\n", sep = "")
cat("Catalog fields:   ", sum(fields$in_catalog), "\n", sep = "")

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

write_field_groups("Catalog fields missing from database", missing_fields)
write_field_groups("Database fields not yet cataloged", new_fields)

invisible(list(
    missing_tables = missing_tables,
    uncataloged_nonempty = uncataloged_nonempty,
    missing_fields = missing_fields,
    new_fields = new_fields,
    coverage = coverage
))
