#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
accdb <- if (length(args) >= 1L) args[[1L]] else Sys.getenv("DESTEP_SCHEMA_ACCDB", "")
output <- if (length(args) >= 2L) args[[2L]] else file.path("inst", "schema", "fields_cn.tsv")
source_model <- if (length(args) >= 3L) {
    args[[3L]]
} else {
    sub("\\.[^.]+$", "", basename(accdb))
}

if (!nzchar(accdb) || !file.exists(accdb)) {
    stop(
        "Provide a DeST Access file as the first argument or via DESTEP_SCHEMA_ACCDB.",
        call. = FALSE
    )
}

required_files <- c(
    file.path("inst", "schema", "fields.tsv"),
    file.path("inst", "schema", "tables.tsv")
)
if (!all(file.exists(required_files))) {
    stop("Run this script from the package root.", call. = FALSE)
}

mdbtools <- c(
    "mdb-tables" = unname(Sys.which("mdb-tables")),
    "mdb-schema" = unname(Sys.which("mdb-schema")),
    "mdb-prop" = unname(Sys.which("mdb-prop"))
)
if (any(!nzchar(mdbtools))) {
    missing <- names(mdbtools)[!nzchar(mdbtools)][[1L]]
    stop("`", missing, "` executable was not found on PATH.", call. = FALSE)
}

run_mdb <- function(command, args, error = TRUE) {
    out <- system2(command, args, stdout = TRUE, stderr = TRUE)
    status <- attr(out, "status")
    if (error && !is.null(status)) {
        stop(paste(out, collapse = "\n"), call. = FALSE)
    }

    out
}

write_schema_tsv <- function(x, file) {
    x[] <- lapply(x, function(column) {
        column[is.na(column)] <- ""
        as.character(column)
    })

    has_embedded_delimiter <- vapply(x, function(column) {
        any(grepl("[\t\r\n]", column))
    }, logical(1L))
    if (any(has_embedded_delimiter)) {
        stop("TSV fields must not contain tabs or line breaks.", call. = FALSE)
    }

    escaped <- x
    escaped[] <- lapply(escaped, function(column) {
        column[!nzchar(column)] <- "\"\""
        column
    })
    lines <- c(
        paste(names(escaped), collapse = "\t"),
        do.call(paste, c(escaped, sep = "\t"))
    )

    con <- file(file, open = "w", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(lines, con, useBytes = TRUE)
}

list_access_tables <- function(accdb) {
    tables <- run_mdb(mdbtools[["mdb-tables"]], c("-1", shQuote(accdb)))
    tables[nzchar(tables)]
}

list_access_fields <- function(accdb, table) {
    schema <- run_mdb(
        mdbtools[["mdb-schema"]],
        c(
            "--no-drop-table",
            "--no-not-null",
            "--no-default-values",
            "--no-not_empty",
            "--no-comments",
            "--no-relations",
            "--no-indexes",
            "-T", shQuote(table),
            shQuote(accdb),
            "sqlite"
        )
    )

    fields <- sub("^\\s*`([^`]+)`.*$", "\\1", schema)
    fields <- fields[fields != schema]
    fields[nzchar(fields)]
}

read_access_descriptions <- function(accdb, table) {
    props <- run_mdb(
        mdbtools[["mdb-prop"]],
        c(shQuote(accdb), shQuote(table)),
        error = FALSE
    )

    if (!is.null(attr(props, "status"))) {
        warning(
            "Failed to read Access properties for table `", table, "`.",
            call. = FALSE
        )
        return(setNames(character(), character()))
    }

    descriptions <- setNames(character(), character())
    current_field <- NULL
    for (line in props) {
        if (startsWith(line, "name: ")) {
            current_field <- sub("^name: ", "", line)
        } else if (!is.null(current_field) &&
            !identical(current_field, "(none)") &&
            startsWith(line, "\tDescription: ")) {
            descriptions[[current_field]] <- trimws(gsub(
                "[\t\r\n]+",
                " ",
                sub("^\tDescription: ", "", line)
            ))
        }
    }

    descriptions
}

tables <- list_access_tables(accdb)
fields <- lapply(tables, function(table) {
    table_fields <- list_access_fields(accdb, table)
    descriptions <- read_access_descriptions(accdb, table)

    data.frame(
        source_model = source_model,
        table = table,
        field = table_fields,
        ordinal = seq_along(table_fields),
        description_cn = unname(descriptions[table_fields]),
        stringsAsFactors = FALSE
    )
})
fields <- do.call(rbind, fields)
fields$description_cn[is.na(fields$description_cn)] <- ""

dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
write_schema_tsv(fields, output)

cat("Wrote ", nrow(fields), " Access field descriptions to ", output, "\n", sep = "")
