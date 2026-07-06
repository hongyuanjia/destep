#!/usr/bin/env Rscript

source_fields <- file.path("inst", "schema", "fields.tsv")
source_fields_cn <- file.path("inst", "schema", "fields_cn.tsv")
source_terms <- file.path("tools", "field-terms.tsv")

read_schema_tsv <- function(file) {
    x <- utils::read.delim(
        file,
        sep = "\t",
        quote = "",
        comment.char = "",
        na.strings = character(),
        colClasses = "character",
        check.names = FALSE
    )
    x[] <- lapply(x, function(column) {
        column[column == "\"\""] <- ""
        column
    })
    x
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

required_files <- c(source_fields, source_fields_cn, source_terms)
if (!all(file.exists(required_files))) {
    missing <- required_files[!file.exists(required_files)][[1L]]
    stop("Required schema tool input does not exist: ", missing, call. = FALSE)
}

fields <- read_schema_tsv(source_fields)
fields_cn <- read_schema_tsv(source_fields_cn)
terms <- read_schema_tsv(source_terms)

if (!identical(names(terms), c("description_cn", "semantics"))) {
    stop(
        "`tools/field-terms.tsv` must have columns: description_cn, semantics.",
        call. = FALSE
    )
}
if (any(!nzchar(terms$description_cn)) || any(!nzchar(terms$semantics))) {
    stop("Translation terms must not contain empty cells.", call. = FALSE)
}
if (anyDuplicated(terms$description_cn)) {
    stop("Translation terms must have unique `description_cn` values.", call. = FALSE)
}
if (any(grepl("[\u4e00-\u9fff]", terms$semantics))) {
    stop("Translation `semantics` values must be English.", call. = FALSE)
}

field_key <- paste(fields$table, fields$field, sep = "\r")
field_cn_key <- paste(fields_cn$table, fields_cn$field, sep = "\r")
description_cn <- fields_cn$description_cn[match(field_key, field_cn_key)]

if (anyNA(description_cn) || any(!nzchar(description_cn))) {
    stop("Every catalog field must have a non-empty Access field description.", call. = FALSE)
}

needs_translation <- sort(unique(description_cn[grepl("[\u4e00-\u9fff]", description_cn)]))
missing <- setdiff(needs_translation, terms$description_cn)
if (length(missing)) {
    stop(
        paste(
            c("Missing English translations for Access descriptions:", paste0("- ", missing)),
            collapse = "\n"
        ),
        call. = FALSE
    )
}

translation <- setNames(terms$semantics, terms$description_cn)
translate_description <- function(x) {
    if (!grepl("[\u4e00-\u9fff]", x)) {
        return(x)
    }

    unname(translation[[x]])
}

fields$semantics <- vapply(description_cn, translate_description, character(1L))
fields$notes <- sub("^Access Description from CoA_Chongqin_2015; ?", "", fields$notes)
fields$notes[fields$notes == "Access Description from CoA_Chongqin_2015."] <- ""

write_schema_tsv(fields, source_fields)

cat("Updated ", nrow(fields), " catalog field semantics from Access descriptions.\n", sep = "")
