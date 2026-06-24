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
