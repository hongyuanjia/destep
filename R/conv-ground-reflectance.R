# ENVIRONMENT.GROUND_REFLECT_COEF -> Site:GroundReflectance.
# DeST stores one annual ground-albedo value, while EnergyPlus exposes twelve
# monthly fields, so the source value is repeated without seasonal inference.
ground_reflectance__convert <- function(dest, ep) {
    if (!db_has_rows(dest, "ENVIRONMENT") ||
        !db_has_fields(dest, "ENVIRONMENT", "GROUND_REFLECT_COEF")) {
        return(NULL)
    }

    reflectance <- DBI::dbGetQuery(
        dest,
        paste(
            "SELECT GROUND_REFLECT_COEF AS GROUND_REFLECTANCE",
            "FROM ENVIRONMENT"
        )
    )
    data.table::setDT(reflectance)
    dt_force_numeric(reflectance, "GROUND_REFLECTANCE")

    # EnergyPlus accepts only one Site:GroundReflectance object. Multiple DeST
    # environments therefore need an explicit model-selection rule.
    if (nrow(reflectance) != 1L) {
        stop(sprintf(
            "Expected one ENVIRONMENT row for ground reflectance but found %i.",
            nrow(reflectance)
        ), call. = FALSE)
    }

    value <- reflectance$GROUND_REFLECTANCE[[1L]]
    if (is.na(value)) return(NULL)
    if (!is.finite(value) || value < 0 || value > 1) {
        stop(sprintf(
            "ENVIRONMENT.GROUND_REFLECT_COEF must be within [0, 1], but found %s.",
            format(value)
        ), call. = FALSE)
    }

    out <- conv__add(
        dest, ep,
        "Site:GroundReflectance" := ground_reflectance__value(value)
    )
    attr(out, "table") <- reflectance
    out
}

# Build the twelve monthly EnergyPlus fields from DeST's single annual value.
ground_reflectance__value <- function(reflectance) {
    values <- as.list(rep(reflectance, 12L))
    names(values) <- paste0(
        tolower(month.name),
        "_ground_reflectance"
    )
    values
}
