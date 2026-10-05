# Resolve current and legacy shadow fields for read-only default reporting.
simulation__shadow_fields <- function(fields) {
    if (
        all(
            c(
                "Shading Calculation Update Frequency Method",
                "Shading Calculation Update Frequency"
            ) %in%
                fields
        )
    ) {
        return(list(
            method = "Shading Calculation Update Frequency Method",
            frequency = "Shading Calculation Update Frequency",
            periodic = "Periodic"
        ))
    }
    if (all(c("Calculation Method", "Calculation Frequency") %in% fields)) {
        return(list(
            method = "Calculation Method",
            frequency = "Calculation Frequency",
            periodic = "AverageOverDaysInFrequency"
        ))
    }
    stop("Unsupported ShadowCalculation field layout.", call. = FALSE)
}

# Read actual singleton fields, resolving omitted fields through the target's
# own IDD. This audit never adds default objects or changes their field values.
simulation__audit <- function(ep) {
    table <- ep$to_table()
    read_field <- function(class, field) {
        definition <- ep$definition(class)
        index <- definition$field_index(field)
        value <- table$value[table$class == class & table$index == index]
        if (length(value) > 1L) {
            stop("Ambiguous simulation settings object.", call. = FALSE)
        }
        if (!length(value) || is.na(value) || value == "") {
            return(definition$field_default(field)[[1L]])
        }
        value[[1L]]
    }
    fields <- simulation__shadow_fields(ep$definition(
        "ShadowCalculation"
    )$field_name())
    effective <- list(
        terrain = read_field("Building", "Terrain"),
        solar_distribution = read_field("Building", "Solar Distribution"),
        shadow_update_days = as.integer(read_field(
            "ShadowCalculation",
            fields$frequency
        )),
        shadow_update_method = read_field("ShadowCalculation", fields$method)
    )
    list(
        effective = effective,
        selection = stats::setNames(
            rep("retained_default", length(effective)),
            names(effective)
        )
    )
}

# Persist effective engine settings and their origin alongside mode comments,
# so saving the IDF does not discard the R-only conversion metadata.
simulation__comments <- function(audit) {
    values <- paste(
        names(audit$effective),
        unlist(audit$effective),
        sep = "=",
        collapse = "; "
    )
    paste0("destep simulation settings (retained defaults): ", values)
}
