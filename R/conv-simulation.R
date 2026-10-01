# Validate user-selected target-engine settings independently of DeST presets.
# Unspecified values retain the converter's existing generated/IDD defaults.
simulation__options <- function(options) {
    if (is.null(options)) {
        return(list())
    }
    checkmate::assert_list(
        options,
        names = "unique",
        .var.name = "simulation settings"
    )
    allowed <- c("terrain", "solar_distribution", "shadow_update_days")
    if (
        length(options) &&
            (is.null(names(options)) ||
                any(!nzchar(names(options))) ||
                any(!names(options) %in% allowed))
    ) {
        stop(
            "Unknown or unnamed simulation setting. Use terrain, solar_distribution or shadow_update_days.",
            call. = FALSE
        )
    }
    if ("terrain" %in% names(options)) {
        checkmate::assert_choice(
            options$terrain,
            c("Country", "Suburbs", "City", "Ocean", "Urban"),
            .var.name = "terrain"
        )
    }
    if ("solar_distribution" %in% names(options)) {
        checkmate::assert_choice(
            options$solar_distribution,
            c(
                "MinimalShadowing",
                "FullExterior",
                "FullInteriorAndExterior",
                "FullExteriorWithReflections",
                "FullInteriorAndExteriorWithReflections"
            ),
            .var.name = "solar_distribution"
        )
    }
    if ("shadow_update_days" %in% names(options)) {
        checkmate::assert_int(
            options$shadow_update_days,
            lower = 1L,
            .var.name = "shadow_update_days"
        )
    }
    options
}

# Select fields from the actual IDD rather than inferring schema from a version
# string. A day interval also selects the corresponding periodic method.
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

# Apply explicit target-engine settings after source objects are assembled.
simulation__apply <- function(ep, options) {
    building <- options[intersect(
        names(options),
        c("terrain", "solar_distribution")
    )]
    if (length(building)) {
        ep$Building$set(building)
    }
    if (!is.null(options$shadow_update_days)) {
        fields <- simulation__shadow_fields(ep$definition(
            "ShadowCalculation"
        )$field_name())
        values <- stats::setNames(
            list(fields$periodic, options$shadow_update_days),
            c(fields$method, fields$frequency)
        )
        # Public conversion starts without a ShadowCalculation object. Updating
        # an existing object also keeps this helper usable for isolated checks.
        if ("ShadowCalculation" %in% ep$to_table()$class) {
            ep$ShadowCalculation$set(values)
        } else {
            ep$add(ShadowCalculation = values)
        }
    }
    invisible(ep)
}

# Read actual singleton fields, resolving omitted fields through the target's
# own IDD. This audit never adds default objects or changes their field values.
simulation__audit <- function(ep, options) {
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
        requested = options,
        effective = effective,
        selection = stats::setNames(
            ifelse(
                names(effective) %in%
                    c(
                        names(options),
                        if ("shadow_update_days" %in% names(options)) {
                            "shadow_update_method"
                        }
                    ),
                "user",
                "retained_default"
            ),
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
    explicit <- names(audit$requested)
    c(
        paste0("destep simulation settings: ", values),
        paste0(
            "destep explicit simulation options: ",
            if (length(explicit)) {
                paste(explicit, collapse = ", ")
            } else {
                "none (retained defaults)"
            }
        )
    )
}
