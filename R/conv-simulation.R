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
        ),
        # Record solver/reporting boundaries separately from retained object
        # defaults. This metadata does not change timesteps, control or outputs.
        response = list(
            native_reference = "DeST DPE July 2023 verified hourly branch",
            native_time_step_hours = 1L,
            target_zone_timesteps_per_hour = as.integer(read_field(
                "Timestep",
                "Number of Timesteps per Hour"
            )),
            source_history = "Modal history with current and previous source samples; source types have distinct consumers.",
            load_output = "Native hourly control solution differs from target system-step load and averaged hourly rates.",
            peak_alignment = "Peak value and hour are not guaranteed to match; no automatic time shift or load fitting.",
            scope = "A temporal algorithm difference does not classify every unexplained residual as an engine difference."
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
    c(
        paste0("destep simulation settings (retained defaults): ", values),
        sprintf(
            "destep response boundary: target zone timesteps per hour=%s; native reference=%s.",
            audit$response$target_zone_timesteps_per_hour,
            audit$response$native_reference
        ),
        audit$response$source_history,
        audit$response$load_output,
        audit$response$peak_alignment,
        audit$response$scope
    )
}
