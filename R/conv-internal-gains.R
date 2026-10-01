# Collect source room-type gains under the explicitly selected people mode.
internal_gains__convert <- function(dest, ep, people_heat = "constant") {
    conv <- Filter(
        Negate(is.null),
        list(
            PEOPLE = people__convert(dest, ep, people_heat),
            LIGHTS = light__convert(dest, ep),
            EQUIPMENT = equipment__convert(dest, ep)
        )
    )

    out <- conv__combine_outputs(conv)
    if (is.null(out)) {
        return(NULL)
    }

    # All three object families are projections of the same room-type record.
    data.table::set(attr(out, "table"), NULL, "SOURCE_TABLE", "ROOM_TYPE_DATA")
    # Each owning converter supplies its source metadata alongside the objects.
    attr(out, "sources") <- unname(unlist(
        lapply(conv, attr, "sources"),
        recursive = FALSE
    ))
    out
}

# Internal gains used by Calload exist only when rooms can select a room-type
# template; per-room drawing objects are not the authoritative numeric source.
internal_gains__has_room_type_data <- function(dest) {
    db_has_rows(dest, "ROOM") && db_has_rows(dest, "ROOM_TYPE_DATA")
}

# Fail on dangling room-type, schedule, or distribution references before an
# invalid or silently incomplete EnergyPlus gain object can be generated.
internal_gains__assert_references <- function(gain, label) {
    missing_type <- is.na(gain$ROOM_TYPE_DATA_ID)
    active <- !is.na(gain$ACTIVE) & gain$ACTIVE != 0L
    missing_schedule <- active &
        !missing_type &
        (is.na(gain$SCHEDULE_ID) |
            gain$SCHEDULE_ID == 0L |
            is.na(gain$SCHEDULE_NAME))
    missing_distribution <- active &
        !missing_type &
        (is.na(gain$DIST_MODE_ID) | is.na(gain$FRACTION_RADIANT))
    invalid_basis <- active &
        !missing_type &
        (is.na(gain$CALCULATION_BASIS) |
            !gain$CALCULATION_BASIS %in% c(0L, 1L))
    invalid <- missing_type |
        missing_schedule |
        missing_distribution |
        invalid_basis
    if (!any(invalid)) {
        return(invisible(NULL))
    }

    rows <- gain[invalid]
    detail <- paste(
        sprintf(
            paste0(
                "%s: TYPE=%s, SCHEDULE=%s, DIST_MODE=%s, ",
                "CALCULATION_BASIS=%s"
            ),
            rows$ROOM_NAME,
            rows$ROOM_TYPE_ID,
            rows$SCHEDULE_ID,
            rows$DIST_MODE_ID,
            rows$CALCULATION_BASIS
        ),
        collapse = "; "
    )
    stop(
        sprintf(
            "Cannot resolve supported ROOM_TYPE_DATA %s reference(s): %s",
            label,
            detail
        ),
        call. = FALSE
    )
}

# Split one DeST internal gain into a scheduled maximum-minus-minimum object and
# an optional always-on minimum object, with shared source-value validation.
internal_gains__split_minimum <- function(
    gain,
    i,
    max_value,
    min_value,
    field,
    always_on,
    value_factory
) {
    min_value <- internal_gains__zero_if_na(min_value)
    name <- gain$NAME[[i]]
    if (!is.finite(max_value) || !is.finite(min_value)) {
        stop(
            sprintf(
                "Internal gain '%s' has a non-finite minimum or maximum value.",
                name
            ),
            call. = FALSE
        )
    }
    if (min_value > max_value) {
        stop(
            sprintf(
                "Internal gain '%s' minimum (%s) exceeds maximum (%s).",
                name,
                min_value,
                max_value
            ),
            call. = FALSE
        )
    }

    variable_value <- max_value - min_value
    out <- list()
    if (variable_value > 0 || min_value <= 0) {
        value <- value_factory(
            gain,
            i,
            name,
            gain$SCHEDULE_NAME[[i]]
        )
        value[[field]] <- variable_value
        out <- c(out, list(value))
    }
    if (min_value > 0) {
        value <- value_factory(
            gain,
            i,
            paste(name, "Minimum"),
            always_on
        )
        value[[field]] <- min_value
        out <- c(out, list(value))
    }
    out
}

# Resolve the version-specific zone-reference label shared by internal-gain
# objects from field 2 of the selected target IDD class.
internal_gains__zone_field_name <- function(ep, class) {
    conv__idd_field_name(ep, class, 2L)
}

# Describe the exact named values just created by an internal-gain converter.
# This does not parse an IDF: object identity, minimum splits and design levels
# come from that converter's own value lists; source fractions come from DeST.
# The owner supplies its schedule field and any domain-specific metadata.
internal_gains__source_specs <- function(
    dest,
    gain,
    values,
    kind,
    total_field,
    area_field,
    schedule_field,
    decorate
) {
    columns <- c(
        "DIST_MODE_ID",
        "DIST_AIR",
        "DIST_AROUND",
        "DIST_FLOOR",
        "DIST_ROOF"
    )
    # Older partial schemas can still use the existing gain conversion, but
    # cannot supply a complete prescribed surface distribution. Never invent
    # missing fractions; the source projector rejects this absent inventory.
    if (!db_has_fields(dest, "DIST_MODE", columns)) {
        return(NULL)
    }
    distributions <- DBI::dbGetQuery(
        dest,
        paste("SELECT", paste(columns, collapse = ","), "FROM DIST_MODE")
    )
    rooms <- DBI::dbGetQuery(dest, "SELECT ID, AREA FROM ROOM")
    # Match complete vectors once. Normal object names take precedence over
    # the minimum suffix, and match() preserves the first duplicate-key match.
    value_names <- vapply(values, `[[`, character(1L), "name")
    row <- match(value_names, gain$NAME)
    missing <- is.na(row)
    if (any(missing)) {
        row[missing] <- match(value_names[missing], paste(gain$NAME, "Minimum"))
    }
    stopifnot(!is.na(row))
    source_rows <- row
    areas <- rooms$AREA[match(gain$ROOM_ID, rooms$ID)]
    mode_rows <- match(gain$DIST_MODE_ID, distributions$DIST_MODE_ID)
    # Build only the referenced distributions, once each. An NA index keeps
    # the existing missing-reference result, including an empty source table.
    mode_indices <- unique(mode_rows)
    modes <- lapply(mode_indices, function(i) {
        stats::setNames(
            as.list(distributions[i, -1L]),
            c("air", "wall", "floor", "roof")
        )
    })
    mode_rows <- match(mode_rows, mode_indices)
    out <- lapply(seq_along(values), function(i) {
        value <- values[[i]]
        row <- source_rows[[i]]
        per_area <- gain$CALCULATION_BASIS[[row]] == 1L
        power <- if (per_area) {
            value[[area_field]] * areas[[row]]
        } else {
            value[[total_field]]
        }
        item <- list(
            name = value$name,
            zone = gain$ROOM_NAME[[row]],
            kind = kind,
            design_power = power,
            schedule = value[[schedule_field]],
            mode = modes[[mode_rows[[row]]]],
            existing_radiant = value$fraction_radiant,
            existing_air = 1 - value$fraction_radiant,
            companion_objects = character()
        )
        # Domain-specific heat and companion metadata belongs to the owner.
        decorate(item, row)
    })
    # Index-based assembly must retain names on caller-supplied value lists.
    names(out) <- names(values)
    out
}

# Identify assigned positive minima that require an always-on companion.
internal_gains__has_positive_minimum <- function(x) {
    !is.na(x) & x > 0
}

# Preserve the established zero convention for absent optional minima.
internal_gains__zero_if_na <- function(x) {
    if (is.na(x)) 0 else x
}

# Create the constant schedule shared by minimum gain components.
internal_gains__always_on <- function(dest, ep, name) {
    conv__add(
        dest,
        ep,
        "Schedule:Constant" := list(
            name = name,
            schedule_type_limits_name = NULL,
            hourly_value = 1
        )
    )
}
