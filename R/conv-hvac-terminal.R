# Read a named integer property from linked source records. Traversal is
# sequential by definition; preallocate once and reject cycles or broken keys.
hvac__linked_integer_property <- function(dest, roots, name) {
    output <- rep(NA_integer_, length(roots))
    active <- which(!is.na(roots) & roots != 0L)
    if (!length(active)) {
        return(output)
    }
    hvac__assert_source_table(
        dest,
        "EXT_PROPERTY",
        c("PROPERTY_ID", "NEXT_PROPERTY", "NAME", "DATA_LONG"),
        "The DeST model"
    )
    properties <- DBI::dbGetQuery(
        dest,
        "SELECT PROPERTY_ID, NEXT_PROPERTY, NAME, DATA_LONG FROM EXT_PROPERTY"
    )
    duplicate <- unique(properties$PROPERTY_ID[duplicated(
        properties$PROPERTY_ID
    )])
    # Shared roots are visited once; results remain aligned with the caller.
    distinct_roots <- unique(roots[active])
    values <- rep(NA_integer_, length(distinct_roots))
    for (i in seq_along(distinct_roots)) {
        pointer <- distinct_roots[[i]]
        visited <- rep(FALSE, nrow(properties))
        found <- FALSE
        while (pointer != 0L) {
            row <- match(pointer, properties$PROPERTY_ID)
            if (is.na(row) || pointer %in% duplicate || visited[[row]]) {
                abort(
                    sprintf(
                        "Invalid EXT_PROPERTY chain at property %s.",
                        pointer
                    ),
                    class = "destep_unresolved_hvac_property"
                )
            }
            visited[[row]] <- TRUE
            if (is.na(properties$NAME[[row]])) {
                abort(
                    "EXT_PROPERTY.NAME must not be missing in a referenced chain.",
                    class = "destep_unresolved_hvac_property"
                )
            }
            if (properties$NAME[[row]] == name) {
                value <- properties$DATA_LONG[[row]]
                if (
                    found ||
                        is.na(value) ||
                        !is.finite(value) ||
                        value != trunc(value) ||
                        abs(value) > .Machine$integer.max
                ) {
                    abort(
                        sprintf(
                            "Property %s must occur once with a finite integer value.",
                            name
                        ),
                        class = "destep_unresolved_hvac_property"
                    )
                }
                values[[i]] <- as.integer(value)
                found <- TRUE
            }
            pointer <- properties$NEXT_PROPERTY[[row]]
            if (is.na(pointer)) {
                abort(
                    "EXT_PROPERTY.NEXT_PROPERTY must end with zero, not NA.",
                    class = "destep_unresolved_hvac_property"
                )
            }
        }
    }
    output[active] <- values[match(roots[active], distinct_roots)]
    output
}

# ROOM.SET_TERMINAL_MAX is total W per room, independent of AHU reheat.
# Explicit ROOM properties take precedence. An absent type uses a disclosed
# converter electric default, informed by the audited installation, without
# claiming that every DeST version or runtime shares this default.
hvac__room_terminal_source <- function(dest, room_ids) {
    checkmate::assert_integerish(room_ids, any.missing = FALSE, unique = TRUE)
    hvac__assert_source_table(
        dest,
        "ROOM",
        c("ID", "SET_TERMINAL_MAX"),
        "The DeST model",
        TRUE
    )
    rooms <- DBI::dbReadTable(dest, "ROOM")
    hvac__assert_unique_key(rooms$ID, "ROOM.ID")
    rows <- match(room_ids, rooms$ID)
    if (anyNA(rows)) {
        abort(
            "Unresolved ROOM terminal references.",
            class = "destep_unresolved_hvac_terminal"
        )
    }
    rooms <- rooms[rows, , drop = FALSE]
    capacity <- as.numeric(rooms$SET_TERMINAL_MAX)
    if (anyNA(capacity) || any(!is.finite(capacity)) || any(capacity < 0)) {
        abort(
            "ROOM.SET_TERMINAL_MAX must be finite nonnegative total watts per room.",
            class = "destep_invalid_hvac_terminal_capacity"
        )
    }
    roots <- if ("EXT_PROPERTY" %in% names(rooms)) {
        rooms$EXT_PROPERTY
    } else {
        rep(0L, nrow(rooms))
    }
    type <- hvac__linked_integer_property(dest, roots, "ROOM_REHEATER_TYPE")
    explicit <- !is.na(type)
    if (any(explicit & !type %in% 0:2)) {
        abort(
            "ROOM_REHEATER_TYPE must be 0 (none), 1 (electric), or 2 (hot water).",
            class = "destep_invalid_hvac_terminal_type"
        )
    }
    # The GUI key alone does not establish ROOM_GROUP property ownership.
    # Detect such records rather than silently ignoring a possibly active type.
    if (
        all(c("OF_ROOM_GROUP") %in% names(rooms)) &&
            db_has_fields(
                dest,
                "ROOM_GROUP",
                c("ROOM_GROUP_ID", "EXT_PROPERTY")
            )
    ) {
        groups <- DBI::dbReadTable(dest, "ROOM_GROUP")
        hvac__assert_unique_key(
            groups$ROOM_GROUP_ID,
            "ROOM_GROUP.ROOM_GROUP_ID"
        )
        group_roots <- groups$EXT_PROPERTY[match(
            rooms$OF_ROOM_GROUP,
            groups$ROOM_GROUP_ID
        )]
        group_type <- hvac__linked_integer_property(
            dest,
            group_roots,
            "ROOM_REHEATER_TYPE"
        )
        if (any(!is.na(group_type))) {
            abort(
                "ROOM_GROUP contains ROOM_REHEATER_TYPE; its ownership/inheritance is not yet verified.",
                class = "destep_unresolved_hvac_terminal"
            )
        }
    }
    missing <- !explicit & capacity > 0
    type[!explicit] <- data.table::fifelse(capacity[!explicit] > 0, 1L, 0L)
    data.table::data.table(
        room_id = as.integer(room_ids),
        terminal_capacity_w = capacity,
        terminal_type = as.integer(type),
        terminal_type_origin = data.table::fcase(
            explicit , "source_room_property"       ,
            missing  , "converter_default_electric" ,
            default = "zero_source_capacity"
        ),
        terminal_has_reheat = capacity > 0 & type != 0L
    )
}

# Derive zone OA from source system total and terminal operating flow limits.
# This preserves the system total; it is not a DeST hourly allocation solver.
# Optional explicit allocations retain their former per-system checks.
hvac__terminal_outdoor_air <- function(source, allocation = NULL) {
    zones <- data.table::copy(source$zones)
    room_ids <- as.character(zones$room_id)
    total <- source$system$outdoor_air_flow_m3_s[[1L]]
    checkmate::assert_number(total, finite = TRUE, lower = 0)
    if (is.null(allocation)) {
        supply <- zones$maximum_supply_flow_m3_s
        checkmate::assert_numeric(
            supply,
            any.missing = FALSE,
            finite = TRUE,
            lower = 0
        )
        if (!length(supply) || sum(supply) <= 0) {
            abort(
                "Cannot allocate source outdoor air without positive design supply flow.",
                class = "destep_invalid_hvac_air_balance"
            )
        }
        minimum <- zones$source_minimum_outdoor_air_flow_m3_s
        if (is.null(minimum)) {
            minimum <- rep(0, length(supply))
        }
        checkmate::assert_numeric(
            minimum,
            len = length(supply),
            any.missing = FALSE,
            finite = TRUE,
            lower = 0
        )
        if (any(minimum > supply) || sum(minimum) > total + 1e-8) {
            abort(
                "ROOM minimum outdoor-air requirements conflict with source system or design supply limits.",
                class = "destep_invalid_hvac_air_balance"
            )
        }
        # Preserve declared room minima and distribute only the remaining
        # system total over available minimum operating supply capacity. For
        # VAV, design-flow shares alone can exceed a zone's minimum supply;
        # CAV minimum and maximum flows coincide. This is a target allocation.
        remaining <- max(0, total - sum(minimum))
        limit <- zones$minimum_supply_flow_m3_s
        if (is.null(limit)) {
            limit <- supply
        }
        checkmate::assert_numeric(
            limit,
            len = length(supply),
            any.missing = FALSE,
            finite = TRUE,
            lower = 0
        )
        if (any(minimum > limit) || total > sum(limit) + 1e-8) {
            abort(
                "Fixed outdoor air exceeds terminal minimum operating supply limits.",
                class = "destep_invalid_hvac_air_balance"
            )
        }
        available <- limit - minimum
        if (remaining > 0 && sum(available) <= 0) {
            abort(
                "Source system outdoor air exceeds available design supply flow.",
                class = "destep_invalid_hvac_air_balance"
            )
        }
        values <- minimum
        if (remaining > 0) {
            values <- values + remaining * available / sum(available)
        }
        # Close floating-point summation only; never normalize conflicting source values.
        values[[length(values)]] <- values[[length(values)]] +
            total -
            sum(values)
        origin <- if (any(minimum > 0)) {
            "source_room_minima_plus_system_remainder"
        } else {
            "source_system_total_by_terminal_supply_share"
        }
    } else {
        checkmate::assert_numeric(
            allocation,
            lower = 0,
            finite = TRUE,
            any.missing = FALSE,
            names = "unique"
        )
        checkmate::assert_subset(room_ids, names(allocation))
        values <- as.numeric(allocation[room_ids])
        checkmate::assert_true(isTRUE(all.equal(
            sum(values),
            total,
            tolerance = 1e-8
        )))
        origin <- "user_override"
    }
    ceiling <- zones$minimum_supply_flow_m3_s
    if (is.null(ceiling)) {
        ceiling <- zones$maximum_supply_flow_m3_s
    }
    if (any(values > ceiling + 1e-8)) {
        abort(
            "Zone outdoor air exceeds source minimum operating supply flow.",
            class = "destep_invalid_hvac_air_balance"
        )
    }
    data.table::set(zones, NULL, "outdoor_air_flow_m3_s", values)
    data.table::set(
        zones,
        NULL,
        "outdoor_air_allocation_origin",
        rep(origin, nrow(zones))
    )
    source$zones <- zones
    source
}

# Emit one disclosure only after successful object assembly; input readers
# remain pure and repeated validation does not repeat default warnings.
hvac__warn_terminal_defaults <- function(terminals) {
    assumed <- terminals$room_id[
        terminals$terminal_type_origin == "converter_default_electric"
    ]
    if (length(assumed)) {
        warn(
            paste0(
                "ROOM_REHEATER_TYPE is absent for ROOM IDs ",
                fmt_integer_sample(assumed),
                "; using the converter electric-terminal default while preserving source capacity in W."
            ),
            class = "destep_assumed_hvac_terminal_type"
        )
    }
    invisible(NULL)
}

# Persist source units, explicit/default type and derived OA allocation in
# saved IDFs as well as the richer in-memory conversion audit.
hvac__terminal_comments <- function(terminals) {
    if (is.null(terminals) || !nrow(terminals)) {
        return(character())
    }
    sprintf(
        "destep ROOM %s terminal: capacity=%g W; type=%s; origin=%s; OA=%g m3/s; allocation=%s",
        terminals$room_id,
        terminals$terminal_capacity_w,
        terminals$terminal_type,
        terminals$terminal_type_origin,
        terminals$outdoor_air_flow_m3_s,
        terminals$outdoor_air_allocation_origin
    )
}
