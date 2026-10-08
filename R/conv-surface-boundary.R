# Reject missing explicit face/plane references before joins turn them into
# incomplete geometry. The condition records the enclosure, field and saved ID;
# the read-only check never changes source ownership or creates missing geometry.
surface__validate_source_references <- function(dest) {
    references <- DBI::dbGetQuery(
        dest,
        "
        SELECT E.ID AS enclosure_id, 'SIDE1' AS field, E.SIDE1 AS reference
        FROM MAIN_ENCLOSURE E
        LEFT JOIN SURFACE S ON E.SIDE1 = S.SURFACE_ID
        WHERE S.SURFACE_ID IS NULL
        UNION ALL
        SELECT E.ID, 'SIDE2', E.SIDE2
        FROM MAIN_ENCLOSURE E
        LEFT JOIN SURFACE S ON E.SIDE2 = S.SURFACE_ID
        WHERE S.SURFACE_ID IS NULL
        UNION ALL
        SELECT E.ID, 'MIDDLE_PLANE', E.MIDDLE_PLANE
        FROM MAIN_ENCLOSURE E
        LEFT JOIN PLANE P ON E.MIDDLE_PLANE = P.PLANE_ID
        WHERE P.PLANE_ID IS NULL
        ORDER BY enclosure_id, field
        "
    )
    if (nrow(references) == 0L) {
        return(invisible(NULL))
    }
    stop(structure(
        list(
            message = paste(
                sprintf(
                    "MAIN_ENCLOSURE %s has missing %s reference (%s).",
                    references$enclosure_id,
                    references$field,
                    references$reference
                ),
                collapse = " "
            ),
            call = NULL,
            references = references
        ),
        class = c("destep_invalid_surface_references", "error", "condition")
    ))
}

# Preserve the saved representative-zone boundary graph independently of storey
# multipliers. Geometry repair may split a face, but never changes its source
# peer, construction, outdoor exposure, or ground contact to balance weights.
surface__preserve_boundaries <- function(
    surface,
    window = data.table::data.table(),
    profile = eplus_geom__profile()
) {
    surface <- data.table::copy(surface)
    for (field in c(
        "TYPE",
        "SIDE",
        "CONSTRUCTION",
        "BOUNDARY",
        "BOUNDARY_OBJECT"
    )) {
        data.table::set(
            surface,
            j = paste0("SOURCE_", field),
            value = surface[[field]]
        )
    }
    data.table::set(surface, j = "BOUNDARY_MODE", value = "source")
    surface <- surface__normalize_room_junctions(surface, window, profile)
    reference <- unique(surface[,
        c("NAME", "BOUNDARY", "BOUNDARY_OBJECT"),
        with = FALSE
    ])
    peer <- match(reference$BOUNDARY_OBJECT, reference$NAME)
    interior <- which(reference$BOUNDARY == "Surface")
    back <- reference$BOUNDARY_OBJECT[peer[interior]]
    if (
        anyNA(peer[interior]) ||
            anyNA(back) ||
            any(reference$BOUNDARY[peer[interior]] != "Surface") ||
            any(back != reference$NAME[interior])
    ) {
        stop(
            "Source surface boundary references must exist and be reciprocal.",
            call. = FALSE
        )
    }
    surface
}

# List each original interzone pair once, regardless of its geometric parts.
# Unequal multiplicities are retained but cannot represent balanced physical
# whole-building transfer merely by weighting the representative zone outputs.
surface__multiplier_pairs <- function(surface) {
    columns <- c(
        "ID",
        "NAME",
        "ROOM",
        "STOREY_ID",
        "STOREY_MULTIPLIER",
        "BOUNDARY",
        "BOUNDARY_OBJECT"
    )
    object <- unique(surface[, columns, with = FALSE], by = "NAME")
    peer <- match(object$BOUNDARY_OBJECT, object$NAME)
    row <- which(
        object$BOUNDARY == "Surface" &
            !is.na(peer) &
            object$ID < object$ID[peer] &
            object$STOREY_MULTIPLIER != object$STOREY_MULTIPLIER[peer]
    )
    result <- data.table::data.table(
        source_surface = object$ID[row],
        peer_source_surface = object$ID[peer[row]],
        room = object$ROOM[row],
        peer_room = object$ROOM[peer[row]],
        storey = object$STOREY_ID[row],
        peer_storey = object$STOREY_ID[peer[row]],
        multiplier = object$STOREY_MULTIPLIER[row],
        peer_multiplier = object$STOREY_MULTIPLIER[peer[row]]
    )
    result <- unique(result, by = c("source_surface", "peer_source_surface"))
    data.table::setorderv(result, c("source_surface", "peer_source_surface"))
    result
}
