# Restore the redundant window-side binding only from the corresponding saved
# host side. The caller owns this conversion database; source files stay intact
# under to_idf()'s default copy=TRUE. Validate the complete repair before writing.
window__restore_bindings <- function(dest) {
    audit <- data.table::data.table(
        window_id = integer(),
        side = integer(),
        surface_id = integer(),
        enclosure_id = integer(),
        host_surface_id = integer(),
        original_of_room = integer(),
        original_type = integer(),
        restored_of_room = integer(),
        restored_type = integer()
    )
    if (!DBI::dbExistsTable(dest, "WINDOW")) {
        return(audit)
    }
    if (DBI::dbGetQuery(dest, "SELECT COUNT(*) AS N FROM WINDOW")$N == 0L) {
        return(audit)
    }

    surfaces <- window__binding_source(
        dest,
        "SURFACE",
        c("SURFACE_ID", "OF_ROOM", "TYPE")
    )
    # -1 is the observed unbound sentinel. NULL or another invalid reference
    # is not evidence for this normalization and must not be guessed.
    missing <- which(surfaces$OF_ROOM == -1L)
    if (length(missing) == 0L) {
        return(audit)
    }
    windows <- window__binding_source(
        dest,
        "WINDOW",
        c("ID", "OF_ENCLOSURE", "SIDE1", "SIDE2")
    )
    enclosures <- window__binding_source(
        dest,
        "MAIN_ENCLOSURE",
        c("ID", "SIDE1", "SIDE2")
    )
    window__binding_unique(surfaces$SURFACE_ID, "SURFACE.SURFACE_ID")
    window__binding_unique(windows$ID, "WINDOW.ID")
    window__binding_unique(enclosures$ID, "MAIN_ENCLOSURE.ID")

    # Keep both sides as separate references, including repeated references.
    # A join or unique() must not hide a face shared by multiple openings.
    side_ids <- c(windows$SIDE1, windows$SIDE2)
    references <- match(side_ids, surfaces$SURFACE_ID)
    counts <- tabulate(references, nbins = nrow(surfaces))
    if (any(counts[missing] != 1L)) {
        window__binding_error(
            "each unbound surface must belong to exactly one window side",
            surfaces$SURFACE_ID[missing[counts[missing] != 1L]]
        )
    }
    rows <- match(missing, references)
    window_rows <- (rows - 1L) %% nrow(windows) + 1L
    side <- (rows - 1L) %/% nrow(windows) + 1L
    enclosure_rows <- match(windows$OF_ENCLOSURE[window_rows], enclosures$ID)
    host_ids <- data.table::fifelse(
        side == 1L,
        enclosures$SIDE1[enclosure_rows],
        enclosures$SIDE2[enclosure_rows]
    )
    host_rows <- match(host_ids, surfaces$SURFACE_ID)
    invalid_host <- is.na(host_rows) |
        is.na(enclosure_rows) |
        host_ids == surfaces$SURFACE_ID[missing] |
        host_ids %in% side_ids
    if (any(invalid_host)) {
        window__binding_error(
            "the matching host side must resolve to a distinct enclosure surface",
            surfaces$SURFACE_ID[missing[invalid_host]]
        )
    }
    # An affected window's other side must also agree with its matching host
    # whenever it already has a binding. Restore gaps, never overwrite a conflict.
    affected_windows <- unique(window_rows)
    affected_hosts <- match(
        windows$OF_ENCLOSURE[affected_windows],
        enclosures$ID
    )
    for (field in c("SIDE1", "SIDE2")) {
        face <- match(windows[[field]][affected_windows], surfaces$SURFACE_ID)
        host <- match(enclosures[[field]][affected_hosts], surfaces$SURFACE_ID)
        if (anyNA(face) || anyNA(host) || anyNA(surfaces$OF_ROOM[face])) {
            window__binding_error(
                "an affected window has an unresolved opposite side"
            )
        }
        bound <- which(surfaces$OF_ROOM[face] != -1L)
        if (
            anyNA(surfaces$TYPE[face[bound]]) ||
                anyNA(surfaces$OF_ROOM[host[bound]]) ||
                anyNA(surfaces$TYPE[host[bound]]) ||
                any(
                    surfaces$OF_ROOM[face[bound]] !=
                        surfaces$OF_ROOM[host[bound]]
                ) ||
                any(surfaces$TYPE[face[bound]] != surfaces$TYPE[host[bound]])
        ) {
            window__binding_error(
                "an existing window-side binding conflicts with its host"
            )
        }
    }
    # A window face may not double as an enclosure or door face. Updating such
    # a record would change a different object even with a unique window owner.
    other_faces <- c(enclosures$SIDE1, enclosures$SIDE2)
    if (DBI::dbExistsTable(dest, "DOOR")) {
        doors <- window__binding_source(dest, "DOOR", c("SIDE1", "SIDE2"))
        other_faces <- c(other_faces, doors$SIDE1, doors$SIDE2)
    }
    if (any(surfaces$SURFACE_ID[missing] %in% other_faces)) {
        window__binding_error(
            "an unbound window face is also referenced by an enclosure or door",
            surfaces$SURFACE_ID[missing][
                surfaces$SURFACE_ID[missing] %in% other_faces
            ]
        )
    }

    owner <- surfaces$OF_ROOM[host_rows]
    type <- surfaces$TYPE[host_rows]
    if (anyNA(owner) || any(owner < 0L) || any(!type %in% 0:2)) {
        window__binding_error(
            "the host must have a valid room, outdoor or ground binding",
            surfaces$SURFACE_ID[missing]
        )
    }
    # Validate owner identity in its own namespace: ROOM and OUTSIDE IDs can
    # overlap, so existence in an unrelated table is insufficient evidence.
    owner_tables <- c("ROOM", "OUTSIDE", "GROUND")
    owner_keys <- c("ID", "OUTSIDE_ID", "GROUND_ID")
    for (boundary in unique(type)) {
        index <- boundary + 1L
        owners <- window__binding_source(
            dest,
            owner_tables[index],
            owner_keys[index]
        )[[1L]]
        window__binding_unique(
            owners,
            paste0(owner_tables[index], ".", owner_keys[index])
        )
        selected <- which(type == boundary)
        if (any(!owner[selected] %in% owners)) {
            window__binding_error(
                "the host owner does not exist in its declared boundary table",
                surfaces$SURFACE_ID[missing[selected[
                    !owner[selected] %in% owners
                ]]]
            )
        }
    }

    audit <- data.table::data.table(
        window_id = windows$ID[window_rows],
        side = side,
        surface_id = surfaces$SURFACE_ID[missing],
        enclosure_id = windows$OF_ENCLOSURE[window_rows],
        host_surface_id = host_ids,
        original_of_room = surfaces$OF_ROOM[missing],
        original_type = surfaces$TYPE[missing],
        restored_of_room = owner,
        restored_type = type
    )
    data.table::setorderv(audit, c("window_id", "side"))

    # One transaction covers every row. A failed update or warning promoted to
    # an error rolls back the repair, including for an explicit copy=FALSE call.
    temporary <- basename(tempfile("destep_window_bindings_"))
    quoted <- as.character(DBI::dbQuoteIdentifier(dest, temporary))
    DBI::dbWriteTable(dest, temporary, as.data.frame(audit), temporary = TRUE)
    on.exit(DBI::dbRemoveTable(dest, temporary), add = TRUE)
    DBI::dbWithTransaction(dest, {
        affected <- DBI::dbExecute(
            dest,
            sprintf(
                "UPDATE SURFACE SET OF_ROOM =
                (SELECT restored_of_room FROM %s WHERE surface_id = SURFACE.SURFACE_ID),
                TYPE = (SELECT restored_type FROM %s WHERE surface_id = SURFACE.SURFACE_ID)
             WHERE SURFACE_ID IN (SELECT surface_id FROM %s) AND OF_ROOM = -1",
                quoted,
                quoted,
                quoted
            )
        )
        if (affected != nrow(audit)) {
            window__binding_error(
                "the repair did not update every validated face",
                audit$surface_id
            )
        }
        warn(
            sprintf(
                "Restored %d unbound DeST window-side surface bindings from their explicit matching host sides; see conversion$window_bindings.",
                nrow(audit)
            ),
            class = "destep_restored_window_bindings",
            bindings = data.table::copy(audit)
        )
    })
    audit
}

# Read only the relationship columns required for a deterministic restoration.
# Missing schema is a diagnostic, never permission to infer an owner geometrically.
window__binding_source <- function(dest, table, columns) {
    if (
        !DBI::dbExistsTable(dest, table) ||
            !all(columns %in% DBI::dbListFields(dest, table))
    ) {
        window__binding_error(paste0("missing required fields in ", table))
    }
    DBI::dbGetQuery(
        dest,
        paste0(
            "SELECT ",
            paste(DBI::dbQuoteIdentifier(dest, columns), collapse = ", "),
            " FROM ",
            DBI::dbQuoteIdentifier(dest, table)
        )
    )
}

# Duplicate or absent keys make even an apparently unique SQL join ambiguous.
window__binding_unique <- function(ids, field) {
    if (anyNA(ids) || anyDuplicated(ids)) {
        window__binding_error(paste0(
            "missing or duplicate identifiers in ",
            field
        ))
    }
}

# Identify unsafe source relationships without guessing a replacement binding.
window__binding_error <- function(reason, ids = numeric()) {
    abort(
        paste0(
            "Cannot restore DeST window-side bindings: ",
            reason,
            if (length(ids)) {
                paste0(" (SURFACE_ID: ", paste(ids, collapse = ", "), ")")
            },
            "."
        ),
        class = "destep_invalid_window_bindings",
        surface_ids = ids
    )
}

# Retain a compact provenance note when the model is saved outside the R session.
window__binding_comments <- function(bindings) {
    if (nrow(bindings) == 0L) {
        return(character())
    }
    sprintf(
        "destep restored window %s side %s SURFACE %s from host %s: OF_ROOM %s -> %s; TYPE %s -> %s",
        bindings$window_id,
        bindings$side,
        bindings$surface_id,
        bindings$host_surface_id,
        bindings$original_of_room,
        bindings$restored_of_room,
        bindings$original_type,
        bindings$restored_type
    )
}
