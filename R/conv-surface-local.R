# Enumerate valid single diagonals, leaving either resulting polygon concave
# when its consumer permits that. A local closure defect is not a requirement
# to convex-decompose every other corner of the same source face.
surface__local_cuts <- function(face, profile) {
    n <- nrow(face)
    if (n < 4L) {
        return(list())
    }
    frame <- geom__polygon_frame(face)
    xy <- frame$xy
    result <- list()
    for (i in seq_len(n - 2L)) {
        for (j in seq.int(i + 2L, n)) {
            if (i == 1L && j == n) {
                next
            }
            first <- face[seq.int(i, j)]
            second <- face[c(seq.int(j, n), seq_len(i))]
            if (
                geom__polygon_area(first) <= profile$area ||
                    geom__polygon_area(second) <= profile$area
            ) {
                next
            }
            # Absolute areas alone cannot certify a valid diagonal. Its middle
            # must be inside and it must not cross another boundary segment.
            middle <- (xy[i, ] + xy[j, ]) / 2
            inside <- FALSE
            crosses <- FALSE
            for (k in seq_len(n)) {
                following <- k %% n + 1L
                a <- xy[k, ]
                b <- xy[following, ]
                if (
                    (a[[2L]] > middle[[2L]]) != (b[[2L]] > middle[[2L]]) &&
                        middle[[1L]] <
                            a[[1L]] +
                                (middle[[2L]] - a[[2L]]) *
                                    (b[[1L]] - a[[1L]]) /
                                    (b[[2L]] - a[[2L]])
                ) {
                    inside <- !inside
                }
                if (!k %in% c(i, j) && !following %in% c(i, j)) {
                    cross <- c(
                        geom__cross_2d(xy[i, ], xy[j, ], a),
                        geom__cross_2d(xy[i, ], xy[j, ], b),
                        geom__cross_2d(a, b, xy[i, ]),
                        geom__cross_2d(a, b, xy[j, ])
                    )
                    if (
                        cross[[1L]] * cross[[2L]] < -profile$intersection &&
                            cross[[3L]] * cross[[4L]] < -profile$intersection
                    ) {
                        crosses <- TRUE
                    }
                }
            }
            if (
                !inside ||
                    crosses ||
                    abs(
                        geom__polygon_area(first) +
                            geom__polygon_area(second) -
                            frame$area
                    ) >
                        profile$area
            ) {
                next
            }
            result[[length(result) + 1L]] <- list(first, second)
        }
    }
    result
}

# Apply the same local cut to an explicit reciprocal pair, keeping all source
# identity, room, construction and boundary fields. New names affect only this
# pair; no neighbouring or coplanar source faces are merged or reassigned.
surface__apply_local_cut <- function(surface, id, parts) {
    base <- surface[surface$OUTPUT_ID == id, ]
    name <- base$NAME[[1L]]
    peer_name <- base$BOUNDARY_OBJECT[[1L]]
    paired <- base$BOUNDARY[[1L]] == "Surface" &&
        !is.na(peer_name) &&
        peer_name != name
    ids <- id
    if (paired) {
        ids <- c(ids, unique(surface$OUTPUT_ID[surface$NAME == peer_name]))
    }
    result <- list(surface[!surface$OUTPUT_ID %in% ids, ])
    coordinate_columns <- geom__coordinate_columns()
    for (member_id in ids) {
        member <- surface[surface$OUTPUT_ID == member_id, ]
        metadata <- member[
            1L,
            setdiff(names(member), coordinate_columns),
            with = FALSE
        ]
        for (k in seq_along(parts)) {
            geometry <- data.table::copy(parts[[k]][,
                coordinate_columns,
                with = FALSE
            ])
            data.table::set(
                geometry,
                j = "POINT_NO",
                value = seq_len(nrow(geometry)) - 1L
            )
            value <- data.table::copy(metadata)
            data.table::set(
                value,
                j = "OUTPUT_ID",
                value = paste0(member_id, "-L", k)
            )
            data.table::set(
                value,
                j = "NAME",
                value = paste0(member$NAME[[1L]], " [Local ", k, "]")
            )
            if (member$BOUNDARY[[1L]] == "Surface") {
                boundary <- if (paired) {
                    member$BOUNDARY_OBJECT[[1L]]
                } else {
                    member$NAME[[1L]]
                }
                data.table::set(
                    value,
                    j = "BOUNDARY_OBJECT",
                    value = paste0(boundary, " [Local ", k, "]")
                )
            }
            result[[length(result) + 1L]] <- cbind(
                value[rep(1L, nrow(geometry))],
                geometry
            )
        }
    }
    value <- data.table::rbindlist(result, fill = TRUE)
    data.table::setorderv(value, c("ID", "OUTPUT_ID", "POINT_NO"))
    # PART drives opening-to-host references; all pieces of one source face
    # receive fresh deterministic numbers after a cut.
    value[, PART := data.table::rleid(OUTPUT_ID), by = "ID"]
    value[, PART_COUNT := data.table::uniqueN(PART), by = "ID"]
    # Flatten labels after successive local cuts. A recursive cut history must
    # not create unbounded EnergyPlus names; reciprocal references follow the
    # explicit old-to-new object mapping rather than string substitution.
    affected <- unique(surface$ID[surface$OUTPUT_ID %in% ids])
    renamed <- unique(
        value[
            value$ID %in% affected,
            c("OUTPUT_ID", "NAME", "ID", "PART"),
            with = FALSE
        ],
        by = "OUTPUT_ID"
    )
    base_names <- unique(
        surface[
            surface$ID %in% affected,
            c("ID", "ORIGINAL_NAME"),
            with = FALSE
        ],
        by = "ID"
    )
    names_new <- paste0(
        base_names$ORIGINAL_NAME[match(renamed$ID, base_names$ID)],
        " [Local ",
        renamed$PART,
        "]"
    )
    object_index <- match(value$NAME, renamed$NAME)
    rows <- which(!is.na(object_index))
    data.table::set(
        value,
        i = rows,
        j = "NAME",
        value = names_new[object_index[rows]]
    )
    peer_index <- match(value$BOUNDARY_OBJECT, renamed$NAME)
    rows <- which(!is.na(peer_index))
    data.table::set(
        value,
        i = rows,
        j = "BOUNDARY_OBJECT",
        value = names_new[peer_index[rows]]
    )
    value
}

# Preserve cleaned source shells and report wider engine-tolerance ambiguities.
# Only a real source-tolerance failure is allowed to trigger local subdivision.
surface__repair_local_edges <- function(surface, window, profile) {
    # Engine volume diagnostics use a much wider tolerance than source polygon
    # arithmetic. Nearby distinct edges can be counted more than twice in that
    # fallback even when the cleaned source shell is closed. Splitting whole
    # rooms to suppress this diagnostic would change valid source discretization.
    source_profile <- profile
    source_profile$closure_vertex_distance <- profile$plane_distance
    value <- surface__repair_source_edges(surface, window, source_profile)
    engine_open <- surface__energyplus_unclosed_rooms(value, profile)
    if (length(engine_open)) {
        warning(structure(
            list(
                message = sprintf(
                    paste0(
                        "Room shell(s) %s close at the source geometry tolerance ",
                        "but fail the EnergyPlus 1 cm closure diagnostic. ",
                        "Source outlines are preserved; inspect native geometry ",
                        "diagnostics for near-coincident edges."
                    ),
                    paste(engine_open, collapse = ", ")
                ),
                call = NULL,
                rooms = engine_open,
                source_tolerance_m = profile$plane_distance,
                engine_tolerance_m = profile$closure_vertex_distance
            ),
            class = c(
                "destep_geometry_closure_tolerance",
                "warning",
                "condition"
            )
        ))
    }
    value
}

# Repair only actual cleanup-induced source-shell defects. Running every trial
# at source tolerance prevents an unrelated engine-tolerance warning from
# pulling a closed room into the repair when another room genuinely fails.
surface__repair_source_edges <- function(surface, window, profile) {
    value <- data.table::copy(surface)
    # Cache only identical XYZ polygons within this call; trial cuts cannot
    # reuse a different polygon, and the cache never escapes to the caller.
    cleanup_cache <- new.env(hash = TRUE, parent = emptyenv())
    settled <- character()
    history <- list()
    for (iteration in seq_len(64L)) {
        diagnostic <- surface__energyplus_unclosed_rooms(
            value,
            profile,
            details = TRUE,
            cleanup_cache = cleanup_cache
        )
        if (!length(diagnostic$rooms)) {
            return(surface__restore_local_cuts(
                value,
                surface,
                history,
                profile,
                cleanup_cache
            ))
        }
        objects <- unique(
            value[
                value$OUTPUT_ID %in% diagnostic$output_ids,
                c("OUTPUT_ID", "NAME", "TYPE", "PLANE"),
                with = FALSE
            ],
            by = "OUTPUT_ID"
        )
        objects <- objects[
            order(
                # Repair final-pass failing edges before considering harmless
                # first-pass T-junctions elsewhere in the same room.
                !objects$OUTPUT_ID %in% diagnostic$bad_output_ids,
                match(objects$TYPE, c("Floor", "Ceiling", "Wall", "Roof")),
                objects$PLANE %in% window$PLANE
            ),
        ]
        accepted <- FALSE
        for (id in objects$OUTPUT_ID) {
            face <- value[value$OUTPUT_ID == id, ]
            # Unchanged rooms are already closed. Recheck only the failed
            # rooms and the actual reciprocal neighbour affected by this cut.
            active_rooms <- unique(c(
                diagnostic$rooms,
                value$ROOM[value$NAME == face$BOUNDARY_OBJECT[[1L]]]
            ))
            # Reintroduce exact boundary junctions only on the candidate face.
            # These are source points, not projected or nearby replacement XYZs.
            room_points <- unique(value[
                value$ROOM %in% active_rooms,
                c("POINT_X", "POINT_Y", "POINT_Z"),
                with = FALSE
            ])
            split <- surface__split_edges(face, room_points, profile)
            choices <- surface__local_cuts(split, profile)
            avoid <- window[window$PLANE == face$PLANE[[1L]], ]
            has_junction <- nrow(split) > nrow(face) ||
                any(surface__redundant_vertices(split, profile))
            if (has_junction && geom__polygon_is_convex(split, profile$angle)) {
                # A near-collinear junction can be missed by every diagonal
                # between existing vertices. A planar interior fan is another
                # exact representation; it is accepted only if closure improves.
                center <- colMeans(geom__coordinates(split))
                fan <- lapply(seq_len(nrow(split)), function(k) {
                    triangle <- data.table::copy(split[c(
                        k,
                        k,
                        k %% nrow(split) + 1L
                    )])
                    for (axis in seq_along(center)) {
                        data.table::set(
                            triangle,
                            i = 1L,
                            j = c("POINT_X", "POINT_Y", "POINT_Z")[[axis]],
                            value = center[[axis]]
                        )
                    }
                    triangle
                })
                representable <- vapply(
                    fan,
                    function(triangle) {
                        xyz <- geom__coordinates(triangle)
                        geom__polygon_area(triangle) > profile$area &&
                            all(
                                sqrt(rowSums((xyz - xyz[c(2L, 3L, 1L), ])^2)) >=
                                    profile$coordinate_distance
                            )
                    },
                    logical(1L)
                )
                if (all(representable)) choices[[length(choices) + 1L]] <- fan
            }
            if (nrow(avoid) && has_junction) {
                fallback <- surface__triangulate_polygon(split, avoid, profile)
                choices[[length(choices) + 1L]] <- lapply(
                    unique(fallback$PART),
                    function(part) fallback[fallback$PART == part, ]
                )
            }
            for (parts in choices) {
                # A necessary wall repair must also preserve its openings.
                # Check the complete source-side partition: other host pieces
                # still carry their original share of a crossing opening.
                untouched <- value[
                    value$ID == face$ID[[1L]] & value$OUTPUT_ID != id,
                ]
                host_parts <- c(parts, split(untouched, untouched$OUTPUT_ID))
                if (
                    nrow(avoid) &&
                        !surface__cut_preserves_openings(
                            host_parts,
                            avoid,
                            profile
                        )
                ) {
                    next
                }
                candidate <- surface__apply_local_cut(value, id, parts)
                check <- surface__energyplus_unclosed_rooms(
                    candidate[candidate$ROOM %in% active_rooms, ],
                    profile,
                    details = TRUE,
                    cleanup_cache = cleanup_cache
                )
                resolved <- setdiff(diagnostic$rooms, check$rooms)
                # Refining a shared slab may expose its neighbour's latent
                # cleanup defect. Permit that dependency only when it closes
                # an original failing room and never reopens a settled room.
                transfers_to_peer <- length(resolved) > 0L &&
                    !any(check$rooms %in% settled) &&
                    all(check$rooms %in% active_rooms)
                improves <- check$defects < diagnostic$defects ||
                    (check$defects == diagnostic$defects &&
                        check$first_defects < diagnostic$first_defects)
                if (
                    !any(check$rooms %in% settled) &&
                        (improves || transfers_to_peer)
                ) {
                    history[[length(history) + 1L]] <- data.table::copy(
                        value[value$PLANE == face$PLANE[[1L]], ]
                    )
                    value <- candidate
                    settled <- unique(c(settled, resolved))
                    accepted <- TRUE
                    break
                }
            }
            if (accepted) break
        }
        if (!accepted) break
    }
    remaining <- surface__energyplus_unclosed_rooms(value, profile)
    if (length(remaining)) {
        stop(
            sprintf(
                "Could not preserve a closed EnergyPlus shell for DeST room(s): %s.",
                paste(remaining, collapse = ", ")
            ),
            call. = FALSE
        )
    }
    value
}

# Check each source opening against proposed host pieces before accepting a
# local cut. The real opening converter subsequently enforces the same area
# invariant; this avoids selecting a repair known to discard part of a window.
surface__cut_preserves_openings <- function(parts, window, profile) {
    all(vapply(
        unique(window$OPENING_ID),
        function(id) {
            original <- window[window$OPENING_ID == id, ]
            areas <- vapply(
                parts,
                function(part) {
                    clipped <- tryCatch(
                        subsurface__clip_polygon(original, part, profile),
                        error = function(e) NULL
                    )
                    if (is.null(clipped)) 0 else geom__polygon_area(clipped)
                },
                numeric(1L)
            )
            area <- geom__polygon_area(original)
            abs(sum(areas) - area) <= max(profile$area, area * 1e-8)
        },
        logical(1L)
    ))
}

# Once the complete shared-room graph closes, remove provisional cuts again.
# Whole source planes have priority; then rewind each accepted local operation.
# A cut is retained only when its removal would reintroduce a closure defect.
surface__restore_local_cuts <- function(
    value,
    original,
    history,
    profile,
    cleanup_cache
) {
    planes <- unique(original$PLANE)
    priority <- vapply(
        planes,
        function(plane) {
            role <- original$TYPE[match(plane, original$PLANE)]
            match(role, c("Roof", "Floor", "Ceiling", "Wall"))
        },
        integer(1L)
    )
    replacements <- c(
        lapply(planes[order(priority)], function(plane) {
            original[original$PLANE == plane, ]
        }),
        rev(history)
    )
    for (replacement in replacements) {
        plane <- replacement$PLANE[[1L]]
        current <- value[value$PLANE == plane, ]
        if (
            length(unique(replacement$OUTPUT_ID)) >=
                length(unique(current$OUTPUT_ID))
        ) {
            next
        }
        candidate <- data.table::rbindlist(
            list(value[value$PLANE != plane, ], replacement),
            fill = TRUE
        )
        rooms <- unique(current$ROOM)
        if (
            !length(surface__energyplus_unclosed_rooms(
                candidate[candidate$ROOM %in% rooms, ],
                profile,
                cleanup_cache = cleanup_cache
            ))
        ) {
            value <- candidate
        }
    }
    data.table::setorderv(value, c("ID", "OUTPUT_ID", "POINT_NO"))
    value
}
