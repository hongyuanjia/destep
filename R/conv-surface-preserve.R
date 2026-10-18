# Canonicalize only vertices with the same explicit middle-plane/index identity.
# A spatial tolerance is not permission to merge unrelated source points. Both
# room-side copies already read the same PLANE polygon, so inconsistent copies
# indicate invalid input rather than a nearest-neighbour repair opportunity.
surface__canonical_source_vertices <- function(
    surface,
    profile = eplus_geom__profile()
) {
    value <- data.table::copy(surface)
    if (!all(c("PLANE", "POINT_NO") %in% names(value))) {
        return(value)
    }
    columns <- c("PLANE", "POINT_NO", "POINT_X", "POINT_Y", "POINT_Z")
    canonical <- unique(
        value[, columns, with = FALSE],
        by = c("PLANE", "POINT_NO")
    )
    key <- paste(value$PLANE, value$POINT_NO, sep = "/")
    reference <- match(
        key,
        paste(canonical$PLANE, canonical$POINT_NO, sep = "/")
    )
    coordinates <- c("POINT_X", "POINT_Y", "POINT_Z")
    difference <- abs(
        as.matrix(value[, coordinates, with = FALSE]) -
            as.matrix(canonical[reference, coordinates, with = FALSE])
    )
    inconsistent <- which(rowSums(difference > profile$zero_distance) > 0L)
    if (length(inconsistent)) {
        stop(
            sprintf(
                "DeST middle plane %s has inconsistent copies of source vertex %s.",
                value$PLANE[inconsistent[[1L]]],
                value$POINT_NO[inconsistent[[1L]]]
            ),
            call. = FALSE
        )
    }
    value
}

# Identify only exterior walls rising from a positive-length source roof edge.
# This is boundary incidence, not a shadow test: distant heights, point contacts
# and near-coincident coordinates do not establish a local clipping constraint.
# Return source planes so the reason for retaining a roof partition is auditable.
surface__roof_rising_walls <- function(roof, walls, profile) {
    empty <- walls$PLANE[FALSE]
    if (!nrow(roof) || !nrow(walls)) {
        return(empty)
    }
    tolerance <- profile$plane_distance
    level <- roof$POINT_Z[[1L]]
    roof_xy <- as.matrix(roof[, c("POINT_X", "POINT_Y"), with = FALSE])
    roof_end <- roof_xy[c(seq_len(nrow(roof_xy))[-1L], 1L), , drop = FALSE]
    direction <- roof_end - roof_xy
    edge_length <- sqrt(rowSums(direction^2))
    planes <- unique(walls$PLANE)
    # Each source polygon is small. Scan its actual boundary segments rather
    # than constructing a global arrangement or cutting every incident face.
    incident <- vapply(
        planes,
        function(plane) {
            face <- walls[walls$PLANE == plane, ]
            if (max(face$POINT_Z) <= level + tolerance) {
                return(FALSE)
            }
            xyz <- geom__coordinates(face)
            end <- xyz[c(seq_len(nrow(xyz))[-1L], 1L), , drop = FALSE]
            at_level <- which(
                abs(xyz[, 3L] - level) <= tolerance &
                    abs(end[, 3L] - level) <= tolerance
            )
            for (edge in at_level) {
                a <- xyz[edge, 1:2]
                b <- end[edge, 1:2]
                offset_a <- sweep(roof_xy, 2L, a, "-")
                offset_b <- sweep(roof_xy, 2L, b, "-")
                distance_a <- abs(
                    direction[, 1L] *
                        offset_a[, 2L] -
                        direction[, 2L] * offset_a[, 1L]
                ) /
                    edge_length
                distance_b <- abs(
                    direction[, 1L] *
                        offset_b[, 2L] -
                        direction[, 2L] * offset_b[, 1L]
                ) /
                    edge_length
                collinear <- which(
                    edge_length > tolerance &
                        distance_a <= tolerance &
                        distance_b <= tolerance
                )
                if (!length(collinear)) {
                    next
                }
                # Project onto the roof edge in metres. Only positive overlap
                # counts; sharing an endpoint cannot force a whole roof to split.
                along_a <- -rowSums(offset_a * direction) / edge_length
                along_b <- -rowSums(offset_b * direction) / edge_length
                overlap <- pmin(edge_length, pmax(along_a, along_b)) -
                    pmax(0, pmin(along_a, along_b))
                if (any(overlap[collinear] > tolerance)) {
                    return(TRUE)
                }
            }
            FALSE
        },
        logical(1L)
    )
    planes[incident]
}

# Select explicit convex clipping consumers without predicting shadow paths.
# Opening hosts and exposed casting faces retain their clipping constraints.
# Horizontal roofs use local rising-wall incidence, never a building maximum
# height or the mere existence of active shading elsewhere in the model.
surface__convex_planes <- function(surface, window, profile) {
    forced <- unique(window$PLANE)
    if (!all(c("TYPE", "BOUNDARY") %in% names(surface))) {
        return(forced)
    }
    real <- if ("TYPE_SURFACE" %in% names(surface)) {
        surface[!surface$TYPE_SURFACE %in% c(1L, 2L)]
    } else {
        surface
    }
    exterior <- real[real$BOUNDARY == "Outdoors", ]
    if (!nrow(exterior)) {
        return(forced)
    }
    walls <- exterior[exterior$TYPE == "Wall", ]
    candidates <- unique(exterior$PLANE)
    required <- vapply(
        candidates,
        function(plane) {
            face <- exterior[exterior$PLANE == plane, ]
            upward <- all(face$TYPE == "Roof") &&
                diff(range(face$POINT_Z)) <= profile$planarity_distance
            if (!upward) {
                return(TRUE)
            }
            length(surface__roof_rising_walls(face, walls, profile)) > 0L
        },
        logical(1L)
    )
    unique(c(forced, candidates[required]))
}

# Reject nonplanar or self-intersecting outlines before a normal or a fan can
# turn invalid source input into an apparently valid set of unrelated facets.
surface__validate_outline <- function(value, profile, plane) {
    frame <- geom__polygon_frame(value, profile$normal_magnitude)
    fail <- function(reason) {
        stop(
            sprintf("DeST middle plane %s has %s.", plane, reason),
            call. = FALSE
        )
    }
    if (
        !frame$valid ||
            anyNA(frame$coordinates) ||
            any(!is.finite(frame$coordinates))
    ) {
        fail("invalid polygon coordinates")
    }
    if (frame$planarity_error > profile$planarity_distance) {
        fail("a non-planar polygon")
    }
    xy <- frame$xy
    n <- nrow(xy)
    # Nonadjacent boundary segments must not cross; adjacent shared endpoints
    # are legitimate. This fixed per-face check does not compare building faces.
    if (n >= 4L) {
        for (i in seq_len(n - 2L)) {
            j <- seq.int(i + 2L, n)
            if (i == 1L) {
                j <- j[j != n]
            }
            for (k in j) {
                a <- xy[i, ]
                b <- xy[i %% n + 1L, ]
                c <- xy[k, ]
                d <- xy[k %% n + 1L, ]
                cross <- c(
                    geom__cross_2d(a, b, c),
                    geom__cross_2d(a, b, d),
                    geom__cross_2d(c, d, a),
                    geom__cross_2d(c, d, b)
                )
                if (
                    cross[[1L]] * cross[[2L]] < -profile$intersection &&
                        cross[[3L]] * cross[[4L]] < -profile$intersection
                ) {
                    fail("a self-intersecting polygon")
                }
            }
        }
    }
    invisible(NULL)
}

# Retain source identities and exact output ownership for reviewing each local
# split. This is an audit of exported objects, not another input or shadow model.
surface__geometry_audit <- function(surface) {
    surface[,
        .(
            source_surface = .SD[["ID"]][[1L]],
            source_plane = .SD[["PLANE"]][[1L]],
            target_name = .SD[["NAME"]][[1L]],
            room = .SD[["ROOM"]][[1L]],
            type = .SD[["TYPE"]][[1L]],
            construction = .SD[["CONSTRUCTION"]][[1L]],
            boundary = .SD[["BOUNDARY"]][[1L]],
            peer = .SD[["BOUNDARY_OBJECT"]][[1L]],
            area = geom__polygon_area(.SD),
            vertices = nrow(.SD)
        ),
        by = "OUTPUT_ID"
    ]
}
