# SURFACE|MAIN_ENCLOSURE|PLANE -> BuildingSurface:Detailed
destep_conv_surface <- function(dest, ep) {
    # NOTE: In DeST, the main enclosure table is used to store the relationship
    # between surfaces and the rooms they belong to. Different from EnergyPlus,
    # 'adjacent' surfaces in DeST have different locations. The distance between
    # the outer surface and the inner surface is the thickness of the
    # construction. Here, the 'MIDDLE_PLANE' column in 'MAIN_ENCLOSURE' table
    # is used to get the vertices of adjacent surfaces
    surface <- DBI::dbGetQuery(
        dest,
        "
        SELECT
            E.ENCLOSURE_ID                                AS ENCLOSURE_ID,
            E.SURFACE_ID                                   AS ID,
            E.PLANE                                        AS PLANE,
            S.NAME                                         AS NAME,
            E.KIND                                         AS KIND_ENCLOSURE,
            S.TYPE                                         AS TYPE_SURFACE,
            -- KIND describes the construction role, but KIND 5 is a shared
            -- floor/ceiling construction whose room-side role depends on its
            -- DeST direction sentinel. KIND 6 is an exposed floor and must not
            -- be treated as a roof, otherwise its normal is forced upward.
            CASE
                WHEN E.KIND = 1 OR E.KIND = 2 THEN 'Wall'
                WHEN E.KIND = 3               THEN 'Roof'
                WHEN E.KIND = 4               THEN 'Floor'
                WHEN E.KIND = 5 AND S.AZIMUTH = 999 THEN 'Floor'
                WHEN E.KIND = 5               THEN 'Ceiling'
                WHEN E.KIND = 6               THEN 'Floor'
            END                                            AS TYPE,
            E.SIDE                                         AS SIDE,
            E.CONSTRUCTION                                 AS CONSTRUCTION,
            COALESCE(ROOM.NAME, OUTSIDE.NAME, GROUND.NAME) AS ROOM,
            -- DeST represents outdoors and ground as peer pseudo-surfaces
            -- (TYPE 1 and 2). Only a peer that is another room surface maps to
            -- EnergyPlus's Surface boundary condition and needs a peer name.
            CASE
                WHEN PEER.TYPE = 1 THEN 'Outdoors'
                WHEN PEER.TYPE = 2 THEN 'Ground'
                ELSE 'Surface'
            END                                            AS BOUNDARY,
            CASE
                WHEN PEER.TYPE NOT IN (1, 2) THEN PEER.NAME
            END                                            AS BOUNDARY_OBJECT,
            -- Keep the source direction metadata so vertex winding is derived
            -- from DeST geometry instead of assuming SIDE 1 or SIDE 2 is out.
            S.AZIMUTH                                      AS AZIMUTH,
            S.TILT                                         AS TILT,
            L.POINT_NO                                     AS POINT_NO,
            ROUND(P.X, 3)                                  AS POINT_X,
            ROUND(P.Y, 3)                                  AS POINT_Y,
            ROUND(P.Z, 3)                                  AS POINT_Z
        FROM (
            SELECT
                ENCLOSURE_ID, SURFACE_ID, PEER_ID, S.KIND, SIDE,
                C.CNAME AS CONSTRUCTION, PLANE
            FROM (
                -- get both side surfaces
                SELECT ID AS ENCLOSURE_ID, SIDE1 AS SURFACE_ID,
                    SIDE2 AS PEER_ID, KIND, 1 AS SIDE, CONSTRUCTION,
                    MIDDLE_PLANE AS PLANE
                FROM MAIN_ENCLOSURE
                UNION ALL
                SELECT ID AS ENCLOSURE_ID, SIDE2 AS SURFACE_ID,
                    SIDE1 AS PEER_ID, KIND, 2 AS SIDE, CONSTRUCTION,
                    MIDDLE_PLANE AS PLANE
                FROM MAIN_ENCLOSURE
            ) S
            -- get construction name
            LEFT JOIN (
                SELECT STRUCT_ID, CNAME, 1 AS KIND
                FROM SYS_OUTWALL
                UNION
                SELECT STRUCT_ID, CNAME, 2 AS KIND
                FROM SYS_INWALL
                UNION
                SELECT STRUCT_ID, CNAME, 3 AS KIND
                FROM SYS_ROOF
                UNION
                SELECT STRUCT_ID, CNAME, 4 AS KIND
                FROM SYS_GROUNDFLOOR
                UNION
                SELECT STRUCT_ID, CNAME, 5 AS KIND
                FROM SYS_MIDDLEFLOOR
                UNION
                SELECT STRUCT_ID, CNAME, 6 AS KIND
                FROM SYS_AIRFLOOR
            ) C
            ON S.CONSTRUCTION = C.STRUCT_ID AND S.KIND = C.KIND
        ) E
        LEFT JOIN SURFACE S
        ON E.SURFACE_ID = S.SURFACE_ID
        LEFT JOIN SURFACE PEER
        ON E.PEER_ID = PEER.SURFACE_ID
        -- get room name
        LEFT JOIN ROOM
        ON S.OF_ROOM = ROOM.ID
        LEFT JOIN OUTSIDE
        ON S.TYPE = 1 AND S.OF_ROOM = OUTSIDE.OUTSIDE_ID
        LEFT JOIN GROUND
        ON S.TYPE = 2 AND S.OF_ROOM = GROUND.GROUND_ID
        LEFT JOIN PLANE P
        ON E.PLANE = P.PLANE_ID
        LEFT JOIN GEOMETRY G
        ON P.GEOMETRY = G.GEOMETRY_ID
        LEFT JOIN LOOP_POINT L
        ON G.BOUNDARY_LOOP_ID = L.LOOP_ID
        LEFT JOIN POINT P
        ON L.POINT = P.POINT_ID
        "
    )
    assert_unique_name(surface$NAME[surface$POINT_NO == 0L], "surface")
    data.table::setDT(surface)
    data.table::setorderv(surface, c("ID", "POINT_NO"))

    # Normalize each shared middle-plane polygon once before the two room-side
    # copies are oriented. Vertices used by another plane are topological
    # junctions and must survive collinear-point cleanup.
    window_parent <- integer()
    if (destep_has_rows(dest, "WINDOW")) {
        window_parent <- DBI::dbGetQuery(dest, "
            SELECT CASE
                WHEN S1.TYPE NOT IN (1, 2) THEN E.SIDE1
                ELSE E.SIDE2
            END AS ID
            FROM WINDOW W
            INNER JOIN MAIN_ENCLOSURE E ON W.OF_ENCLOSURE = E.ID
            LEFT JOIN SURFACE S1 ON E.SIDE1 = S1.SURFACE_ID
        ")$ID
    }
    surface <- destep_normalize_surface_topology(surface, window_parent)
    # remove the surface indicating outside environment and grounds
    surface <- surface[!J(c(1L, 2L)), on = "TYPE_SURFACE"]

    # Orient every polygon from DeST's source azimuth and tilt. This also makes
    # exposed floors face downward without relying on enclosure side numbers.
    south_direction <- destep_south_direction(dest)
    surface <- surface[
        , destep_orient_surface_polygon(.SD, south_direction),
        by = "OUTPUT_ID"
    ]

    # TODO: how does DeST handle the case when the surface is both a floor and a ceiling?
    # TODO: how does EnergyPlus handle "empty floor slab"?

    value <- surface[,
        by = "OUTPUT_ID",
        list(value = list(c(
            list(
                # 01: Name
                name = NAME[[1L]],
                # 02: Surface Type
                surface_type = TYPE[[1L]],
                # 03: Construction Name
                construction_name = CONSTRUCTION[[1L]],
                # 04: Zone Name
                zone_name = ROOM[[1L]],
                # 05: Space Name - Space was introduced in EnergyPlus v9.6
                space_name = NULL,
                # 06: Outside Boundary Condition
                outside_boundary_condition = BOUNDARY[[1L]],
                # 07: Outside Boundary Condition Object
                outside_boundary_condition_object = if (!is.na(BOUNDARY_OBJECT[[1L]])) BOUNDARY_OBJECT[[1L]],
                # 08: Sun Exposure
                sun_exposure = if (BOUNDARY[[1L]] == "Outdoors") "SunExposed" else "NoSun",
                # 09: Wind Exposure
                wind_exposure = if (BOUNDARY[[1L]] == "Outdoors") "WindExposed" else "NoWind",
                # 10: View Factor to Ground
                view_factor_to_ground = "Autocalculate",
                # 11: Number of Vertices
                number_of_vertices = max(POINT_NO) + 1L
            ),
            # Vertices
            as.list(as.double(vapply(POINT_NO + 1L,
                FUN.VALUE = double(3),
                function(ind) {
                    c(POINT_X[ind], POINT_Y[ind], POINT_Z[ind])
                }
            )))
        )))
    ]$value

    # remove space name field
    if (ep$version() <= "9.5") {
        ind <- which(names(value[[1L]]) == "space_name")
        if (length(ind) > 0L) {
            value <- lapply(value, .subset, -ind)
        }
    }

    out <- eval(as.call(c(
        destep_add, dest, ep,
        lapply(value, function(val) bquote("BuildingSurface:Detailed" := .(val)))
    )))

    # always attach the table to the output in case it is useful later
    attr(out, "table") <- surface

    out
}

# Return the drawing-space south direction used to interpret DeST surface
# azimuths. Older ad hoc databases without ENVIRONMENT retain DeST's standard
# drawing convention, where drawing +Y points north.
destep_south_direction <- function(dest) {
    if (!"ENVIRONMENT" %in% DBI::dbListTables(dest) ||
        !destep_table_has_fields(dest, "ENVIRONMENT", "SOUTH_DIRECTION")) {
        # DeST's standard 270-degree setting places drawing north on +Y. Use it
        # only for legacy or synthetic databases that lack ENVIRONMENT metadata.
        return(270.0)
    }

    direction <- DBI::dbGetQuery(
        dest,
        "SELECT DISTINCT SOUTH_DIRECTION FROM ENVIRONMENT WHERE SOUTH_DIRECTION IS NOT NULL"
    )$SOUTH_DIRECTION
    if (length(direction) == 0L) return(270.0)
    if (length(direction) > 1L) {
        # One EnergyPlus Building object has only one North Axis, so silently
        # choosing one of several DeST orientations would rotate some geometry.
        stop("Multiple DeST south directions cannot be represented in one EnergyPlus model.")
    }

    as.double(direction[[1L]]) %% 360.0
}

# Translate the DeST drawing south-vector angle to EnergyPlus's clockwise
# rotation from true north to the model +Y axis.
destep_north_axis <- function(dest) {
    # Drawing +Y is 90 degrees counterclockwise from +X. Offsetting DeST's
    # south-vector angle by 90 degrees gives EnergyPlus's clockwise rotation
    # from true north to the model +Y axis.
    (destep_south_direction(dest) + 90.0) %% 360.0
}

# Convert DeST's azimuth/tilt convention into one drawing-coordinate unit
# normal. The sentinel azimuths distinguish downward and upward horizontal
# faces, while regular azimuths rotate clockwise from drawing north.
destep_expected_surface_normal <- function(azimuth, tilt, south_direction) {
    if (length(azimuth) != 1L || length(tilt) != 1L ||
        is.na(azimuth) || is.na(tilt)) {
        stop("A DeST surface must have one azimuth and tilt to determine its orientation.")
    }
    # The +/-999 values are DeST sentinels rather than compass azimuths: +999
    # denotes a downward horizontal face and -999 an upward horizontal face.
    if (azimuth == 999.0) return(c(0.0, 0.0, -1.0))
    if (azimuth == -999.0) return(c(0.0, 0.0, 1.0))

    # DeST azimuth increases clockwise from drawing north. Drawing north is
    # opposite SOUTH_DIRECTION, so clockwise azimuth is subtracted here.
    direction <- (south_direction - 180.0 - azimuth) * pi / 180.0
    inclination <- tilt * pi / 180.0
    # TILT is measured from the horizontal: sin(TILT) is the horizontal normal
    # magnitude and cos(TILT) is its vertical component.
    c(
        cos(direction) * sin(inclination),
        sin(direction) * sin(inclination),
        cos(inclination)
    )
}

# Calculate a polygon normal with Newell's method so concave planar DeST faces
# can be oriented without assuming a particular starting vertex.
destep_surface_normal <- function(surface) {
    # Wrap the final vertex back to the first so each Newell term represents
    # one ordered polygon edge without requiring a particular starting corner.
    following <- seq_len(nrow(surface)) %% nrow(surface) + 1L
    # Newell's accumulated vector remains stable for the concave, planar floor
    # polygons found in DeST, where a three-point cross product is insufficient.
    normal <- c(
        sum((surface$POINT_Y - surface$POINT_Y[following]) *
            (surface$POINT_Z + surface$POINT_Z[following])),
        sum((surface$POINT_Z - surface$POINT_Z[following]) *
            (surface$POINT_X + surface$POINT_X[following])),
        sum((surface$POINT_X - surface$POINT_X[following]) *
            (surface$POINT_Y + surface$POINT_Y[following]))
    )
    magnitude <- sqrt(sum(normal ^ 2))
    if (!is.finite(magnitude) || magnitude <= 1e-12) {
        # A zero normal means EnergyPlus cannot determine an outside face, so
        # fail before emitting a geometrically ambiguous surface.
        name <- if ("NAME" %in% names(surface)) surface$NAME[[1L]] else "<unknown>"
        stop(sprintf("DeST surface '%s' has a degenerate polygon.", name))
    }

    normal / magnitude
}

# Reverse a canonical polygon only when its geometric normal opposes the DeST
# outward direction. A non-positive alignment is rejected rather than silently
# emitting an ambiguous EnergyPlus face.
destep_orient_surface_polygon <- function(surface, south_direction, tolerance = 1e-8) {
    surface <- data.table::copy(surface)
    expected <- destep_expected_surface_normal(
        surface$AZIMUTH[[1L]], surface$TILT[[1L]], south_direction
    )
    # The dot product tests only direction, independent of polygon area. A
    # near-zero value means the coordinates contradict the DeST direction data.
    alignment <- sum(destep_surface_normal(surface) * expected)
    if (abs(alignment) <= tolerance) {
        stop(sprintf(
            "DeST surface '%s' polygon is inconsistent with its azimuth and tilt.",
            surface$NAME[[1L]]
        ))
    }
    # Reversing the complete sequence changes only winding; coordinates, area,
    # and the canonical set of shared-edge vertices remain unchanged.
    if (alignment < 0.0) surface <- surface[nrow(surface):1L]
    data.table::set(surface, NULL, "POINT_NO", seq_len(nrow(surface)) - 1L)
    surface
}

# Build one canonical vertex sequence per MAIN_ENCLOSURE middle plane. A
# collinear coordinate shared with another plane is a zone-edge junction, so it
# is protected while redundant vertices private to the plane are removed.
destep_normalize_surface_topology <- function(surface, window_surface = integer()) {
    coordinate_columns <- c("POINT_NO", "POINT_X", "POINT_Y", "POINT_Z")
    # Separate per-surface metadata from middle-plane coordinates so a shared
    # polygon is normalized once and then copied consistently to both rooms.
    metadata <- unique(
        surface[, setdiff(names(surface), coordinate_columns), with = FALSE],
        by = "ID"
    )
    # Window parents need a stable unsuffixed part name because every
    # FenestrationSurface:Detailed object references exactly one base surface.
    window_plane <- unique(metadata[ID %in% window_surface, PLANE])
    point <- unique(
        surface[, c("PLANE", coordinate_columns), with = FALSE],
        by = c("PLANE", "POINT_NO")
    )
    # Split edges only with coordinates from zones incident to the same middle
    # plane. This closes local T-junctions without importing unrelated points
    # from collinear walls elsewhere in the building.
    room_plane <- unique(
        metadata[!TYPE_SURFACE %in% c(1L, 2L), .(ROOM, PLANE)]
    )
    room_point <- merge(room_plane, point, by = "PLANE", allow.cartesian = TRUE)
    plane_room <- room_plane[, .(ROOMS = list(ROOM)), by = "PLANE"]
    point <- point[, {
        rooms <- plane_room[PLANE == .BY$PLANE, ROOMS][[1L]]
        candidates <- unique(
            room_point[ROOM %in% rooms, .(POINT_X, POINT_Y, POINT_Z)]
        )
        destep_split_surface_edges(.SD, candidates)
    }, by = "PLANE"]
    point[, COORDINATE_KEY := sprintf(
        "%.3f|%.3f|%.3f", POINT_X, POINT_Y, POINT_Z
    )]
    # A coordinate appearing on more than one plane is a zone-topology
    # junction. Removing it as merely collinear would reopen a shared edge.
    point[, PROTECTED := data.table::uniqueN(PLANE) > 1L, by = "COORDINATE_KEY"]
    # Private collinear points can be removed safely before surface copies are
    # oriented, which also prevents peer vertex-count differences in EnergyPlus.
    point <- point[, destep_simplify_surface_polygon(.SD), by = "PLANE"]
    point <- point[, {
        # EnergyPlus does not rewrite the IDF, but its GetSurfaceData path copies
        # input vertices into an in-memory SurfaceTmp and CheckConvexity removes
        # collinear vertices from that working copy. With reversed peer winding,
        # EnergyPlus 23.1 removed different counts from some complex peer faces,
        # causing a vertex-size-mismatch fatal error. Encode each protected
        # junction as a true part boundary before export: minimally partition
        # window hosts, and triangulate other polygons with identical part IDs
        # on both sides of an interzone construction.
        split <- any(destep_redundant_surface_vertices(.SD) & PROTECTED)
        if (split && .BY$PLANE %in% window_plane) {
            destep_partition_window_surface_polygon(.SD)
        } else if (split) {
            destep_triangulate_surface_polygon(.SD)
        } else {
            copy <- data.table::copy(.SD)
            copy[, `:=`(PART = 1L, POINT_NO = seq_len(.N) - 1L)]
            copy
        }
    }, by = "PLANE"]
    point[, PART_COUNT := data.table::uniqueN(PART), by = "PLANE"]
    point[, c("COORDINATE_KEY", "PROTECTED") := NULL]

    surface <- merge(metadata, point, by = "PLANE", allow.cartesian = TRUE)
    surface[, ORIGINAL_NAME := NAME]
    # Part 1 of a window host keeps the original name used by the window. Other
    # parts receive deterministic suffixes, mirrored in reciprocal references.
    surface[, PRESERVE_BASE_NAME := PLANE %in% window_plane & PART == 1L]
    surface[PART_COUNT > 1L & !PRESERVE_BASE_NAME,
        NAME := sprintf("%s [%d]", NAME, PART)]
    surface[PART_COUNT > 1L & !PRESERVE_BASE_NAME & !is.na(BOUNDARY_OBJECT),
        BOUNDARY_OBJECT := sprintf("%s [%d]", BOUNDARY_OBJECT, PART)]
    surface[, OUTPUT_ID := sprintf("%s-%d", ID, PART)]
    data.table::setorderv(surface, c("ID", "PART", "POINT_NO"))
    surface
}

# Insert zone-local junction coordinates that lie in the interior of a polygon
# edge. This does not detect duplicate polygons; it only gives incident faces
# the same edge segmentation. Candidate points are limited to incident rooms
# and tested with vectorized projection, so the work is O(edges * candidates)
# per middle plane instead of comparing every point in the building.
destep_split_surface_edges <- function(surface, all_coordinates, tolerance = 1e-8) {
    surface <- data.table::copy(surface)
    coordinates <- as.matrix(surface[, .(POINT_X, POINT_Y, POINT_Z)])
    candidates <- as.matrix(all_coordinates[, .(POINT_X, POINT_Y, POINT_Z)])
    output <- vector("list", nrow(surface))

    for (index in seq_len(nrow(surface))) {
        following <- index %% nrow(surface) + 1L
        start <- coordinates[index, ]
        difference <- coordinates[following, ] - start
        length_squared <- sum(difference ^ 2)
        if (length_squared <= tolerance ^ 2) {
            stop("A DeST surface polygon contains a zero-length edge.")
        }

        # For edge start A, direction d, and candidate P, the scalar projection
        # t = ((P - A) dot d) / (d dot d) locates P along the infinite edge line.
        # A point is strictly inside the segment when 0 < t < 1.
        relative <- sweep(candidates, 2L, start, "-")
        position <- as.vector(relative %*% difference / length_squared)
        # The residual (P - A) - t*d is the component perpendicular to the
        # edge. Its norm must be within tolerance for P to be collinear.
        projected <- relative - position * rep(difference, each = nrow(relative))
        on_segment <- position > tolerance & position < 1.0 - tolerance &
            sqrt(rowSums(projected ^ 2)) <= tolerance
        interior <- which(on_segment)

        value <- data.table::data.table(
            POINT_X = start[[1L]],
            POINT_Y = start[[2L]],
            POINT_Z = start[[3L]]
        )
        if (length(interior) > 0L) {
            order <- interior[order(position[interior])]
            value <- data.table::rbindlist(list(
                value,
                all_coordinates[order]
            ))
        }
        output[[index]] <- value
    }

    output <- data.table::rbindlist(output)
    output[, POINT_NO := seq_len(.N) - 1L]
    output
}

# Partition a window-bearing polygon only as far as needed to eliminate its
# protected collinear junctions. This routine runs only for window hosts that
# actually need splitting; it clips small ears next to junctions instead of
# triangulating the whole wall, so part 1 remains the largest window-hosting
# polygon and retains the original base-surface name.
destep_partition_window_surface_polygon <- function(surface, tolerance = 1e-10) {
    surface <- data.table::copy(surface)
    normal <- destep_surface_normal(surface)
    # Drop the coordinate aligned most strongly with the face normal. The
    # remaining two coordinates preserve the planar polygon for 2-D ear tests.
    projection <- setdiff(1:3, which.max(abs(normal)))
    xy <- as.matrix(surface[, .(POINT_X, POINT_Y, POINT_Z)])[, projection, drop = FALSE]
    signed_area <- sum(
        xy[, 1L] * xy[c(2:nrow(xy), 1L), 2L] -
            xy[c(2:nrow(xy), 1L), 1L] * xy[, 2L]
    ) / 2.0
    if (signed_area < 0.0) {
        # Ear clipping below assumes counterclockwise order, where a positive
        # 2-D cross product identifies a convex candidate ear.
        surface <- surface[nrow(surface):1L]
        xy <- xy[nrow(xy):1L, , drop = FALSE]
    }

    cross_2d <- function(a, b, c) {
        (b[[1L]] - a[[1L]]) * (c[[2L]] - a[[2L]]) -
            (b[[2L]] - a[[2L]]) * (c[[1L]] - a[[1L]])
    }
    inside_triangle <- function(point, a, b, c) {
        cross_2d(a, b, point) >= -tolerance &&
            cross_2d(b, c, point) >= -tolerance &&
            cross_2d(c, a, point) >= -tolerance
    }
    is_ear <- function(position, remaining) {
        previous <- remaining[(position - 2L) %% length(remaining) + 1L]
        current <- remaining[[position]]
        following <- remaining[position %% length(remaining) + 1L]
        area <- cross_2d(xy[previous, ], xy[current, ], xy[following, ])
        # A valid ear must be convex and its triangle must contain no other
        # polygon vertex; otherwise clipping it could cross a concavity.
        if (area <= tolerance) return(NULL)
        other <- setdiff(remaining, c(previous, current, following))
        contains <- vapply(other, function(index) {
            inside_triangle(
                xy[index, ], xy[previous, ], xy[current, ], xy[following, ]
            )
        }, logical(1L))
        if (any(contains)) return(NULL)
        list(indices = c(previous, current, following), area = area / 2.0)
    }

    remaining <- seq_len(nrow(surface))
    clipped <- list()
    repeat {
        subset <- surface[remaining]
        redundant <- which(
            destep_redundant_surface_vertices(subset) & subset$PROTECTED
        )
        if (length(redundant) == 0L) break

        junction <- redundant[[1L]]
        # Clip one of the two convex ears adjacent to the straight-through
        # junction. Choosing the smaller valid ear preserves the largest
        # possible remainder for the window base surface.
        candidates <- unique(c(
            (junction - 2L) %% length(remaining) + 1L,
            junction %% length(remaining) + 1L
        ))
        ears <- lapply(candidates, is_ear, remaining = remaining)
        valid <- which(!vapply(ears, is.null, logical(1L)))
        if (length(valid) == 0L) {
            stop("Could not partition a window-bearing DeST surface polygon.")
        }
        selected <- valid[[which.min(vapply(
            ears[valid], function(ear) ear$area, double(1L)
        ))]]
        clipped[[length(clipped) + 1L]] <- ears[[selected]]$indices
        remaining <- remaining[-candidates[[selected]]]
    }

    part <- list(remaining)
    if (length(clipped) > 0L) part <- c(part, clipped)
    data.table::rbindlist(lapply(seq_along(part), function(index) {
        value <- data.table::copy(surface[part[[index]]])
        value[, `:=`(PART = index, POINT_NO = seq_len(.N) - 1L)]
        value
    }))
}

# Mark redundant vertices inside one ordered polygon; this function does not
# decide whether two complete polygons are duplicates. A vertex is redundant
# when it coincides with either neighbor, or when the incoming and outgoing
# edges continue in the same straight direction. Keeping this calculation
# separate ensures topology splitting and simplification use the same tolerance.
destep_redundant_surface_vertices <- function(surface, tolerance = 1e-8) {
    n_vertex <- nrow(surface)
    previous <- c(n_vertex, seq_len(n_vertex - 1L))
    following <- c(seq.int(2L, n_vertex), 1L)
    coordinates <- as.matrix(surface[, .(POINT_X, POINT_Y, POINT_Z)])
    incoming <- coordinates - coordinates[previous, , drop = FALSE]
    outgoing <- coordinates[following, , drop = FALSE] - coordinates

    incoming_length <- sqrt(rowSums(incoming ^ 2))
    outgoing_length <- sqrt(rowSums(outgoing ^ 2))
    cross_product <- cbind(
        incoming[, 2L] * outgoing[, 3L] - incoming[, 3L] * outgoing[, 2L],
        incoming[, 3L] * outgoing[, 1L] - incoming[, 1L] * outgoing[, 3L],
        incoming[, 1L] * outgoing[, 2L] - incoming[, 2L] * outgoing[, 1L]
    )
    # A near-zero adjacent edge means the current vertex duplicates its previous
    # or next coordinate. For non-zero edges, |incoming x outgoing| close to zero
    # means collinear; the positive dot product requires straight continuation
    # and deliberately retains a collinear reversal or backtracking edge.
    incoming_length <= tolerance |
        outgoing_length <= tolerance |
        (
            sqrt(rowSums(cross_product ^ 2)) <=
                tolerance * pmax(1, incoming_length * outgoing_length) &
            rowSums(incoming * outgoing) > 0
        )
}

# Triangulate a simple planar polygon with ear clipping while retaining every
# protected boundary junction. It is called only for planes that still contain
# required collinear junctions after simplification, not for every surface.
# This dependency-free R implementation is cubic in the worst case because each
# candidate ear checks remaining vertices, but surface polygons are processed
# independently and normally contain few vertices. Each resulting triangle is a
# valid EnergyPlus base surface, and peer faces reuse identical part numbering.
destep_triangulate_surface_polygon <- function(surface, tolerance = 1e-10) {
    surface <- data.table::copy(surface)
    normal <- destep_surface_normal(surface)
    # Project the planar 3-D face onto its most stable 2-D coordinate pair.
    projection <- setdiff(1:3, which.max(abs(normal)))
    xy <- as.matrix(surface[, .(POINT_X, POINT_Y, POINT_Z)])[, projection, drop = FALSE]
    signed_area <- sum(
        xy[, 1L] * xy[c(2:nrow(xy), 1L), 2L] -
            xy[c(2:nrow(xy), 1L), 1L] * xy[, 2L]
    ) / 2.0
    if (signed_area < 0.0) {
        # Normalize to counterclockwise order so positive cross products are
        # convex turns throughout the ear-clipping loop.
        surface <- surface[nrow(surface):1L]
        xy <- xy[nrow(xy):1L, , drop = FALSE]
    }

    cross_2d <- function(a, b, c) {
        (b[[1L]] - a[[1L]]) * (c[[2L]] - a[[2L]]) -
            (b[[2L]] - a[[2L]]) * (c[[1L]] - a[[1L]])
    }
    inside_triangle <- function(point, a, b, c) {
        cross_2d(a, b, point) >= -tolerance &&
            cross_2d(b, c, point) >= -tolerance &&
            cross_2d(c, a, point) >= -tolerance
    }

    remaining <- seq_len(nrow(surface))
    triangle <- list()
    while (length(remaining) > 3L) {
        found <- FALSE
        for (position in seq_along(remaining)) {
            previous <- remaining[(position - 2L) %% length(remaining) + 1L]
            current <- remaining[[position]]
            following <- remaining[position %% length(remaining) + 1L]
            # A non-positive turn is concave or collinear and cannot be an ear.
            if (cross_2d(xy[previous, ], xy[current, ], xy[following, ]) <= tolerance) {
                next
            }
            other <- setdiff(remaining, c(previous, current, following))
            contains <- vapply(other, function(index) {
                inside_triangle(
                    xy[index, ], xy[previous, ], xy[current, ], xy[following, ]
                )
            }, logical(1L))
            # The diagonal from previous to following stays inside a simple
            # polygon only when no remaining vertex lies in the ear triangle.
            if (any(contains)) next

            triangle[[length(triangle) + 1L]] <- c(previous, current, following)
            remaining <- remaining[-position]
            found <- TRUE
            break
        }
        if (!found) stop("Could not triangulate a DeST surface polygon.")
    }
    triangle[[length(triangle) + 1L]] <- remaining

    data.table::rbindlist(lapply(seq_along(triangle), function(part) {
        value <- data.table::copy(surface[triangle[[part]]])
        value[, `:=`(PART = part, POINT_NO = 0:2)]
        value
    }))
}

# Remove redundant vertices from one ordered DeST surface polygon while
# retaining turns and at least the three vertices needed for a valid face.
destep_simplify_surface_polygon <- function(surface, tolerance = 1e-8) {
    surface <- data.table::copy(surface)

    repeat {
        n_vertex <- nrow(surface)
        if (n_vertex <= 3L) break

        redundant <- destep_redundant_surface_vertices(surface, tolerance)
        if ("PROTECTED" %in% names(surface)) {
            redundant <- redundant & !surface$PROTECTED
        }

        if (!any(redundant) || n_vertex - sum(redundant) < 3L) break
        surface <- surface[!redundant]
    }

    data.table::set(surface, NULL, "POINT_NO", seq_len(nrow(surface)) - 1L)
    surface
}
