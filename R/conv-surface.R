# SURFACE|MAIN_ENCLOSURE|PLANE -> BuildingSurface:Detailed
surface__convert <- function(
    dest, ep, geometry_profile = eplus_geom__profile(ep$version())
) {
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
            -- DeST construction layers run from SIDE1 (left/upper) to SIDE2
            -- (right/lower). EnergyPlus lists layers outside-to-inside for
            -- each zone face, so the SIDE1 face needs the reversed stack.
            CASE
                WHEN E.SIDE = 1 THEN E.CONSTRUCTION || ' [Reverse]'
                ELSE E.CONSTRUCTION
            END                                            AS CONSTRUCTION,
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
            STOREY.ID                                      AS STOREY_ID,
            STOREY.NAME                                    AS STOREY_NAME,
            STOREY.MULTIPLE                                AS STOREY_MULTIPLIER,
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
        LEFT JOIN STOREY
        ON ROOM.OF_STOREY = STOREY.ID
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
    surface <- surface__snap_coordinates(surface, geometry_profile)

    # Normalize each shared middle-plane polygon once before the two room-side
    # copies are oriented. Vertices used by another plane are topological
    # junctions and must survive collinear-point cleanup.
    window <- data.table::data.table()
    if (db_has_rows(dest, "WINDOW")) {
        window <- data.table::as.data.table(DBI::dbGetQuery(dest, "
            SELECT
                W.ID AS WINDOW_ID,
                E.MIDDLE_PLANE AS PLANE,
                L.POINT_NO,
                ROUND(P.X, 3) AS POINT_X,
                ROUND(P.Y, 3) AS POINT_Y,
                ROUND(P.Z, 3) AS POINT_Z
            FROM WINDOW W
            INNER JOIN MAIN_ENCLOSURE E ON W.OF_ENCLOSURE = E.ID
            INNER JOIN PLANE PL ON W.MIDDLE_PLANE = PL.PLANE_ID
            INNER JOIN GEOMETRY G ON PL.GEOMETRY = G.GEOMETRY_ID
            INNER JOIN LOOP_POINT L ON G.BOUNDARY_LOOP_ID = L.LOOP_ID
            INNER JOIN POINT P ON L.POINT = P.POINT_ID
            ORDER BY W.ID, L.POINT_NO
        "))
    }
    surface <- surface__normalize_topology(surface, window, geometry_profile)
    # remove the surface indicating outside environment and grounds
    surface <- surface[!J(c(1L, 2L)), on = "TYPE_SURFACE"]
    surface <- surface__apply_typical_storey_boundaries(
        surface, window, geometry_profile
    )

    # Orient every polygon from DeST's source azimuth and tilt. This also makes
    # exposed floors face downward without relying on enclosure side numbers.
    south_direction <- geom__south_direction(dest)
    surface <- surface[
        , geom__orient_surface_polygon(.SD, south_direction, geometry_profile),
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
            geom__eplus_vertex_values(.SD)
        )))
    ]$value

    # remove space name field
    if (ep$version() <= "9.5") {
        ind <- which(names(value[[1L]]) == "space_name")
        if (length(ind) > 0L) {
            value <- lapply(value, .subset, -ind)
        }
    }

    out <- conv__add_objects(dest, ep, "BuildingSurface:Detailed", value)

    # always attach the table to the output in case it is useful later
    attr(out, "table") <- surface

    out
}

# Replace every horizontal boundary of a multiplied storey with a cyclic
# floor-to-ceiling pair. The common overlay is built for the complete storey,
# rather than per room, because the zones above and below a repeated floor can
# partition the same footprint differently. Source faces remain traceable by
# ID and plane, while cut faces in adjacent non-multiplied storeys become
# self-referenced adiabatic surfaces.
# Prepare the source metadata and select the horizontal faces that participate
# in typical-storey rewiring.
surface__prepare_typical_storey <- function(
    surface, window = data.table::data.table(),
    profile = eplus_geom__profile()
) {
    surface <- data.table::copy(surface)
    surface[, `:=`(
        SOURCE_TYPE = TYPE,
        SOURCE_SIDE = SIDE,
        SOURCE_CONSTRUCTION = CONSTRUCTION,
        SOURCE_BOUNDARY = BOUNDARY,
        SOURCE_BOUNDARY_OBJECT = BOUNDARY_OBJECT,
        BOUNDARY_MODE = "source"
    )]

    # DeST's horizontal direction sentinels identify the two complete faces of
    # a repeated floor even when part of one face was originally an exterior
    # roof or exposed floor.
    target <- surface[
        STOREY_MULTIPLIER > 1L & AZIMUTH %in% c(-999.0, 999.0)
    ]
    if (nrow(target) == 0L) {
        return(list(surface = surface, target = target))
    }

    if (nrow(window) > 0L && any(target$PLANE %in% window$PLANE)) {
        stop(paste(
            "Typical-storey approximation does not support a window on a",
            "rewired horizontal surface."
        ))
    }

    list(surface = surface, target = target)
}

# Return the construction name without the explicit reverse-stack suffix.
surface__construction_base <- function(value) {
    sub(" \\[Reverse\\]$", "", value)
}

# Reconstruct one source face from the exterior edges of any triangles or
# convex parts created by the earlier EnergyPlus topology normalization. Each
# internal mesh edge occurs twice, so one-occurrence edges remove diagonals.
surface__polygon_region <- function(value) {
    edge <- data.table::rbindlist(lapply(unique(value$OUTPUT_ID), function(output_id) {
        part <- value[OUTPUT_ID == output_id]
        following <- seq_len(nrow(part)) %% nrow(part) + 1L
        data.table::data.table(
            START = sprintf("%.12f|%.12f", part$POINT_X, part$POINT_Y),
            END = sprintf(
                "%.12f|%.12f",
                part$POINT_X[following], part$POINT_Y[following]
            ),
            START_X = part$POINT_X,
            START_Y = part$POINT_Y,
            END_X = part$POINT_X[following],
            END_Y = part$POINT_Y[following]
        )
    }))
    edge[, EDGE := geom__edge_key(START, END)]
    count <- edge[, .N, by = "EDGE"]
    boundary <- edge[count[N == 1L], on = "EDGE", nomatch = 0L]
    coordinate <- unique(data.table::rbindlist(list(
        boundary[, .(KEY = START, X = START_X, Y = START_Y)],
        boundary[, .(KEY = END, X = END_X, Y = END_Y)]
    )), by = "KEY")

    region <- list()
    while (nrow(boundary) > 0L) {
        start <- boundary$START[[1L]]
        current <- boundary$END[[1L]]
        path <- c(start, current)
        boundary <- boundary[-1L]
        while (current != start) {
            incident <- which(
                boundary$START == current | boundary$END == current
            )
            if (length(incident) != 1L) {
                stop("A normalized DeST surface does not have a simple boundary cycle.")
            }
            selected <- incident[[1L]]
            following <- if (boundary$START[[selected]] == current) {
                boundary$END[[selected]]
            } else {
                boundary$START[[selected]]
            }
            boundary <- boundary[-selected]
            current <- following
            if (current != start) path <- c(path, current)
        }
        point <- coordinate[match(path, coordinate$KEY)]
        region[[length(region) + 1L]] <- list(x = point$X, y = point$Y)
    }
    region
}

# Build paired floor and ceiling polygons from the complete planar overlap of
# each multiplied storey's horizontal faces.
surface__rebuild_typical_storeys <- function(target, profile) {
    tolerance <- profile$plane_distance
    coordinate_columns <- geom__coordinate_columns()
    rebuilt <- list()
    pair_index <- 0L
    storey_ids <- sort(unique(target$STOREY_ID))
    for (storey_id in storey_ids) {
        storey <- target[STOREY_ID == storey_id]
        down_ids <- unique(storey[AZIMUTH == 999.0, ID])
        up_ids <- unique(storey[AZIMUTH == -999.0, ID])
        if (length(down_ids) == 0L || length(up_ids) == 0L) {
            stop(sprintf(
                "Multiplied DeST storey '%s' needs both floor and ceiling faces.",
                storey$STOREY_NAME[[1L]]
            ))
        }

        # A unique middle-floor construction is the only defensible fallback
        # for portions whose source boundary was Roof or exposed Floor. Local
        # middle-floor faces still take precedence when they are available.
        default_construction <- unique(surface__construction_base(
            storey[KIND_ENCLOSURE == 5L, CONSTRUCTION]
        ))
        default_construction <- default_construction[!is.na(default_construction)]
        if (length(default_construction) != 1L) {
            stop(sprintf(
                paste(
                    "Multiplied DeST storey '%s' must have exactly one",
                    "middle-floor construction for typical-storey approximation."
                ),
                storey$STOREY_NAME[[1L]]
            ))
        }

        for (down_id in down_ids) {
            down <- storey[ID == down_id]
            if (diff(range(down$POINT_Z)) > tolerance) {
                stop("A typical-storey floor surface is not horizontal.")
            }
            for (up_id in up_ids) {
                up <- storey[ID == up_id]
                if (diff(range(up$POINT_Z)) > tolerance) {
                    stop("A typical-storey ceiling surface is not horizontal.")
                }

                # Intersect the original face regions in plan after discarding
                # all auxiliary triangulation diagonals. Keeping those diagonals
                # would create millimetre-scale slivers at their crossings.
                overlap <- polyclip::polyclip(
                    surface__polygon_region(down), surface__polygon_region(up),
                    op = "intersection", eps = profile$intersection
                )
                if (length(overlap) == 0L) next

                local_construction <- unique(surface__construction_base(c(
                    down[KIND_ENCLOSURE == 5L, CONSTRUCTION],
                    up[KIND_ENCLOSURE == 5L, CONSTRUCTION]
                )))
                local_construction <- local_construction[!is.na(local_construction)]
                if (length(local_construction) > 1L) {
                    stop("Overlapping typical-storey faces use different floor constructions.")
                }
                construction <- if (length(local_construction) == 1L) {
                    local_construction[[1L]]
                } else {
                    default_construction[[1L]]
                }

                down_metadata <- down[1L,
                    setdiff(names(down), coordinate_columns), with = FALSE
                ]
                up_metadata <- up[1L,
                    setdiff(names(up), coordinate_columns), with = FALSE
                ]
                down_metadata[, `:=`(
                    SOURCE_ID = ID,
                    SOURCE_NAME = ORIGINAL_NAME,
                    TYPE = "Floor",
                    SIDE = 1L,
                    CONSTRUCTION = sprintf("%s [Reverse]", construction),
                    BOUNDARY = "Surface",
                    BOUNDARY_OBJECT = NA_character_,
                    AZIMUTH = 999.0,
                    TILT = 0.0,
                    BOUNDARY_MODE = "typical_cycle"
                )]
                up_metadata[, `:=`(
                    SOURCE_ID = ID,
                    SOURCE_NAME = ORIGINAL_NAME,
                    TYPE = "Ceiling",
                    SIDE = 2L,
                    CONSTRUCTION = construction,
                    BOUNDARY = "Surface",
                    BOUNDARY_OBJECT = NA_character_,
                    AZIMUTH = -999.0,
                    TILT = 0.0,
                    BOUNDARY_MODE = "typical_cycle"
                )]

                for (contour in overlap) {
                    polygon <- data.table::data.table(
                        POINT_X = contour$x,
                        POINT_Y = contour$y,
                        POINT_Z = up$POINT_Z[[1L]]
                    )
                    polygon[, POINT_NO := seq_len(.N) - 1L]
                    polygon <- surface__simplify_polygon(polygon, profile)
                    if (nrow(polygon) < 3L ||
                        geom__polygon_area(polygon) <= profile$area) next
                    # A convex overlap is already a valid synchronized part on
                    # both sides. Preserve it intact and triangulate only a
                    # concave overlap that EnergyPlus cannot use reliably for
                    # shadow receiving or casting.
                    triangle <- if (
                        nrow(polygon) == 3L ||
                            geom__polygon_is_convex(polygon, profile$angle)
                    ) {
                        polygon[, PART := 1L]
                    } else {
                        tryCatch(
                            surface__triangulate_polygon(
                                polygon, profile = profile
                            ),
                            error = function(error) {
                                stop(sprintf(
                                    paste(
                                        "Could not triangulate typical-storey",
                                        "overlap between source surfaces %s and %s: %s"
                                    ),
                                    down_id, up_id, conditionMessage(error)
                                ))
                            }
                        )
                    }
                    triangle <- surface__merge_convex_parts(triangle, profile)
                    for (part in unique(triangle$PART)) {
                        up_geometry <- triangle[PART == part]
                        up_geometry[, c("PART") := NULL]
                        up_geometry[, POINT_NO := seq_len(.N) - 1L]
                        down_geometry <- data.table::copy(up_geometry)
                        down_geometry[, POINT_Z := down$POINT_Z[[1L]]]

                        pair_index <- pair_index + 1L
                        pair_id <- sprintf("%s-%05d", storey_id, pair_index)
                        down_part <- data.table::copy(down_metadata)
                        up_part <- data.table::copy(up_metadata)
                        down_part[, TYPICAL_PAIR_ID := pair_id]
                        up_part[, TYPICAL_PAIR_ID := pair_id]
                        rebuilt[[length(rebuilt) + 1L]] <- cbind(
                            down_part[rep(1L, nrow(down_geometry))], down_geometry
                        )
                        rebuilt[[length(rebuilt) + 1L]] <- cbind(
                            up_part[rep(1L, nrow(up_geometry))], up_geometry
                        )
                    }
                }
            }
        }
    }
    rebuilt <- data.table::rbindlist(rebuilt, fill = TRUE)
    if (nrow(rebuilt) == 0L) {
        stop("Typical-storey floor and ceiling footprints do not overlap.")
    }
    rebuilt
}

# Check that the common overlay covers every original source face exactly once.
surface__validate_typical_storey_area <- function(target, rebuilt, profile) {
    tolerance <- profile$plane_distance
    source_area <- target[, .(
        PART_AREA = geom__polygon_area(.SD)
    ), by = .(ID, OUTPUT_ID)][, .(
        SOURCE_AREA = sum(PART_AREA)
    ), by = "ID"]
    rebuilt_area <- rebuilt[, .(
        REBUILT_AREA = geom__polygon_area(.SD)
    ), by = .(ID = SOURCE_ID, TYPICAL_PAIR_ID)]
    rebuilt_area <- rebuilt_area[, .(
        REBUILT_AREA = sum(REBUILT_AREA)
    ), by = "ID"][source_area, on = "ID"]
    rebuilt_area[, ERROR := REBUILT_AREA - SOURCE_AREA]
    area_tolerance <- pmax(
        profile$area, tolerance * abs(rebuilt_area$SOURCE_AREA)
    )
    if (anyNA(rebuilt_area$REBUILT_AREA) ||
        any(abs(rebuilt_area$ERROR) > area_tolerance)) {
        failure <- rebuilt_area[
            is.na(REBUILT_AREA) | abs(ERROR) > area_tolerance
        ][1L]
        stop(sprintf(
            paste(
                "Typical-storey overlay does not preserve source surface %s area:",
                "source %.12g m2, rebuilt %.12g m2, error %.12g m2."
            ),
            failure$ID, failure$SOURCE_AREA,
            failure$REBUILT_AREA, failure$ERROR
        ))
    }
    invisible(rebuilt)
}

# Assign deterministic part identifiers, names, and reciprocal pair references
# to source faces split by the typical-storey overlay.
surface__name_typical_storey_parts <- function(rebuilt) {
    data.table::setorderv(rebuilt, c(
        "STOREY_ID", "SOURCE_ID", "TYPICAL_PAIR_ID", "POINT_NO"
    ))
    rebuilt[, TYPICAL_PART := data.table::rleid(TYPICAL_PAIR_ID),
        by = "SOURCE_ID"]
    rebuilt[, TYPICAL_PART_COUNT := data.table::uniqueN(TYPICAL_PAIR_ID),
        by = "SOURCE_ID"]
    rebuilt[, NAME := ifelse(
        TYPICAL_PART_COUNT == 1L,
        SOURCE_NAME,
        sprintf("%s [Typical %d]", SOURCE_NAME, TYPICAL_PART)
    )]
    rebuilt[, PART := data.table::rleid(TYPICAL_PAIR_ID), by = "ID"]
    rebuilt[, PART_COUNT := data.table::uniqueN(PART), by = "ID"]
    rebuilt[, OUTPUT_ID := sprintf("%s-T%d", ID, PART)]
    pair_name <- unique(rebuilt[, .(
        TYPICAL_PAIR_ID, TYPE, NAME
    )])
    floor_name <- pair_name[TYPE == "Floor", .(
        TYPICAL_PAIR_ID, FLOOR_NAME = NAME
    )]
    ceiling_name <- pair_name[TYPE == "Ceiling", .(
        TYPICAL_PAIR_ID, CEILING_NAME = NAME
    )]
    pair_name <- merge(floor_name, ceiling_name, by = "TYPICAL_PAIR_ID")
    rebuilt[pair_name, on = "TYPICAL_PAIR_ID", BOUNDARY_OBJECT := ifelse(
        TYPE == "Floor", i.CEILING_NAME, i.FLOOR_NAME
    )]
    rebuilt
}

# Replace every multiplied-storey horizontal boundary with a cyclic paired
# floor and ceiling overlay while keeping adjacent cut faces adiabatic.
surface__apply_typical_storey_boundaries <- function(
    surface, window = data.table::data.table(),
    profile = eplus_geom__profile()
) {
    prepared <- surface__prepare_typical_storey(surface, window, profile)
    surface <- prepared$surface
    target <- prepared$target
    if (nrow(target) == 0L) return(surface)

    target_output <- unique(target$OUTPUT_ID)
    counterpart <- unique(target[
        BOUNDARY == "Surface" & !is.na(BOUNDARY_OBJECT), BOUNDARY_OBJECT
    ])
    # A neighboring first/top-storey face cannot reference a source face that
    # is repurposed as a cyclic typical boundary; self-reference makes it
    # adiabatic while retaining its thermal mass.
    cut <- surface[
        NAME %in% counterpart & !OUTPUT_ID %in% target_output,
        unique(OUTPUT_ID)
    ]
    surface[OUTPUT_ID %in% cut, `:=`(
        BOUNDARY = "Surface",
        BOUNDARY_OBJECT = NAME,
        BOUNDARY_MODE = "typical_cut_adiabatic"
    )]

    rebuilt <- surface__rebuild_typical_storeys(target, profile)
    surface__validate_typical_storey_area(target, rebuilt, profile)
    rebuilt <- surface__name_typical_storey_parts(rebuilt)

    surface <- data.table::rbindlist(list(
        surface[!OUTPUT_ID %in% target_output],
        rebuilt
    ), fill = TRUE)
    data.table::setorderv(surface, c("ID", "PART", "POINT_NO"))
    surface <- surface__normalize_room_junctions(
        surface, window, profile
    )

    # All non-adiabatic Surface references must be reciprocal after rewiring.
    reference <- unique(surface[, .(NAME, BOUNDARY_OBJECT)])
    peer_index <- match(reference$BOUNDARY_OBJECT, reference$NAME)
    unresolved <- reference[
        !is.na(BOUNDARY_OBJECT) & is.na(peer_index)
    ]
    nonmutual <- reference[
        !is.na(BOUNDARY_OBJECT) & NAME != BOUNDARY_OBJECT &
            reference$BOUNDARY_OBJECT[peer_index] != NAME
    ]
    if (nrow(unresolved) > 0L || nrow(nonmutual) > 0L) {
        stop("Typical-storey rewiring produced a non-reciprocal surface reference.")
    }

    surface
}

# Return rooms that fail the profile's EnergyPlus two-pass closure test.
# The second pass inserts missing collinear points, so harmless T-junction edge
# segmentation does not trigger expensive exported-surface normalization.
surface__energyplus_unclosed_rooms <- function(
    surface, profile = eplus_geom__profile()
) {
    vertex_tolerance <- profile$closure_vertex_distance
    room_is_closed <- function(room) {
        output_ids <- unique(room$OUTPUT_ID)
        faces <- lapply(output_ids, function(output_id) {
            as.matrix(room[OUTPUT_ID == output_id,
                .(POINT_X, POINT_Y, POINT_Z)])
        })
        if (length(faces) == 0L) return(FALSE)

        # EnergyPlus keeps the first coordinate in traversal order as the
        # representative of every coordinate-wise 1.27 cm vertex cluster.
        unique_vertices <- matrix(numeric(), nrow = 0L, ncol = 3L)
        vertex_index <- function(point) {
            if (nrow(unique_vertices) > 0L) {
                close <- which(apply(
                    abs(sweep(unique_vertices, 2L, point, "-")),
                    1L, max
                ) < vertex_tolerance)
                if (length(close) > 0L) return(close[[1L]])
            }
            unique_vertices <<- rbind(unique_vertices, point)
            nrow(unique_vertices)
        }
        face_indices <- lapply(faces, function(face) {
            apply(face, 1L, vertex_index)
        })
        count_edges <- function(indices) {
            edge <- unlist(lapply(indices, function(index) {
                following <- c(index[-1L], index[[1L]])
                geom__edge_key(index, following)
            }), use.names = FALSE)
            table(edge)
        }

        first_count <- count_edges(face_indices)
        if (length(first_count) > 0L && all(first_count == 2L)) return(TRUE)

        # Only a failed first pass receives every near-collinear room vertex.
        # This mirrors EnergyPlus's computationally intensive fallback and
        # avoids creating new high-count edges in an already closed shell.
        expanded_indices <- lapply(faces, function(face) {
            expanded <- integer()
            following <- seq_len(nrow(face)) %% nrow(face) + 1L
            for (index in seq_len(nrow(face))) {
                start <- face[index, ]
                end <- face[following[[index]], ]
                direction <- end - start
                edge_length <- sqrt(sum(direction ^ 2))
                if (edge_length <= profile$zero_distance) next

                start_distance <- abs(sweep(
                    unique_vertices, 2L, start, "-"
                ))
                end_distance <- abs(sweep(
                    unique_vertices, 2L, end, "-"
                ))
                not_endpoint <- apply(start_distance, 1L, max) >=
                    vertex_tolerance & apply(end_distance, 1L, max) >=
                    vertex_tolerance
                relative <- sweep(unique_vertices, 2L, start, "-")
                unit <- direction / edge_length
                perpendicular <- relative - outer(
                    as.vector(relative %*% unit), unit
                )
                line_distance <- sqrt(rowSums(perpendicular ^ 2))
                between_error <- abs(
                    edge_length - sqrt(rowSums(relative ^ 2)) -
                        sqrt(rowSums(end_distance ^ 2))
                )
                candidate <- which(
                    not_endpoint & line_distance < vertex_tolerance &
                        between_error < vertex_tolerance
                )
                if (length(candidate) > 0L) {
                    position <- as.vector(
                        relative[candidate, , drop = FALSE] %*% direction
                    ) / sum(direction ^ 2)
                    candidate <- candidate[order(position)]
                }
                expanded <- c(expanded, vertex_index(start), candidate)
            }
            expanded[c(TRUE, diff(expanded) != 0L)]
        })
        second_count <- count_edges(expanded_indices)
        length(second_count) > 0L && all(second_count == 2L)
    }

    rooms <- unique(surface$ROOM)
    closed <- vapply(rooms, function(room_name) {
        room_is_closed(surface[
            ROOM == room_name &
                TYPE %in% c("Wall", "Floor", "Ceiling", "Roof")
        ])
    }, logical(1L))
    rooms[!closed]
}

# Split surface edges at every coplanar room-shell junction introduced by the
# typical-storey overlay. Reciprocal surface pairs are triangulated from one
# common polygon and translated to the peer plane, preserving both exact shell
# closure and one-to-one EnergyPlus boundary references.
surface__normalize_room_junctions <- function(
    surface, window = data.table::data.table(),
    profile = eplus_geom__profile()
) {
    tolerance <- profile$plane_distance
    distance_tolerance <- profile$coordinate_distance
    surface <- data.table::copy(surface)
    coordinate_columns <- geom__coordinate_columns()
    object <- unique(surface[, .(
        OUTPUT_ID, NAME, ROOM, BOUNDARY, BOUNDARY_OBJECT, BOUNDARY_MODE
    )])
    peer_index <- match(object$BOUNDARY_OBJECT, object$NAME)
    object[, PEER_OUTPUT_ID := object$OUTPUT_ID[peer_index]]
    object[, GROUP := ifelse(
        BOUNDARY == "Surface" & NAME != BOUNDARY_OBJECT & !is.na(PEER_OUTPUT_ID),
        ifelse(
            OUTPUT_ID < PEER_OUTPUT_ID,
            paste(OUTPUT_ID, PEER_OUTPUT_ID, sep = "|"),
            paste(PEER_OUTPUT_ID, OUTPUT_ID, sep = "|")
        ),
        OUTPUT_ID
    )]
    repair_rooms <- surface__energyplus_unclosed_rooms(surface, profile)
    if (length(repair_rooms) == 0L) return(surface)
    repair_groups <- unique(object[ROOM %in% repair_rooms]$GROUP)
    repair_member_rooms <- unique(object[GROUP %in% repair_groups]$ROOM)
    room_point <- unique(surface[
        ROOM %in% repair_member_rooms,
        .(ROOM, POINT_X, POINT_Y, POINT_Z)
    ])
    # A cyclic face can acquire a junction by projecting a room-shell vertex
    # onto its horizontal plane. Precompute the same projections so incident
    # walls see those vertices regardless of group traversal order.
    typical_object <- object[
        GROUP %in% repair_groups & BOUNDARY_MODE == "typical_cycle"
    ]
    projected_point <- lapply(seq_len(nrow(typical_object)), function(index) {
        typical_id <- typical_object$OUTPUT_ID[[index]]
        typical_room <- typical_object$ROOM[[index]]
        typical <- surface[OUTPUT_ID == typical_id]
        candidate <- room_point[ROOM == typical_room]
        normal <- geom__unit_normal(typical)
        origin <- as.numeric(typical[1L, .(POINT_X, POINT_Y, POINT_Z)])
        coordinate <- as.matrix(candidate[, .(POINT_X, POINT_Y, POINT_Z)])
        plane_distance <- as.vector(
            sweep(coordinate, 2L, origin, "-") %*% normal
        )
        coordinate <- coordinate -
            plane_distance * rep(normal, each = nrow(coordinate))
        data.table::data.table(
            ROOM = typical_room,
            POINT_X = coordinate[, 1L],
            POINT_Y = coordinate[, 2L],
            POINT_Z = coordinate[, 3L]
        )
    })
    room_point <- unique(data.table::rbindlist(
        c(list(room_point), projected_point), fill = TRUE
    ))
    output <- list()

    for (group in unique(object$GROUP)) {
        member <- object[GROUP == group]
        base_id <- member$OUTPUT_ID[[1L]]
        base <- surface[OUTPUT_ID == base_id]
        paired <- nrow(member) == 2L
        peer <- if (paired) {
            surface[OUTPUT_ID == member$OUTPUT_ID[[2L]]]
        } else {
            NULL
        }
        if (!group %in% repair_groups) {
            output[[length(output) + 1L]] <- base
            if (paired) output[[length(output) + 1L]] <- peer
            next
        }

        rooms <- unique(member$ROOM)
        project_parallel <- paired &&
            any(member$BOUNDARY_MODE == "typical_cycle")
        candidate <- room_point[ROOM %in% rooms]
        normal <- geom__unit_normal(base)
        origin <- as.numeric(base[1L, .(POINT_X, POINT_Y, POINT_Z)])
        coordinate <- as.matrix(candidate[, .(POINT_X, POINT_Y, POINT_Z)])
        plane_distance <- as.vector(sweep(coordinate, 2L, origin, "-") %*% normal)
        if (project_parallel) {
            coordinate <- coordinate - plane_distance * rep(normal, each = nrow(coordinate))
        } else {
            coordinate <- coordinate[abs(plane_distance) <= tolerance, , drop = FALSE]
        }
        candidate <- unique(data.table::data.table(
            POINT_X = coordinate[, 1L],
            POINT_Y = coordinate[, 2L],
            POINT_Z = coordinate[, 3L]
        ))
        candidate[, JUNCTION_KEY := sprintf(
            "%.8f|%.8f|%.8f", POINT_X, POINT_Y, POINT_Z
        )]
        candidate <- unique(candidate, by = "JUNCTION_KEY")
        candidate[, JUNCTION_KEY := NULL]
        split_profile <- profile
        split_profile$coordinate_distance <-
            distance_tolerance * (1.0 + 1e-6)
        split <- surface__split_edges(base, candidate, split_profile)
        changed <- nrow(split) > nrow(base)
        if (!changed) {
            output[[length(output) + 1L]] <- base
            if (paired) output[[length(output) + 1L]] <- peer
            next
        }

        avoid_points <- if (nrow(window) > 0L) {
            window[PLANE == base$PLANE[[1L]]]
        } else {
            data.table::data.table()
        }
        # A center fan preserves every newly inserted boundary segment. Ordinary
        # ear clipping may legally bypass a collinear junction with one longer
        # diagonal, which reopens the room shell even though total area matches.
        triangle <- if (nrow(avoid_points) == 0L &&
            geom__polygon_is_convex(split, profile$angle)) {
            center <- colMeans(as.matrix(
                split[, .(POINT_X, POINT_Y, POINT_Z)]
            ))
            radial <- sqrt(rowSums(sweep(
                as.matrix(split[, .(POINT_X, POINT_Y, POINT_Z)]),
                2L, center, "-"
            ) ^ 2))
            if (any(radial < distance_tolerance)) {
                stop(sprintf(
                    "Could not normalize room junctions for surface group %s without a short radial edge.",
                    group
                ))
            }
            data.table::rbindlist(lapply(seq_len(nrow(split)), function(index) {
                following <- index %% nrow(split) + 1L
                value <- data.table::copy(split[c(index, index, following)])
                value[1L, `:=`(
                    POINT_X = center[[1L]],
                    POINT_Y = center[[2L]],
                    POINT_Z = center[[3L]]
                )]
                value[, `:=`(PART = index, POINT_NO = 0:2)]
                value
            }))
        } else {
            tryCatch(
                surface__triangulate_polygon(split, avoid_points, profile),
                error = function(error) {
                    stop(sprintf(
                        "Could not normalize room junctions for surface group %s: %s",
                        group, conditionMessage(error)
                    ))
                }
            )
        }
        triangle <- surface__merge_convex_parts(triangle, profile)
        part_ids <- unique(triangle$PART)
        base_metadata <- base[1L,
            setdiff(names(base), coordinate_columns), with = FALSE
        ]
        base_metadata[, SOURCE_JUNCTION_OUTPUT_ID := OUTPUT_ID]
        if (paired) {
            peer_metadata <- peer[1L,
                setdiff(names(peer), coordinate_columns), with = FALSE
            ]
            peer_metadata[, SOURCE_JUNCTION_OUTPUT_ID := OUTPUT_ID]
            translation <- colMeans(as.matrix(
                peer[, .(POINT_X, POINT_Y, POINT_Z)]
            )) - colMeans(as.matrix(base[, .(POINT_X, POINT_Y, POINT_Z)]))
        }

        base_names <- if (length(part_ids) == 1L) {
            base$NAME[[1L]]
        } else {
            sprintf("%s [Junction %d]", base$NAME[[1L]], seq_along(part_ids))
        }
        peer_names <- if (paired) {
            if (length(part_ids) == 1L) {
                peer$NAME[[1L]]
            } else {
                sprintf("%s [Junction %d]", peer$NAME[[1L]], seq_along(part_ids))
            }
        } else {
            character()
        }

        for (index in seq_along(part_ids)) {
            geometry <- triangle[PART == part_ids[[index]],
                .(POINT_NO, POINT_X, POINT_Y, POINT_Z)]
            geometry[, POINT_NO := seq_len(.N) - 1L]
            base_part <- data.table::copy(base_metadata)
            base_part[, `:=`(
                NAME = base_names[[index]],
                OUTPUT_ID = sprintf("%s-J%d", base_id, index),
                BOUNDARY_OBJECT = if (paired) {
                    peer_names[[index]]
                } else if (BOUNDARY == "Surface" &&
                    BOUNDARY_OBJECT == base$NAME[[1L]]) {
                    base_names[[index]]
                } else {
                    BOUNDARY_OBJECT
                }
            )]
            if (!is.na(base_part$TYPICAL_PAIR_ID[[1L]])) {
                base_part[, TYPICAL_PAIR_ID := sprintf(
                    "%s-J%d", TYPICAL_PAIR_ID, index
                )]
            }
            output[[length(output) + 1L]] <- cbind(
                base_part[rep(1L, nrow(geometry))], geometry
            )

            if (paired) {
                peer_geometry <- data.table::copy(geometry)
                peer_geometry[, `:=`(
                    POINT_X = POINT_X + translation[[1L]],
                    POINT_Y = POINT_Y + translation[[2L]],
                    POINT_Z = POINT_Z + translation[[3L]]
                )]
                peer_part <- data.table::copy(peer_metadata)
                peer_part[, `:=`(
                    NAME = peer_names[[index]],
                    OUTPUT_ID = sprintf("%s-J%d", peer$OUTPUT_ID[[1L]], index),
                    BOUNDARY_OBJECT = base_names[[index]]
                )]
                if (!is.na(peer_part$TYPICAL_PAIR_ID[[1L]])) {
                    peer_part[, TYPICAL_PAIR_ID := sprintf(
                        "%s-J%d", TYPICAL_PAIR_ID, index
                    )]
                }
                output[[length(output) + 1L]] <- cbind(
                    peer_part[rep(1L, nrow(peer_geometry))], peer_geometry
                )
            }
        }
    }

    surface <- data.table::rbindlist(output, fill = TRUE)
    data.table::setorderv(surface, c("ID", "OUTPUT_ID", "POINT_NO"))
    surface[, PART := data.table::rleid(OUTPUT_ID), by = "ID"]
    surface[, PART_COUNT := data.table::uniqueN(PART), by = "ID"]
    surface
}

# Snap coordinates that EnergyPlus cannot distinguish to one deterministic
# representative before topology is constructed. A spatial hash restricts each
# search to neighboring 0.01 m buckets instead of comparing every point pair.
surface__snap_coordinates <- function(
    surface, profile = eplus_geom__profile()
) {
    tolerance <- profile$coordinate_distance
    surface <- data.table::copy(surface)
    point <- unique(surface[, .(POINT_X, POINT_Y, POINT_Z)])
    data.table::setorderv(point, c("POINT_X", "POINT_Y", "POINT_Z"))
    coordinate <- as.matrix(point)
    representative <- matrix(numeric(), nrow = 0L, ncol = 3L)
    mapping <- integer(nrow(point))
    bucket <- new.env(hash = TRUE, parent = emptyenv())
    offset <- as.matrix(expand.grid(X = -1L:1L, Y = -1L:1L, Z = -1L:1L))

    for (index in seq_len(nrow(point))) {
        cell <- floor(coordinate[index, ] / tolerance)
        candidate <- integer()
        for (offset_index in seq_len(nrow(offset))) {
            key <- paste(cell + offset[offset_index, ], collapse = "|")
            candidate <- c(candidate, bucket[[key]])
        }
        candidate <- unique(candidate)
        if (length(candidate) > 0L) {
            difference <- abs(
                representative[candidate, , drop = FALSE] - coordinate[index, ]
            )
            # Zone edge matching in EnergyPlus compares x, y, and z separately
            # against 0.01 m rather than using Euclidean distance.
            close <- which(apply(difference, 1L, max) < tolerance)
            if (length(close) > 0L) {
                distance <- sqrt(rowSums(difference ^ 2))
                mapping[[index]] <- candidate[close[[which.min(distance[close])]]]
                next
            }
        }

        representative <- rbind(representative, coordinate[index, ])
        mapping[[index]] <- nrow(representative)
        key <- paste(cell, collapse = "|")
        bucket[[key]] <- c(bucket[[key]], mapping[[index]])
    }

    point[, COORDINATE_KEY := sprintf(
        "%.3f|%.3f|%.3f", POINT_X, POINT_Y, POINT_Z
    )]
    point[, `:=`(
        SNAP_X = representative[mapping, 1L],
        SNAP_Y = representative[mapping, 2L],
        SNAP_Z = representative[mapping, 3L]
    )]
    surface[, COORDINATE_KEY := sprintf(
        "%.3f|%.3f|%.3f", POINT_X, POINT_Y, POINT_Z
    )]
    surface[point, on = "COORDINATE_KEY", `:=`(
        POINT_X = i.SNAP_X,
        POINT_Y = i.SNAP_Y,
        POINT_Z = i.SNAP_Z
    )]
    surface[, COORDINATE_KEY := NULL]
    surface
}

# Merge adjacent triangulation parts with a deterministic greedy policy whenever
# deleting their common diagonal leaves one planar convex polygon. Retained
# collinear junctions prevent a merge from reopening a zone edge.
surface__merge_convex_parts <- function(
    surface, profile = eplus_geom__profile(),
    angle_tolerance = profile$angle,
    distance_tolerance = profile$coordinate_distance,
    planarity_tolerance = profile$planarity_distance
) {
    surface <- data.table::copy(surface)
    part_ids <- unique(surface$PART)
    if (length(part_ids) <= 1L) return(surface)

    # Stable coordinate indices remove input-row order from adjacency and tie
    # breaking. The representative row supplies non-coordinate metadata only.
    key <- sprintf(
        "%.12f|%.12f|%.12f",
        surface$POINT_X, surface$POINT_Y, surface$POINT_Z
    )
    unique_key <- sort(unique(key))
    vertex <- match(key, unique_key)
    representative <- match(unique_key, key)
    coordinates <- as.matrix(surface[representative,
        .(POINT_X, POINT_Y, POINT_Z)])
    polygon <- lapply(part_ids, function(part) vertex[surface$PART == part])

    # Canonical cycle starts make output part numbering reproducible without
    # reversing individual parts, because shared edges need opposite directions.
    canonical_cycle <- function(value) {
        first <- which.min(value)
        value[c(seq.int(first, length(value)), seq_len(first - 1L))]
    }
    polygon <- lapply(polygon, canonical_cycle)
    polygon_key <- function(value) paste(sort(value), collapse = ",")
    polygon <- polygon[order(vapply(polygon, polygon_key, character(1L)))]

    merge_boundary <- function(first, second) {
        first_next <- c(first[-1L], first[[1L]])
        second_next <- c(second[-1L], second[[1L]])
        start <- c(first, second)
        end <- c(first_next, second_next)
        edge <- geom__edge_key(start, end)
        repeated <- names(which(table(edge) == 2L))
        if (length(repeated) != 1L) return(NULL)
        keep <- edge != repeated[[1L]]
        start <- start[keep]
        end <- end[keep]

        boundary <- integer()
        current <- start[[1L]]
        origin <- current
        repeat {
            outgoing <- which(start == current)
            if (length(outgoing) != 1L) return(NULL)
            selected <- outgoing[[1L]]
            boundary <- c(boundary, current)
            current <- end[[selected]]
            start <- start[-selected]
            end <- end[-selected]
            if (current == origin) break
            if (length(boundary) > length(first) + length(second)) return(NULL)
        }
        if (length(start) > 0L || length(boundary) < 3L) return(NULL)
        canonical_cycle(boundary)
    }
    merge_metrics <- function(boundary) {
        xyz <- coordinates[boundary, , drop = FALSE]
        frame <- geom__polygon_frame(xyz, profile$normal_magnitude)
        if (!frame$valid || frame$planarity_error > planarity_tolerance) {
            return(NULL)
        }

        following <- c(2:nrow(xyz), 1L)
        previous <- c(nrow(xyz), seq_len(nrow(xyz) - 1L))
        incoming <- xyz - xyz[previous, , drop = FALSE]
        outgoing <- xyz[following, , drop = FALSE] - xyz
        incoming_length <- sqrt(rowSums(incoming ^ 2))
        outgoing_length <- sqrt(rowSums(outgoing ^ 2))
        if (any(incoming_length < distance_tolerance) ||
            any(outgoing_length < distance_tolerance)) return(NULL)

        xy <- frame$xy
        turn <- vapply(seq_len(nrow(xy)), function(index) {
            geom__cross_2d(
                xy[previous[[index]], ], xy[index, ], xy[following[[index]], ]
            )
        }, numeric(1L))
        scale <- incoming_length * outgoing_length
        # A straight-through point may be a required room-shell T-junction and
        # therefore cannot become an independently removable EnergyPlus vertex.
        if (any(abs(turn) <= sin(angle_tolerance) * scale)) return(NULL)
        if (!(all(turn > 0.0) || all(turn < 0.0))) return(NULL)
        list(area = frame$area, key = paste(boundary, collapse = ","))
    }

    repeat {
        if (length(polygon) < 2L) break
        starts <- integer()
        ends <- integer()
        owners <- integer()
        for (index in seq_along(polygon)) {
            value <- polygon[[index]]
            starts <- c(starts, value)
            ends <- c(ends, c(value[-1L], value[[1L]]))
            owners <- c(owners, rep(index, length(value)))
        }
        occurrence <- split(owners, geom__edge_key(starts, ends))
        adjacent <- occurrence[vapply(
            occurrence,
            function(value) {
                length(value) == 2L && value[[1L]] != value[[2L]]
            },
            logical(1L)
        )]
        if (length(adjacent) == 0L) break

        candidate <- list()
        for (edge in names(adjacent)) {
            owner <- adjacent[[edge]]
            first <- min(owner)
            second <- max(owner)
            boundary <- merge_boundary(polygon[[first]], polygon[[second]])
            if (is.null(boundary)) next
            metric <- merge_metrics(boundary)
            if (is.null(metric)) next
            endpoint <- as.integer(strsplit(edge, "/", fixed = TRUE)[[1L]])
            shared_length <- sqrt(sum(
                (coordinates[endpoint[[1L]], ] -
                    coordinates[endpoint[[2L]], ]) ^ 2
            ))
            candidate[[length(candidate) + 1L]] <- list(
                first = first, second = second, boundary = boundary,
                removed = length(polygon[[first]]) +
                    length(polygon[[second]]) - length(boundary),
                shared_length = shared_length, area = metric$area,
                boundary_key = metric$key
            )
        }
        if (length(candidate) == 0L) break

        score <- data.table::rbindlist(lapply(candidate, function(value) {
            data.table::data.table(
                removed = value$removed,
                shared_length = value$shared_length,
                area = value$area,
                boundary_key = value$boundary_key
            )
        }))
        selected <- order(
            -score$removed, -score$shared_length, -score$area,
            score$boundary_key
        )[[1L]]
        value <- candidate[[selected]]
        polygon[[value$first]] <- value$boundary
        polygon[[value$second]] <- NULL
        polygon <- polygon[order(vapply(polygon, polygon_key, character(1L)))]
    }

    data.table::rbindlist(lapply(seq_along(polygon), function(part) {
        value <- data.table::copy(surface[representative[polygon[[part]]]])
        value[, `:=`(PART = part, POINT_NO = seq_len(.N) - 1L)]
        value
    }))
}

# Build one canonical vertex sequence per MAIN_ENCLOSURE middle plane. Planar
# DeST faces remain intact unless a true topology junction or concavity requires
# a part boundary that EnergyPlus can preserve.
surface__normalize_topology <- function(
    surface, window = data.table::data.table(),
    profile = eplus_geom__profile()
) {
    coordinate_columns <- geom__coordinate_columns()
    # Separate per-surface metadata from middle-plane coordinates so a shared
    # polygon is normalized once and then copied consistently to both rooms.
    metadata <- unique(
        surface[, setdiff(names(surface), coordinate_columns), with = FALSE],
        by = "ID"
    )
    # Window parents need a stable unsuffixed part name because every
    # FenestrationSurface:Detailed object references exactly one base surface.
    window_plane <- unique(window$PLANE)
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
        surface__split_edges(.SD, candidates, profile)
    }, by = "PLANE"]
    point[, COORDINATE_KEY := sprintf(
        "%.3f|%.3f|%.3f", POINT_X, POINT_Y, POINT_Z
    )]
    # A coordinate appearing on more than one plane is a zone-topology
    # junction. Removing it as merely collinear would reopen a shared edge.
    point[, PROTECTED := data.table::uniqueN(PLANE) > 1L, by = "COORDINATE_KEY"]
    # Private collinear points can be removed safely before surface copies are
    # oriented, which also prevents peer vertex-count differences in EnergyPlus.
    point <- point[, {
        value <- surface__simplify_polygon(.SD, profile)
        if (nrow(value) < 3L) {
            warning(sprintf(
                paste(
                    "Dropped DeST middle plane %s because its polygon collapses",
                    "within EnergyPlus's 0.01 m vertex tolerance."
                ),
                .BY$PLANE
            ), call. = FALSE)
        }
        value
    }, by = "PLANE"]
    point <- point[, {
        # EnergyPlus does not rewrite the IDF, but its GetSurfaceData path copies
        # input vertices into an in-memory SurfaceTmp and CheckConvexity removes
        # collinear vertices from that working copy. With reversed peer winding,
        # The reference EnergyPlus profile removed different counts from some
        # complex peer faces,
        # causing a vertex-size-mismatch fatal error. Encode each protected
        # junction as a true part boundary before export: selectively partition
        # window hosts, and triangulate other polygons with identical part IDs
        # on both sides of an interzone construction.
        split <- any(surface__redundant_vertices(.SD, profile) & PROTECTED)
        concave <- !geom__polygon_is_convex(.SD, profile$angle)
        avoid_points <- window[PLANE == .BY$PLANE]
        if (split) {
            # A protected straight-through junction must become an actual edge.
            # Triangles also prevent EnergyPlus from independently flattening a
            # slightly non-planar remainder and deleting different peer points.
            # This path is limited to affected planes; ordinary DeST faces keep
            # their original polygon, while windows are clipped to these parts.
            surface__triangulate_polygon(.SD, avoid_points, profile)
        } else if (concave) {
            # Concave heat-transfer surfaces are legal, but EnergyPlus cannot
            # reliably use them as shadow receivers or casters. This condition
            # independently justifies complete triangulation.
            surface__triangulate_polygon(.SD, avoid_points, profile)
        } else {
            copy <- data.table::copy(.SD)
            copy[, `:=`(PART = 1L, POINT_NO = seq_len(.N) - 1L)]
            copy
        }
    }, by = "PLANE"]
    # Delete only triangulation diagonals whose removal leaves a planar convex
    # face; both room-side copies inherit the same deterministic partition.
    point <- point[, surface__merge_convex_parts(.SD, profile), by = "PLANE"]
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
surface__split_edges <- function(
    surface, all_coordinates, profile = eplus_geom__profile()
) {
    tolerance <- profile$plane_distance
    distance_tolerance <- profile$coordinate_distance
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
            # Global coordinate snapping can intentionally collapse a DeST
            # sliver. Keep one endpoint here; polygon simplification below
            # removes the duplicate consistently from every incident plane.
            output[[index]] <- data.table::data.table(
                POINT_X = start[[1L]],
                POINT_Y = start[[2L]],
                POINT_Z = start[[3L]]
            )
            next
        }

        # For edge start A, direction d, and candidate P, the scalar projection
        # t = ((P - A) dot d) / (d dot d) locates P along the infinite edge line.
        # A point is strictly inside the segment when 0 < t < 1.
        relative <- sweep(candidates, 2L, start, "-")
        position <- as.vector(relative %*% difference / length_squared)
        # The residual (P - A) - t*d is the component perpendicular to the
        # edge. Its norm must be within tolerance for P to be collinear.
        projected <- relative - position * rep(difference, each = nrow(relative))
        edge_length <- sqrt(length_squared)
        # EnergyPlus treats vertices less than 0.01 m apart as coincident. Do
        # not insert a junction that EnergyPlus would immediately collapse back
        # into either endpoint.
        on_segment <- position * edge_length >= distance_tolerance &
            (1.0 - position) * edge_length >= distance_tolerance &
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

# Mark redundant vertices inside one ordered polygon; this function does not
# decide whether two complete polygons are duplicates. A vertex is redundant
# when it coincides with either neighbor, or when the incoming and outgoing
# edges continue in the same straight direction. Keeping this calculation
# separate ensures topology splitting and simplification use the same tolerance.
surface__redundant_vertices <- function(
    surface, profile = eplus_geom__profile()
) {
    tolerance <- profile$angle
    distance_tolerance <- profile$coordinate_distance
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
    # or next coordinate. EnergyPlus uses a 1e-6 radian turn threshold, so the
    # normalized cross product applies the same angular collinearity test here;
    # the positive dot product deliberately retains a backtracking edge.
    incoming_length < distance_tolerance |
        outgoing_length < distance_tolerance |
        (
            sqrt(rowSums(cross_product ^ 2)) <=
                sin(tolerance) * incoming_length * outgoing_length &
            rowSums(incoming * outgoing) > 0
        )
}

# Triangulate a simple planar polygon with ear clipping while retaining every
# protected boundary junction. It is called only for planes that still contain
# required collinear junctions after simplification, not for every surface.
# Ordinary ear clipping is cubic; the window-clearance search may explore more
# states, so it is capped by the versioned geometry profile before falling back
# to deterministic ordinary clipping. Peer faces reuse identical part numbering.
surface__triangulate_polygon <- function(
    surface, avoid_points = data.table::data.table(),
    profile = eplus_geom__profile()
) {
    tolerance <- profile$intersection
    distance_tolerance <- profile$coordinate_distance
    surface <- data.table::copy(surface)
    frame <- geom__polygon_frame(surface, profile$normal_magnitude)
    if (!frame$valid) geom__unit_normal(surface, profile$normal_magnitude)
    normal <- frame$normal
    projection <- frame$projection
    xy <- frame$xy
    if (frame$signed_area < 0.0) {
        # Normalize to counterclockwise order so positive cross products are
        # convex turns throughout the ear-clipping loop.
        surface <- surface[nrow(surface):1L]
        xy <- xy[nrow(xy):1L, , drop = FALSE]
    }

    inside_triangle <- function(point, a, b, c) {
        geom__cross_2d(a, b, point) >= -tolerance &&
            geom__cross_2d(b, c, point) >= -tolerance &&
            geom__cross_2d(c, a, point) >= -tolerance
    }

    # Score a candidate ear by its new diagonal's clearance from every window
    # corner on this host plane. Choosing the largest clearance prevents the
    # later window clipping step from creating sub-centimetre triangular slivers
    # that EnergyPlus necessarily collapses as degenerate surfaces.
    avoid_xy <- matrix(numeric(), nrow = 0L, ncol = 2L)
    if (nrow(avoid_points) > 0L) {
        avoid_xy <- as.matrix(
            avoid_points[, .(POINT_X, POINT_Y, POINT_Z)]
        )[, projection, drop = FALSE]
    }
    diagonal_clearance <- function(start, end) {
        if (nrow(avoid_xy) == 0L) return(Inf)
        direction <- end - start
        length_squared <- sum(direction ^ 2)
        relative <- sweep(avoid_xy, 2L, start, "-")
        position <- as.vector(relative %*% direction / length_squared)
        projected <- relative - position * rep(direction, each = nrow(relative))
        interior <- position > tolerance & position < 1.0 - tolerance
        if (!any(interior)) return(Inf)
        min(sqrt(rowSums(projected[interior, , drop = FALSE] ^ 2)))
    }

    # A convex host can be partitioned through one interior Steiner point. Try
    # deterministic interior candidates and retain a fan only when every radial
    # edge either clears all window corners by 1 cm or passes through a corner
    # exactly. This removes unavoidable near-corner diagonals in ordinary
    # boundary-only triangulations without changing the aggregate host area.
    if (nrow(avoid_xy) > 0L &&
        geom__polygon_is_convex(surface, profile$angle)) {
        span <- apply(xy, 2L, range)
        grid <- expand.grid(
            X = seq(span[1L, 1L], span[2L, 1L], length.out = 11L),
            Y = seq(span[1L, 2L], span[2L, 2L], length.out = 11L)
        )
        # Window corners are valid Steiner centers. Starting all host radials
        # at an exact corner avoids a near-corner cut when no ordinary grid or
        # centroid candidate can maintain EnergyPlus's 1 cm vertex clearance.
        candidate_center <- unique(rbind(
            colMeans(xy), colMeans(avoid_xy), avoid_xy, as.matrix(grid)
        ))
        host_next <- seq_len(nrow(xy)) %% nrow(xy) + 1L
        inside <- apply(candidate_center, 1L, function(point) {
            all(vapply(seq_len(nrow(xy)), function(index) {
                geom__cross_2d(
                    xy[index, ], xy[host_next[[index]], ], point
                ) > tolerance
            }, logical(1L)))
        })
        candidate_center <- candidate_center[inside, , drop = FALSE]
        if (nrow(candidate_center) > 0L) {
            clearance <- apply(candidate_center, 1L, function(center) {
                radial_length <- sqrt(rowSums(sweep(xy, 2L, center, "-") ^ 2))
                if (any(radial_length < distance_tolerance)) return(-Inf)
                radial <- vapply(seq_len(nrow(xy)), function(index) {
                    diagonal_clearance(center, xy[index, ])
                }, numeric(1L))
                radial[radial <= tolerance] <- Inf
                min(radial)
            })
            clear <- which(clearance >= distance_tolerance)
            if (length(clear) > 0L) {
                center_xy <- candidate_center[clear[[which.max(clearance[clear])]], ]
                center_xyz <- numeric(3L)
                center_xyz[projection] <- center_xy
                omitted <- setdiff(1:3, projection)
                origin <- as.numeric(surface[1L, .(POINT_X, POINT_Y, POINT_Z)])
                center_xyz[omitted] <- origin[omitted] - sum(
                    normal[projection] * (center_xyz[projection] - origin[projection])
                ) / normal[omitted]
                return(data.table::rbindlist(lapply(seq_len(nrow(surface)), function(part) {
                    following <- part %% nrow(surface) + 1L
                    value <- data.table::copy(surface[c(part, part, following)])
                    value[1L, `:=`(
                        POINT_X = center_xyz[[1L]],
                        POINT_Y = center_xyz[[2L]],
                        POINT_Z = center_xyz[[3L]]
                    )]
                    value[, `:=`(PART = part, POINT_NO = 0:2)]
                    value
                })))
            }
        }
    }

    # Enumerate valid ears separately so a bounded backtracking search can avoid
    # a locally attractive diagonal that forces a later cut next to a window
    # corner. Failed polygon states are memoized by their remaining vertices.
    valid_ears <- function(remaining) {
        candidate <- list()
        for (position in seq_along(remaining)) {
            previous <- remaining[(position - 2L) %% length(remaining) + 1L]
            current <- remaining[[position]]
            following <- remaining[position %% length(remaining) + 1L]
            # A non-positive turn is concave or collinear and cannot be an ear.
            if (geom__cross_2d(
                xy[previous, ], xy[current, ], xy[following, ]
            ) <= tolerance) {
                next
            }
            candidate_xy <- xy[c(previous, current, following), , drop = FALSE]
            triangle_next <- c(2L, 3L, 1L)
            if (any(sqrt(rowSums(
                (candidate_xy - candidate_xy[triangle_next, , drop = FALSE]) ^ 2
            )) < distance_tolerance)) next
            other <- setdiff(remaining, c(previous, current, following))
            contains <- vapply(other, function(index) {
                inside_triangle(
                    xy[index, ], xy[previous, ], xy[current, ], xy[following, ]
                )
            }, logical(1L))
            # The diagonal from previous to following stays inside a simple
            # polygon only when no remaining vertex lies in the ear triangle.
            if (any(contains)) next

            candidate[[length(candidate) + 1L]] <- list(
                position = position,
                vertex = c(previous, current, following),
                clearance = diagonal_clearance(xy[previous, ], xy[following, ])
            )
        }
        candidate
    }
    failed_state <- new.env(hash = TRUE, parent = emptyenv())
    searched_states <- 0L
    find_clear_triangulation <- function(remaining) {
        if (length(remaining) == 3L) return(list(remaining))
        state <- paste(remaining, collapse = ",")
        if (isTRUE(failed_state[[state]])) return(NULL)
        searched_states <<- searched_states + 1L
        if (searched_states > profile$triangulation_max_states) {
            return(NULL)
        }

        candidate <- valid_ears(remaining)
        if (length(candidate) > 0L) {
            clearance <- vapply(candidate, `[[`, numeric(1L), "clearance")
            # A diagonal may either stay at least 1 cm from a window corner or
            # pass through it exactly. The exact case partitions the opening
            # without creating a finite-area sliver; the later boundary inset
            # handles EnergyPlus's zero-distance containment convention.
            safe <- which(
                clearance >= distance_tolerance | clearance <= tolerance
            )
            if (length(safe) > 0L) {
                safe <- safe[order(clearance[safe], decreasing = TRUE)]
                for (index in safe) {
                    selected <- candidate[[index]]
                    rest <- find_clear_triangulation(
                        remaining[-selected$position]
                    )
                    if (!is.null(rest)) {
                        return(c(list(selected$vertex), rest))
                    }
                }
            }
        }
        failed_state[[state]] <- TRUE
        NULL
    }

    remaining <- seq_len(nrow(surface))
    triangle <- find_clear_triangulation(remaining)
    if (is.null(triangle)) {
        # A pathological layout or the explicit state cap can make a fully clear
        # search unavailable. Deterministic ordinary clipping avoids unbounded
        # conversion time; downstream area checks still report geometry loss.
        triangle <- list()
        while (length(remaining) > 3L) {
            candidate <- valid_ears(remaining)
            if (length(candidate) == 0L) {
                stop("Could not triangulate a DeST surface polygon.")
            }
            clearance <- vapply(candidate, `[[`, numeric(1L), "clearance")
            selected <- candidate[[which.max(clearance)]]
            triangle[[length(triangle) + 1L]] <- selected$vertex
            remaining <- remaining[-selected$position]
        }
        triangle[[length(triangle) + 1L]] <- remaining
    }

    final_xy <- xy[triangle[[length(triangle)]], , drop = FALSE]
    if (any(sqrt(rowSums((
        final_xy - final_xy[c(2L, 3L, 1L), , drop = FALSE]
    ) ^ 2)) < distance_tolerance)) {
        stop("Could not triangulate a DeST surface without a sub-centimetre edge.")
    }

    data.table::rbindlist(lapply(seq_along(triangle), function(part) {
        value <- data.table::copy(surface[triangle[[part]]])
        value[, `:=`(PART = part, POINT_NO = 0:2)]
        value
    }))
}

# Remove redundant vertices from one ordered DeST surface polygon while
# retaining turns and at least the three vertices needed for a valid face.
surface__simplify_polygon <- function(
    surface, profile = eplus_geom__profile()
) {
    distance_tolerance <- profile$coordinate_distance
    surface <- data.table::copy(surface)

    repeat {
        n_vertex <- nrow(surface)
        coordinates <- as.matrix(surface[, .(POINT_X, POINT_Y, POINT_Z)])
        following <- seq_len(n_vertex) %% n_vertex + 1L
        short_edge <- which(sqrt(rowSums(
            (coordinates - coordinates[following, , drop = FALSE]) ^ 2
        )) < distance_tolerance)
        if (n_vertex <= 3L) {
            # A triangle with a sub-centimetre edge becomes a line after
            # EnergyPlus merges the two endpoints. The source face is already
            # below EnergyPlus's representable resolution, so omit the sliver.
            if (length(short_edge) > 0L) return(surface[0L])
            break
        }
        if (length(short_edge) > 0L) {
            # EnergyPlus collapses sub-centimetre edges even when their endpoint
            # is a topology junction. Remove exactly one endpoint per iteration
            # before part creation so no exported triangle can become degenerate.
            remove <- following[short_edge[[1L]]]
            surface <- surface[-remove]
            next
        }

        redundant <- surface__redundant_vertices(surface, profile)
        if ("PROTECTED" %in% names(surface)) {
            redundant <- redundant & !surface$PROTECTED
        }

        if (!any(redundant) || n_vertex - sum(redundant) < 3L) break
        surface <- surface[!redundant]
    }

    if (nrow(surface) >= 3L) {
        # Snapping can collapse a very narrow source polygon into one line even
        # when its remaining edges are long. Omit the zero-area face before any
        # normal or convexity calculation tries to interpret it.
        if (sqrt(sum(geom__newell_vector(surface) ^ 2)) <=
            profile$normal_magnitude) return(surface[0L])
    }

    data.table::set(surface, NULL, "POINT_NO", seq_len(nrow(surface)) - 1L)
    surface
}
