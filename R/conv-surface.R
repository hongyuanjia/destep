# Read the two room-side copies of every DeST enclosure on their shared middle
# plane. Nearby but topologically unrelated vertices keep their source coordinates.
surface__source_table <- function(dest, geometry_profile) {
    surface__validate_source_references(dest)
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
            -- DeST exterior-wall, roof, and exposed-floor libraries are already
            -- stored from outdoors to indoors. Preserve that order only on a
            -- true outdoor boundary; the same construction kinds can also occur
            -- between rooms, where the reciprocal SIDE1 reversal is still
            -- required. Inner-wall layers ascend from SIDE1 to SIDE2, so
            -- EnergyPlus's outside-to-inside convention likewise requires
            -- SIDE1 to use the reverse stack and SIDE2 to retain the source
            -- stack.
            CASE
                WHEN E.KIND IN (1, 3, 6) AND PEER.TYPE = 1
                    THEN E.CONSTRUCTION
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
            -- The current DeST SURFACE row is the room-side face. Its peer
            -- supplies exterior properties only when that peer is outdoors.
            S.ABSORB_COEF                                  AS INSIDE_SOLAR_ABSORPTANCE,
            S.BLACKNESS                                    AS INSIDE_THERMAL_ABSORPTANCE,
            S.VENTILATION_COEF                             AS INSIDE_CONVECTION_COEFFICIENT,
            PEER.ABSORB_COEF                               AS OUTSIDE_SOLAR_ABSORPTANCE,
            PEER.BLACKNESS                                 AS OUTSIDE_THERMAL_ABSORPTANCE,
            PEER.VENTILATION_COEF                          AS OUTSIDE_CONVECTION_COEFFICIENT,
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
    surface__canonical_source_vertices(surface, geometry_profile)
}

# Read both window and door outlines for host clipping and local-cut protection.
# Prefix source IDs by table so independently numbered doors/windows stay distinct.
surface__opening_table <- function(dest) {
    tables <- c("WINDOW", "DOOR")
    rows <- lapply(tables, function(table) {
        if (!db_has_rows(dest, table)) {
            return(NULL)
        }
        data.table::as.data.table(DBI::dbGetQuery(
            dest,
            sprintf(
                "
        SELECT
            '%s/' || W.ID AS OPENING_ID,
            E.MIDDLE_PLANE AS PLANE,
            L.POINT_NO,
            ROUND(P.X, 3) AS POINT_X,
            ROUND(P.Y, 3) AS POINT_Y,
            ROUND(P.Z, 3) AS POINT_Z
        FROM %s W
        INNER JOIN MAIN_ENCLOSURE E ON W.OF_ENCLOSURE = E.ID
        INNER JOIN PLANE PL ON W.MIDDLE_PLANE = PL.PLANE_ID
        INNER JOIN GEOMETRY G ON PL.GEOMETRY = G.GEOMETRY_ID
        INNER JOIN LOOP_POINT L ON G.BOUNDARY_LOOP_ID = L.LOOP_ID
        INNER JOIN POINT P ON L.POINT = P.POINT_ID
        ORDER BY W.ID, L.POINT_NO
    ",
                table,
                table
            )
        ))
    })
    data.table::rbindlist(rows)
}

# Convert normalized surface rows into the ordered field lists expected by
# BuildingSurface:Detailed, accounting for the pre-Space IDD schema.
surface__object_values <- function(surface, ep) {
    value <- surface[,
        by = "OUTPUT_ID",
        list(
            value = list(c(
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
                    outside_boundary_condition_object = if (
                        !is.na(BOUNDARY_OBJECT[[1L]])
                    ) {
                        BOUNDARY_OBJECT[[1L]]
                    },
                    # 08: Sun Exposure
                    sun_exposure = if (BOUNDARY[[1L]] == "Outdoors") {
                        "SunExposed"
                    } else {
                        "NoSun"
                    },
                    # 09: Wind Exposure
                    wind_exposure = if (BOUNDARY[[1L]] == "Outdoors") {
                        "WindExposed"
                    } else {
                        "NoWind"
                    },
                    # 10: View Factor to Ground
                    view_factor_to_ground = "Autocalculate",
                    # 11: Number of Vertices
                    number_of_vertices = max(POINT_NO) + 1L
                ),
                # Vertices
                geom__eplus_vertex_values(.SD)
            ))
        )
    ]$value

    # EnergyPlus 9.5 and earlier do not expose the Space Name field.
    if (ep$version() <= "9.5") {
        ind <- which(names(value[[1L]]) == "space_name")
        if (length(ind) > 0L) value <- lapply(value, .subset, -ind)
    }

    value
}

# Validate one coefficient per exported surface and reduce duplicated vertex
# rows to the object-level property records used by the following converters.
surface_property__object_table <- function(surface) {
    fields <- c(
        "OUTPUT_ID",
        "NAME",
        "KIND_ENCLOSURE",
        "CONSTRUCTION",
        "BOUNDARY",
        "BOUNDARY_OBJECT",
        "INSIDE_SOLAR_ABSORPTANCE",
        "INSIDE_THERMAL_ABSORPTANCE",
        "INSIDE_CONVECTION_COEFFICIENT",
        "OUTSIDE_SOLAR_ABSORPTANCE",
        "OUTSIDE_THERMAL_ABSORPTANCE",
        "OUTSIDE_CONVECTION_COEFFICIENT"
    )
    missing <- setdiff(fields, names(surface))
    if (length(missing) > 0L) {
        stop(
            sprintf(
                "Surface property fields are missing: %s.",
                paste(missing, collapse = ", ")
            ),
            call. = FALSE
        )
    }

    object <- unique(surface[, fields, with = FALSE])
    duplicate <- object[, .N, by = "OUTPUT_ID"][N != 1L, OUTPUT_ID]
    if (length(duplicate) > 0L) {
        stop(
            sprintf(
                "Exported DeST surface(s) have inconsistent properties: %s.",
                paste(utils::head(duplicate, 10L), collapse = ", ")
            ),
            call. = FALSE
        )
    }

    numeric_fields <- setdiff(
        fields,
        c(
            "OUTPUT_ID",
            "NAME",
            "KIND_ENCLOSURE",
            "CONSTRUCTION",
            "BOUNDARY",
            "BOUNDARY_OBJECT"
        )
    )
    dt_force_numeric(object, numeric_fields)
    object
}

# Give each distinct exposed-face absorptance tuple a deterministic construction
# name. The matching material and Construction clones are created later by the
# construction converter, after all source layers have been resolved.
surface_property__assign_constructions <- function(surface) {
    surface <- data.table::copy(surface)
    object <- surface_property__object_table(surface)

    outside_fields <- c(
        "OUTSIDE_SOLAR_ABSORPTANCE",
        "OUTSIDE_THERMAL_ABSORPTANCE"
    )
    # Zero on an exposed-floor outside face is a literal source coefficient.
    # Native ground-reflection checks distinguish alpha=0 from alpha=0.6;
    # borrowing a wall's properties would introduce an absent solar gain.
    # Apply only the EnergyPlus thermal-absorptance limit below.
    invalid_inside <- object[
        !is.finite(INSIDE_SOLAR_ABSORPTANCE) |
            INSIDE_SOLAR_ABSORPTANCE < 0.0 |
            INSIDE_SOLAR_ABSORPTANCE > 1.0 |
            !is.finite(INSIDE_THERMAL_ABSORPTANCE) |
            INSIDE_THERMAL_ABSORPTANCE < 0.0 |
            INSIDE_THERMAL_ABSORPTANCE > 1.0,
        OUTPUT_ID
    ]
    invalid_outside <- object[
        BOUNDARY == "Outdoors" &
            (!is.finite(OUTSIDE_SOLAR_ABSORPTANCE) |
                OUTSIDE_SOLAR_ABSORPTANCE < 0.0 |
                OUTSIDE_SOLAR_ABSORPTANCE > 1.0 |
                !is.finite(OUTSIDE_THERMAL_ABSORPTANCE) |
                OUTSIDE_THERMAL_ABSORPTANCE < 0.0 |
                OUTSIDE_THERMAL_ABSORPTANCE > 1.0),
        OUTPUT_ID
    ]
    invalid <- unique(c(invalid_inside, invalid_outside))
    if (length(invalid) > 0L) {
        stop(
            sprintf(
                "Invalid DeST surface absorptance or blackness for surface(s): %s.",
                paste(utils::head(invalid, 10L), collapse = ", ")
            ),
            call. = FALSE
        )
    }

    # EnergyPlus requires thermal absorptance to be greater than zero and no
    # greater than 0.99999. Preserve DeST's limiting values using the nearest
    # representable inputs used by every supported EnergyPlus version.
    thermal_fields <- c(
        "INSIDE_THERMAL_ABSORPTANCE",
        "OUTSIDE_THERMAL_ABSORPTANCE"
    )
    for (field in thermal_fields) {
        data.table::set(
            object,
            which(is.finite(object[[field]])),
            field,
            pmin(
                pmax(object[[field]][is.finite(object[[field]])], 1e-6),
                0.99999
            )
        )
    }

    # An interzone construction must remain the exact reverse of its peer.
    # Populate its outside material face from the peer's room-side properties;
    # ground is the only boundary without a radiatively exposed second face.
    peer_index <- match(object$BOUNDARY_OBJECT, object$NAME)
    interzone <- object$BOUNDARY == "Surface" &
        object$NAME != object$BOUNDARY_OBJECT
    if (any(interzone & is.na(peer_index))) {
        stop(
            "Could not resolve an interzone surface property peer.",
            call. = FALSE
        )
    }
    object[
        interzone,
        `:=`(
            OUTSIDE_SOLAR_ABSORPTANCE = object$INSIDE_SOLAR_ABSORPTANCE[peer_index[
                interzone
            ]],
            OUTSIDE_THERMAL_ABSORPTANCE = object$INSIDE_THERMAL_ABSORPTANCE[peer_index[
                interzone
            ]]
        )
    ]
    self_reference <- object$BOUNDARY == "Surface" &
        object$NAME == object$BOUNDARY_OBJECT
    object[
        self_reference,
        `:=`(
            OUTSIDE_SOLAR_ABSORPTANCE = INSIDE_SOLAR_ABSORPTANCE,
            OUTSIDE_THERMAL_ABSORPTANCE = INSIDE_THERMAL_ABSORPTANCE
        )
    ]
    object[BOUNDARY == "Ground", (outside_fields) := NA_real_]
    has_outside <- is.finite(object$OUTSIDE_SOLAR_ABSORPTANCE) &
        is.finite(object$OUTSIDE_THERMAL_ABSORPTANCE)
    object[, BASE_CONSTRUCTION := CONSTRUCTION]
    object[,
        CONSTRUCTION := sprintf(
            "%s [DeST i-a%.15g-e%.15g%s]",
            BASE_CONSTRUCTION,
            INSIDE_SOLAR_ABSORPTANCE,
            INSIDE_THERMAL_ABSORPTANCE,
            ifelse(
                has_outside,
                sprintf(
                    " o-a%.15g-e%.15g",
                    OUTSIDE_SOLAR_ABSORPTANCE,
                    OUTSIDE_THERMAL_ABSORPTANCE
                ),
                ""
            )
        )
    ]

    # Reuse the original construction only when DeST requests the exact
    # EnergyPlus defaults on every exposed face.
    object[
        INSIDE_SOLAR_ABSORPTANCE == 0.7 &
            INSIDE_THERMAL_ABSORPTANCE == 0.9 &
            (!has_outside |
                (OUTSIDE_SOLAR_ABSORPTANCE == 0.7 &
                    OUTSIDE_THERMAL_ABSORPTANCE == 0.9)),
        CONSTRUCTION := BASE_CONSTRUCTION
    ]
    surface[
        object,
        on = "OUTPUT_ID",
        `:=`(
            BASE_CONSTRUCTION = i.BASE_CONSTRUCTION,
            CONSTRUCTION = i.CONSTRUCTION,
            INSIDE_SOLAR_ABSORPTANCE = i.INSIDE_SOLAR_ABSORPTANCE,
            INSIDE_THERMAL_ABSORPTANCE = i.INSIDE_THERMAL_ABSORPTANCE,
            OUTSIDE_SOLAR_ABSORPTANCE = i.OUTSIDE_SOLAR_ABSORPTANCE,
            OUTSIDE_THERMAL_ABSORPTANCE = i.OUTSIDE_THERMAL_ABSORPTANCE
        )
    ]
    surface
}

# Convert DeST's fixed room-side and outdoor-side film coefficients to one
# SurfaceProperty object per exported opaque surface.
surface_property__convection_values <- function(surface) {
    object <- surface_property__object_table(surface)
    invalid_inside <- object[
        !is.finite(INSIDE_CONVECTION_COEFFICIENT) |
            INSIDE_CONVECTION_COEFFICIENT < 0.1,
        OUTPUT_ID
    ]
    invalid_outside <- object[
        BOUNDARY == "Outdoors" &
            (!is.finite(OUTSIDE_CONVECTION_COEFFICIENT) |
                OUTSIDE_CONVECTION_COEFFICIENT < 0.1),
        OUTPUT_ID
    ]
    invalid <- unique(c(invalid_inside, invalid_outside))
    if (length(invalid) > 0L) {
        stop(
            sprintf(
                "Invalid DeST convection coefficient for surface(s): %s.",
                paste(utils::head(invalid, 10L), collapse = ", ")
            ),
            call. = FALSE
        )
    }

    lapply(seq_len(nrow(object)), function(index) {
        row <- object[index]
        value <- list(
            surface_name = row$NAME,
            convection_coefficient_1_location = "Inside",
            convection_coefficient_1_type = "Value",
            convection_coefficient_1 = row$INSIDE_CONVECTION_COEFFICIENT
        )
        if (row$BOUNDARY == "Outdoors") {
            value <- c(
                value,
                list(
                    convection_coefficient_2_location = "Outside",
                    convection_coefficient_2_type = "Value",
                    convection_coefficient_2 = row$OUTSIDE_CONVECTION_COEFFICIENT
                )
            )
        }
        value
    })
}

# SURFACE|MAIN_ENCLOSURE|PLANE -> BuildingSurface:Detailed
surface__convert <- function(
    dest,
    ep,
    geometry_profile = eplus_geom__profile(ep$version()),
    surface_convection = "dest"
) {
    surface_convection <- match.arg(surface_convection, c("dest", "energyplus"))
    surface <- surface__source_table(dest, geometry_profile)
    window <- surface__opening_table(dest)

    # Normalize shared middle planes once, preferring complete source polygons.
    # Repair only junctions that fail closure when
    # removable vertices are cleaned as part of target geometry processing.
    surface <- surface__normalize_topology(
        surface,
        window,
        geometry_profile
    )
    # remove the surface indicating outside environment and grounds
    surface <- surface[!J(c(1L, 2L)), on = "TYPE_SURFACE"]
    surface <- surface__preserve_boundaries(
        surface,
        window,
        geometry_profile
    )
    # Construction names depend on the final boundary role because only an
    # outdoor face has an exposed exterior absorptance.
    surface <- surface_property__assign_constructions(surface)

    # Orient every polygon from DeST's source azimuth and tilt. This also makes
    # exposed floors face downward without relying on enclosure side numbers.
    south_direction <- geom__south_direction(dest)
    surface <- surface[,
        geom__orient_surface_polygon(.SD, south_direction, geometry_profile),
        by = "OUTPUT_ID"
    ]

    # TODO: how does DeST handle the case when the surface is both a floor and a ceiling?
    # TODO: how does EnergyPlus handle "empty floor slab"?

    value <- surface__object_values(surface, ep)
    building_surface <- conv__add_objects(
        dest,
        ep,
        "BuildingSurface:Detailed",
        value
    )
    # Omitting these overrides selects EnergyPlus's own surface algorithms;
    # source geometry and construction properties remain the same.
    convection <- if (surface_convection == "dest") {
        conv__add_objects(
            dest,
            ep,
            "SurfaceProperty:ConvectionCoefficients",
            surface_property__convection_values(surface)
        )
    }
    out <- conv__combine_outputs(
        list(surface = building_surface, convection = convection),
        table = surface
    )

    # always attach the table to the output in case it is useful later
    attr(out, "table") <- surface
    pairs <- surface__multiplier_pairs(surface)
    attr(out, "boundary_diagnostics") <- pairs
    if (nrow(pairs) > 0L) {
        warning(
            sprintf(
                paste(
                    "Preserved %d source interzone surface pair(s) joining unequal",
                    "storey multipliers. Representative-zone boundaries are unchanged;",
                    "multiplier-weighted whole-building heat transfer cannot be assumed",
                    "physically balanced. See attr(idf, 'conversion')$surface_boundaries."
                ),
                nrow(pairs)
            ),
            call. = FALSE
        )
    }

    out
}

# Check room closure on a cleaned geometry copy using the compatibility profile.
# EnergyPlus removes coincident/collinear vertices before volume calculation;
# checking the uncleaned shell can miss a break exposed by that preprocessing.
# Its second closure pass accepts harmless T-junctions without exported splits.
# This profile-based check is supplemented by actual engine regression runs.
surface__energyplus_unclosed_rooms <- function(
    surface,
    profile = eplus_geom__profile(),
    details = FALSE,
    cleanup_cache = NULL
) {
    vertex_tolerance <- profile$closure_vertex_distance
    # Private copies keep source junction vertices available to the later repair.
    # A face that collapses during cleanup must make its room fail this check,
    # rather than disappearing from the room's inventory without a diagnostic.
    room_is_closed <- function(room) {
        output_ids <- unique(room$OUTPUT_ID)
        faces <- lapply(output_ids, function(output_id) {
            face <- room[OUTPUT_ID == output_id, .(POINT_X, POINT_Y, POINT_Z)]
            key <- paste(as.vector(t(as.matrix(face))), collapse = "/")
            if (!is.null(cleanup_cache) && !is.null(cleanup_cache[[key]])) {
                return(cleanup_cache[[key]])
            }
            cleaned <- as.matrix(surface__simplify_polygon(face, profile)[,
                .(POINT_X, POINT_Y, POINT_Z)
            ])
            if (!is.null(cleanup_cache)) {
                cleanup_cache[[key]] <- cleaned
            }
            cleaned
        })
        if (length(faces) == 0L || any(vapply(faces, nrow, integer(1L)) < 3L)) {
            return(list(
                output_ids = output_ids,
                points = do.call(rbind, faces),
                score = Inf,
                defects = Inf,
                first_defects = Inf
            ))
        }

        # EnergyPlus keeps the first coordinate in traversal order as the
        # representative of every coordinate-wise 1 cm vertex cluster.
        unique_vertices <- matrix(numeric(), nrow = 0L, ncol = 3L)
        vertex_cache <- new.env(hash = TRUE, parent = emptyenv())
        vertex_index <- function(point) {
            key <- paste(point, collapse = "/")
            if (!is.null(vertex_cache[[key]])) {
                return(vertex_cache[[key]])
            }
            if (nrow(unique_vertices) > 0L) {
                close <- which(
                    rowSums(
                        abs(sweep(unique_vertices, 2L, point, "-")) >=
                            vertex_tolerance
                    ) ==
                        0L
                )
                if (length(close) > 0L) {
                    vertex_cache[[key]] <- close[[1L]]
                    return(close[[1L]])
                }
            }
            unique_vertices <<- rbind(unique_vertices, point)
            vertex_cache[[key]] <- nrow(unique_vertices)
            vertex_cache[[key]]
        }
        face_indices <- lapply(faces, function(face) {
            apply(face, 1L, vertex_index)
        })
        count_edges <- function(indices) {
            edge <- unlist(
                lapply(indices, function(index) {
                    following <- c(index[-1L], index[[1L]])
                    geom__edge_key(index, following)
                }),
                use.names = FALSE
            )
            table(edge)
        }

        first_count <- count_edges(face_indices)
        if (length(first_count) > 0L && all(first_count == 2L)) {
            return(list(
                output_ids = character(),
                points = matrix(numeric(), 0L, 3L),
                score = 0,
                defects = 0,
                first_defects = 0
            ))
        }

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
                edge_length <- sqrt(sum(direction^2))
                if (edge_length <= profile$zero_distance) {
                    next
                }

                start_distance <- abs(sweep(
                    unique_vertices,
                    2L,
                    start,
                    "-"
                ))
                end_distance <- abs(sweep(
                    unique_vertices,
                    2L,
                    end,
                    "-"
                ))
                not_endpoint <- rowSums(start_distance >= vertex_tolerance) >
                    0L &
                    rowSums(end_distance >= vertex_tolerance) > 0L
                relative <- sweep(unique_vertices, 2L, start, "-")
                unit <- direction / edge_length
                perpendicular <- relative -
                    outer(
                        as.vector(relative %*% unit),
                        unit
                    )
                line_distance <- sqrt(rowSums(perpendicular^2))
                between_error <- abs(
                    edge_length -
                        sqrt(rowSums(relative^2)) -
                        sqrt(rowSums(end_distance^2))
                )
                candidate <- which(
                    not_endpoint &
                        line_distance < vertex_tolerance &
                        between_error < vertex_tolerance
                )
                if (length(candidate) > 0L) {
                    position <- as.vector(
                        relative[candidate, , drop = FALSE] %*% direction
                    ) /
                        sum(direction^2)
                    candidate <- candidate[order(position)]
                }
                expanded <- c(expanded, vertex_index(start), candidate)
            }
            expanded[c(TRUE, diff(expanded) != 0L)]
        })
        second_count <- count_edges(expanded_indices)
        bad_edges <- names(second_count)[second_count != 2L]
        # T-junctions accepted by the engine's second pass require no repair.
        # First-pass edge differences are candidates only for a truly open room.
        if (!length(bad_edges)) {
            return(list(
                output_ids = character(),
                points = matrix(numeric(), 0L, 3L),
                score = 0,
                defects = 0,
                first_defects = 0
            ))
        }
        first_bad <- names(first_count)[first_count != 2L]
        first_bad_faces <- vapply(
            face_indices,
            function(index) {
                following <- c(index[-1L], index[[1L]])
                any(geom__edge_key(index, following) %in% first_bad)
            },
            logical(1L)
        )
        bad_faces <- vapply(
            expanded_indices,
            function(index) {
                following <- c(index[-1L], index[[1L]])
                any(geom__edge_key(index, following) %in% bad_edges)
            },
            logical(1L)
        )
        vertices <- unique(as.integer(unlist(strsplit(
            bad_edges,
            "/",
            fixed = TRUE
        ))))
        list(
            output_ids = output_ids[bad_faces | first_bad_faces],
            bad_output_ids = output_ids[bad_faces],
            points = unique_vertices[vertices, , drop = FALSE],
            defects = length(bad_edges),
            first_defects = sum(first_count != 2L),
            score = if (!length(bad_edges)) {
                0
            } else {
                1000 * length(bad_edges) + sum(first_count != 2L)
            }
        )
    }

    rooms <- unique(surface$ROOM)
    diagnostic <- lapply(rooms, function(room_name) {
        room_is_closed(surface[
            ROOM == room_name &
                TYPE %in% c("Wall", "Floor", "Ceiling", "Roof")
        ])
    })
    failed <- lengths(lapply(diagnostic, `[[`, "output_ids")) > 0L
    if (!details) {
        return(rooms[failed])
    }
    points <- data.table::rbindlist(lapply(which(failed), function(index) {
        coordinate <- diagnostic[[index]]$points
        data.table::data.table(
            ROOM = rooms[[index]],
            POINT_X = coordinate[, 1L],
            POINT_Y = coordinate[, 2L],
            POINT_Z = coordinate[, 3L]
        )
    }))
    list(
        rooms = rooms[failed],
        output_ids = unlist(
            lapply(diagnostic, `[[`, "output_ids"),
            use.names = FALSE
        ),
        bad_output_ids = unlist(
            lapply(diagnostic, `[[`, "bad_output_ids"),
            use.names = FALSE
        ),
        points = points,
        score = sum(vapply(diagnostic, `[[`, numeric(1L), "score")),
        defects = sum(vapply(diagnostic, `[[`, numeric(1L), "defects")),
        first_defects = sum(vapply(
            diagnostic,
            `[[`,
            numeric(1L),
            "first_defects"
        ))
    )
}

# Merge adjacent triangulation parts with a deterministic greedy policy whenever
# deleting their common diagonal leaves one planar convex polygon. Retained
# collinear junctions prevent a merge from reopening a zone edge.
surface__merge_convex_parts <- function(
    surface,
    profile = eplus_geom__profile(),
    angle_tolerance = profile$angle,
    distance_tolerance = profile$coordinate_distance,
    planarity_tolerance = profile$planarity_distance
) {
    surface <- data.table::copy(surface)
    part_ids <- unique(surface$PART)
    if (length(part_ids) <= 1L) {
        return(surface)
    }

    # Stable coordinate indices remove input-row order from adjacency and tie
    # breaking. The representative row supplies non-coordinate metadata only.
    key <- sprintf(
        "%.12f|%.12f|%.12f",
        surface$POINT_X,
        surface$POINT_Y,
        surface$POINT_Z
    )
    unique_key <- sort(unique(key))
    vertex <- match(key, unique_key)
    representative <- match(unique_key, key)
    coordinates <- as.matrix(surface[
        representative,
        .(POINT_X, POINT_Y, POINT_Z)
    ])
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
        if (length(repeated) != 1L) {
            return(NULL)
        }
        keep <- edge != repeated[[1L]]
        start <- start[keep]
        end <- end[keep]

        boundary <- integer()
        current <- start[[1L]]
        origin <- current
        repeat {
            outgoing <- which(start == current)
            if (length(outgoing) != 1L) {
                return(NULL)
            }
            selected <- outgoing[[1L]]
            boundary <- c(boundary, current)
            current <- end[[selected]]
            start <- start[-selected]
            end <- end[-selected]
            if (current == origin) {
                break
            }
            if (length(boundary) > length(first) + length(second)) return(NULL)
        }
        if (length(start) > 0L || length(boundary) < 3L) {
            return(NULL)
        }
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
        incoming_length <- sqrt(rowSums(incoming^2))
        outgoing_length <- sqrt(rowSums(outgoing^2))
        if (
            any(incoming_length < distance_tolerance) ||
                any(outgoing_length < distance_tolerance)
        ) {
            return(NULL)
        }

        xy <- frame$xy
        turn <- vapply(
            seq_len(nrow(xy)),
            function(index) {
                geom__cross_2d(
                    xy[previous[[index]], ],
                    xy[index, ],
                    xy[following[[index]], ]
                )
            },
            numeric(1L)
        )
        scale <- incoming_length * outgoing_length
        # Collinear boundary points do not require a real exported diagonal.
        turns <- turn[abs(turn) > sin(angle_tolerance) * scale]
        if (!length(turns) || !(all(turns > 0.0) || all(turns < 0.0))) {
            return(NULL)
        }
        list(area = frame$area, key = paste(boundary, collapse = ","))
    }

    repeat {
        if (length(polygon) < 2L) {
            break
        }
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
        if (length(adjacent) == 0L) {
            break
        }

        candidate <- list()
        for (edge in names(adjacent)) {
            owner <- adjacent[[edge]]
            first <- min(owner)
            second <- max(owner)
            boundary <- merge_boundary(polygon[[first]], polygon[[second]])
            if (is.null(boundary)) {
                next
            }
            metric <- merge_metrics(boundary)
            if (is.null(metric)) {
                next
            }
            endpoint <- as.integer(strsplit(edge, "/", fixed = TRUE)[[1L]])
            shared_length <- sqrt(sum(
                (coordinates[endpoint[[1L]], ] -
                    coordinates[endpoint[[2L]], ])^2
            ))
            candidate[[length(candidate) + 1L]] <- list(
                first = first,
                second = second,
                boundary = boundary,
                removed = length(polygon[[first]]) +
                    length(polygon[[second]]) -
                    length(boundary),
                shared_length = shared_length,
                area = metric$area,
                boundary_key = metric$key
            )
        }
        if (length(candidate) == 0L) {
            break
        }

        score <- data.table::rbindlist(lapply(candidate, function(value) {
            data.table::data.table(
                removed = value$removed,
                shared_length = value$shared_length,
                area = value$area,
                boundary_key = value$boundary_key
            )
        }))
        selected <- order(
            -score$removed,
            -score$shared_length,
            -score$area,
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

# Build one canonical polygon per source middle plane. Concavity alone does
# not require decomposition; only explicit clipping consumers select parts.
# A failed room shell is handled later by individual, verified local cuts.
surface__normalize_topology <- function(
    surface,
    window = data.table::data.table(),
    profile = eplus_geom__profile()
) {
    coordinate_columns <- geom__coordinate_columns()
    convex_planes <- surface__convex_planes(
        surface,
        window,
        profile
    )
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
    # Canonical source planes already share their boundary vertices. EnergyPlus
    # accepts harmless T-junctions in its second closure pass; do not insert or
    # protect points from every other face in the room before any failure exists.
    point <- point[,
        {
            surface__validate_outline(.SD, profile, .BY$PLANE)
            value <- surface__simplify_polygon(.SD, profile)
            if (
                nrow(value) >= 3L &&
                    abs(
                        geom__polygon_area(value) -
                            geom__polygon_area(.SD)
                    ) >
                        profile$area
            ) {
                stop(
                    sprintf(
                        "DeST middle plane %s loses area during vertex cleanup.",
                        .BY$PLANE
                    ),
                    call. = FALSE
                )
            }
            if (nrow(value) < 3L) {
                stop(
                    sprintf(
                        "DeST middle plane %s collapses under EnergyPlus vertex cleanup; the source face cannot be silently omitted.",
                        .BY$PLANE
                    ),
                    call. = FALSE
                )
            }
            value
        },
        by = "PLANE"
    ]
    point <- point[,
        {
            concave <- !geom__polygon_is_convex(.SD, profile$angle)
            if (concave && .BY$PLANE %in% convex_planes) {
                # Only consumers requiring convex clipping enter this branch.
                # Prefer direct convex regions for shadow geometry. A protected
                # point alone does not require this split: the later closure
                # check decides which convex faces need junction repair.
                avoid_points <- window[PLANE == .BY$PLANE]
                surface__partition_polygon(.SD, avoid_points, profile)
            } else {
                copy <- data.table::copy(.SD)
                copy[, `:=`(PART = 1L, POINT_NO = seq_len(.N) - 1L)]
                copy
            }
        },
        by = "PLANE"
    ]
    # Delete only triangulation diagonals whose removal leaves a planar convex
    # face; both room-side copies inherit the same deterministic partition.
    point <- point[, surface__merge_convex_parts(.SD, profile), by = "PLANE"]
    point[, PART_COUNT := data.table::uniqueN(PART), by = "PLANE"]

    surface <- merge(metadata, point, by = "PLANE", allow.cartesian = TRUE)
    surface[, ORIGINAL_NAME := NAME]
    # Part 1 of a window host keeps the original name used by the window. Other
    # parts receive deterministic suffixes, mirrored in reciprocal references.
    surface[, PRESERVE_BASE_NAME := PLANE %in% window_plane & PART == 1L]
    surface[
        PART_COUNT > 1L & !PRESERVE_BASE_NAME,
        NAME := sprintf("%s [%d]", NAME, PART)
    ]
    surface[
        PART_COUNT > 1L & !PRESERVE_BASE_NAME & !is.na(BOUNDARY_OBJECT),
        BOUNDARY_OBJECT := sprintf("%s [%d]", BOUNDARY_OBJECT, PART)
    ]
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
    surface,
    all_coordinates,
    profile = eplus_geom__profile()
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
        length_squared <- sum(difference^2)
        if (length_squared <= tolerance^2) {
            # A repeated source endpoint needs only one copy. This does not
            # move any coordinate or import nearby geometry from another face.
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
        projected <- relative -
            position * rep(difference, each = nrow(relative))
        edge_length <- sqrt(length_squared)
        # EnergyPlus treats vertices less than 0.01 m apart as coincident. Do
        # not insert a junction that EnergyPlus would immediately collapse back
        # into either endpoint.
        on_segment <- position * edge_length >= distance_tolerance &
            (1.0 - position) * edge_length >= distance_tolerance &
            sqrt(rowSums(projected^2)) <= tolerance
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
    surface,
    profile = eplus_geom__profile()
) {
    tolerance <- profile$angle
    distance_tolerance <- profile$coordinate_distance
    n_vertex <- nrow(surface)
    previous <- c(n_vertex, seq_len(n_vertex - 1L))
    following <- c(seq.int(2L, n_vertex), 1L)
    coordinates <- as.matrix(surface[, .(POINT_X, POINT_Y, POINT_Z)])
    incoming <- coordinates - coordinates[previous, , drop = FALSE]
    outgoing <- coordinates[following, , drop = FALSE] - coordinates

    incoming_length <- sqrt(rowSums(incoming^2))
    outgoing_length <- sqrt(rowSums(outgoing^2))
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
        (sqrt(rowSums(cross_product^2)) <=
            sin(tolerance) * incoming_length * outgoing_length &
            rowSums(incoming * outgoing) > 0)
}

# Triangulate a simple planar polygon with ear clipping while retaining every
# protected boundary junction. Concave polygons and selected local junction
# repairs use this helper; ordinary convex source faces remain intact.
# Ordinary ear clipping is cubic; the window-clearance search may explore more
# states, so it is capped by the versioned geometry profile before falling back
# to deterministic ordinary clipping. Peer faces reuse identical part numbering.
surface__triangulate_polygon <- function(
    surface,
    avoid_points = data.table::data.table(),
    profile = eplus_geom__profile()
) {
    tolerance <- profile$intersection
    distance_tolerance <- profile$coordinate_distance
    surface <- data.table::copy(surface)
    frame <- geom__polygon_frame(surface, profile$normal_magnitude)
    if (!frame$valid) {
        geom__unit_normal(surface, profile$normal_magnitude)
    }
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
        if (nrow(avoid_xy) == 0L) {
            return(Inf)
        }
        direction <- end - start
        length_squared <- sum(direction^2)
        relative <- sweep(avoid_xy, 2L, start, "-")
        position <- as.vector(relative %*% direction / length_squared)
        projected <- relative - position * rep(direction, each = nrow(relative))
        interior <- position > tolerance & position < 1.0 - tolerance
        if (!any(interior)) {
            return(Inf)
        }
        min(sqrt(rowSums(projected[interior, , drop = FALSE]^2)))
    }

    # A convex host can be partitioned through one interior Steiner point. Try
    # deterministic interior candidates and retain a fan only when every radial
    # edge either clears all window corners by 1 cm or passes through a corner
    # exactly. This removes unavoidable near-corner diagonals in ordinary
    # boundary-only triangulations without changing the aggregate host area.
    if (
        nrow(avoid_xy) > 0L &&
            geom__polygon_is_convex(surface, profile$angle)
    ) {
        span <- apply(xy, 2L, range)
        grid <- expand.grid(
            X = seq(span[1L, 1L], span[2L, 1L], length.out = 11L),
            Y = seq(span[1L, 2L], span[2L, 2L], length.out = 11L)
        )
        # Window corners are valid Steiner centers. Starting all host radials
        # at an exact corner avoids a near-corner cut when no ordinary grid or
        # centroid candidate can maintain EnergyPlus's 1 cm vertex clearance.
        candidate_center <- unique(rbind(
            colMeans(xy),
            colMeans(avoid_xy),
            avoid_xy,
            as.matrix(grid)
        ))
        host_next <- seq_len(nrow(xy)) %% nrow(xy) + 1L
        inside <- apply(candidate_center, 1L, function(point) {
            all(vapply(
                seq_len(nrow(xy)),
                function(index) {
                    geom__cross_2d(
                        xy[index, ],
                        xy[host_next[[index]], ],
                        point
                    ) >
                        tolerance
                },
                logical(1L)
            ))
        })
        candidate_center <- candidate_center[inside, , drop = FALSE]
        if (nrow(candidate_center) > 0L) {
            clearance <- apply(candidate_center, 1L, function(center) {
                radial_length <- sqrt(rowSums(sweep(xy, 2L, center, "-")^2))
                if (any(radial_length < distance_tolerance)) {
                    return(-Inf)
                }
                radial <- vapply(
                    seq_len(nrow(xy)),
                    function(index) {
                        diagonal_clearance(center, xy[index, ])
                    },
                    numeric(1L)
                )
                radial[radial <= tolerance] <- Inf
                min(radial)
            })
            clear <- which(clearance >= distance_tolerance)
            if (length(clear) > 0L) {
                center_xy <- candidate_center[
                    clear[[which.max(clearance[clear])]],
                ]
                center_xyz <- numeric(3L)
                center_xyz[projection] <- center_xy
                omitted <- setdiff(1:3, projection)
                origin <- as.numeric(surface[1L, .(POINT_X, POINT_Y, POINT_Z)])
                center_xyz[omitted] <- origin[omitted] -
                    sum(
                        normal[projection] *
                            (center_xyz[projection] - origin[projection])
                    ) /
                        normal[omitted]
                return(data.table::rbindlist(lapply(
                    seq_len(nrow(surface)),
                    function(part) {
                        following <- part %% nrow(surface) + 1L
                        value <- data.table::copy(surface[c(
                            part,
                            part,
                            following
                        )])
                        value[
                            1L,
                            `:=`(
                                POINT_X = center_xyz[[1L]],
                                POINT_Y = center_xyz[[2L]],
                                POINT_Z = center_xyz[[3L]]
                            )
                        ]
                        value[, `:=`(PART = part, POINT_NO = 0:2)]
                        value
                    }
                )))
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
            if (
                geom__cross_2d(
                    xy[previous, ],
                    xy[current, ],
                    xy[following, ]
                ) <=
                    tolerance
            ) {
                next
            }
            candidate_xy <- xy[c(previous, current, following), , drop = FALSE]
            triangle_next <- c(2L, 3L, 1L)
            if (
                any(
                    sqrt(rowSums(
                        (candidate_xy -
                            candidate_xy[triangle_next, , drop = FALSE])^2
                    )) <
                        distance_tolerance
                )
            ) {
                next
            }
            other <- setdiff(remaining, c(previous, current, following))
            contains <- vapply(
                other,
                function(index) {
                    inside_triangle(
                        xy[index, ],
                        xy[previous, ],
                        xy[current, ],
                        xy[following, ]
                    )
                },
                logical(1L)
            )
            # The diagonal from previous to following stays inside a simple
            # polygon only when no remaining vertex lies in the ear triangle.
            if (any(contains)) {
                next
            }

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
        if (length(remaining) == 3L) {
            return(list(remaining))
        }
        state <- paste(remaining, collapse = ",")
        if (isTRUE(failed_state[[state]])) {
            return(NULL)
        }
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
    if (
        any(
            sqrt(rowSums(
                (final_xy - final_xy[c(2L, 3L, 1L), , drop = FALSE])^2
            )) <
                distance_tolerance
        )
    ) {
        stop(
            "Could not triangulate a DeST surface without a sub-centimetre edge."
        )
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
    surface,
    profile = eplus_geom__profile()
) {
    distance_tolerance <- profile$coordinate_distance
    surface <- data.table::copy(surface)

    repeat {
        n_vertex <- nrow(surface)
        coordinates <- as.matrix(surface[, .(POINT_X, POINT_Y, POINT_Z)])
        following <- seq_len(n_vertex) %% n_vertex + 1L
        short_edge <- which(
            sqrt(rowSums(
                (coordinates - coordinates[following, , drop = FALSE])^2
            )) <
                distance_tolerance
        )
        if (n_vertex <= 3L) {
            # A triangle with a sub-centimetre edge becomes a line after
            # EnergyPlus merges the two endpoints. The source face is already
            # below EnergyPlus's representable resolution, so omit the sliver.
            if (length(short_edge) > 0L) {
                return(surface[0L])
            }
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

        if (!any(redundant) || n_vertex - sum(redundant) < 3L) {
            break
        }
        surface <- surface[!redundant]
    }

    if (nrow(surface) >= 3L) {
        # Vertex cleanup can collapse a very narrow source polygon into one line even
        # when its remaining edges are long. Omit the zero-area face before any
        # normal or convexity calculation tries to interpret it.
        if (
            sqrt(sum(geom__newell_vector(surface)^2)) <=
                profile$normal_magnitude
        ) {
            return(surface[0L])
        }
    }

    data.table::set(surface, NULL, "POINT_NO", seq_len(nrow(surface)) - 1L)
    surface
}
