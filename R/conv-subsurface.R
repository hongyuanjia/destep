# Clip one planar subsurface polygon against one convex host part. Intersections are
# interpolated in 3-D so the result stays on the original DeST middle plane even
# when the stable clipping projection omits a non-constant coordinate.
subsurface__clip_polygon <- function(
    subsurface,
    host,
    profile = eplus_geom__profile()
) {
    tolerance <- profile$intersection
    distance_tolerance <- profile$coordinate_distance
    boundary_inset <- profile$boundary_inset_distance
    frame <- geom__polygon_frame(host, profile$normal_magnitude)
    if (!frame$valid) {
        geom__unit_normal(host, profile$normal_magnitude)
    }
    if (
        frame$planarity_error > profile$planarity_distance ||
            !geom__polygon_is_convex(host, profile$angle)
    ) {
        stop(
            "A converted EnergyPlus subsurface host must be planar and convex."
        )
    }
    normal <- frame$normal
    projection <- frame$projection
    host_xy <- frame$xy
    if (frame$signed_area < 0.0) {
        host_xy <- host_xy[nrow(host_xy):1L, , drop = FALSE]
    }

    xyz <- as.matrix(subsurface[, .(POINT_X, POINT_Y, POINT_Z)])
    host_origin <- frame$coordinates[1L, ]
    plane_error <- abs(as.vector(sweep(xyz, 2L, host_origin, "-") %*% normal))
    if (any(plane_error > profile$planarity_distance)) {
        stop(
            "A DeST subsurface polygon is not coplanar with its converted host."
        )
    }
    value <- cbind(xyz, xyz[, projection, drop = FALSE])
    intersection <- function(start, end, clip_start, clip_end) {
        edge <- clip_end - clip_start
        segment <- end[4:5] - start[4:5]
        denominator <- edge[[1L]] * segment[[2L]] - edge[[2L]] * segment[[1L]]
        if (abs(denominator) <= tolerance) {
            return(start)
        }
        relative <- start[4:5] - clip_start
        position <- -(edge[[1L]] *
            relative[[2L]] -
            edge[[2L]] * relative[[1L]]) /
            denominator
        start + position * (end - start)
    }

    # Sutherland-Hodgman clipping is valid here because every host part passed
    # by surface conversion is either one convex face or one triangle.
    host_next <- c(seq.int(2L, nrow(host_xy)), 1L)
    for (edge_index in seq_len(nrow(host_xy))) {
        if (nrow(value) == 0L) {
            break
        }
        clip_start <- host_xy[edge_index, ]
        clip_end <- host_xy[host_next[[edge_index]], ]
        input <- value
        value <- matrix(numeric(), nrow = 0L, ncol = 5L)
        previous <- input[nrow(input), ]
        previous_inside <- geom__cross_2d(
            clip_start,
            clip_end,
            previous[4:5]
        ) >=
            -tolerance
        for (index in seq_len(nrow(input))) {
            current <- input[index, ]
            current_inside <- geom__cross_2d(
                clip_start,
                clip_end,
                current[4:5]
            ) >=
                -tolerance
            if (current_inside) {
                if (!previous_inside) {
                    value <- rbind(
                        value,
                        intersection(
                            previous,
                            current,
                            clip_start,
                            clip_end
                        )
                    )
                }
                value <- rbind(value, current)
            } else if (previous_inside) {
                value <- rbind(
                    value,
                    intersection(
                        previous,
                        current,
                        clip_start,
                        clip_end
                    )
                )
            }
            previous <- current
            previous_inside <- current_inside
        }
    }
    if (nrow(value) < 3L) {
        return(NULL)
    }

    # The reference EnergyPlus profile's CHKSBS test treats some vertices on
    # a triangulated host boundary as outside even when the analytical distance
    # is zero. Move only exact-boundary vertices an infinitesimal distance toward
    # the convex host centroid. The displacement is many orders below the
    # conversion coordinate tolerance; aggregate subsurface-area invariants remain
    # enforced.
    host_centroid <- colMeans(as.matrix(
        host[, .(POINT_X, POINT_Y, POINT_Z)]
    ))
    boundary <- vapply(
        seq_len(nrow(value)),
        function(index) {
            any(vapply(
                seq_len(nrow(host_xy)),
                function(edge_index) {
                    edge_end <- host_xy[host_next[[edge_index]], ]
                    edge <- edge_end - host_xy[edge_index, ]
                    abs(geom__cross_2d(
                        host_xy[edge_index, ],
                        edge_end,
                        value[index, 4:5]
                    )) /
                        sqrt(sum(edge^2)) <=
                        tolerance
                },
                logical(1L)
            ))
        },
        logical(1L)
    )
    if (any(boundary)) {
        for (index in which(boundary)) {
            direction <- host_centroid - value[index, 1:3]
            value[index, 1:3] <- value[index, 1:3] +
                boundary_inset * direction / sqrt(sum(direction^2))
        }
        value[, 4:5] <- value[, projection, drop = FALSE]
    }

    # Remove numerical duplicates introduced when a subsurface corner lies exactly
    # on a host diagonal. Sub-centimetre slivers cannot survive EnergyPlus input
    # processing and are rejected instead of becoming degenerate windows.
    repeat {
        following <- seq_len(nrow(value)) %% nrow(value) + 1L
        short <- which(
            sqrt(rowSums(
                (value[, 1:3, drop = FALSE] -
                    value[following, 1:3, drop = FALSE])^2
            )) <
                distance_tolerance
        )
        if (length(short) == 0L) {
            break
        }
        if (nrow(value) <= 3L) {
            return(NULL)
        }
        value <- value[-following[short[[1L]]], , drop = FALSE]
    }

    out <- data.table::data.table(
        POINT_X = value[, 1L],
        POINT_Y = value[, 2L],
        POINT_Z = value[, 3L]
    )
    out[, POINT_NO := seq_len(.N) - 1L]
    if (
        !geom__polygon_frame(out, profile$normal_magnitude)$valid ||
            geom__polygon_area(out) <= profile$area
    ) {
        return(NULL)
    }
    out
}

# Split windows with the exact host parts produced by surface conversion. The
# same deterministic part number is used on both sides of an interzone opening,
# allowing reciprocal FenestrationSurface references after clipping.
subsurface__split_by_surface <- function(
    subsurface,
    surface = NULL,
    profile = eplus_geom__profile()
) {
    expected <- unique(subsurface[, .(OUTPUT_ID, ORIGINAL_NAME)])
    if (is.null(surface)) {
        subsurface[, PART := 1L]
    } else {
        coordinate_columns <- geom__coordinate_columns()
        subsurface <- subsurface[,
            {
                source <- data.table::copy(.SD)
                host <- surface[ID == source$SURFACE_ID[[1L]]]
                if (nrow(host) == 0L) {
                    stop(sprintf(
                        "Could not find the EnergyPlus host for DeST subsurface '%s'.",
                        source$ORIGINAL_NAME[[1L]]
                    ))
                }
                part <- lapply(unique(host$PART), function(part_id) {
                    host_part <- host[PART == part_id]
                    clipped <- subsurface__clip_polygon(
                        source,
                        host_part,
                        profile
                    )
                    if (is.null(clipped)) {
                        return(NULL)
                    }
                    # Canonical winding and start coordinates make reciprocal subsurface
                    # sides choose the same diagonal and subpart numbering.
                    clipped <- geom__canonicalize_polygon(
                        clipped,
                        profile$normal_magnitude
                    )
                    # Preserve rectangles as one EnergyPlus subsurface. Other
                    # polygons are triangulated because EnergyPlus may replace a
                    # four-sided non-rectangle with an equivalent rectangle.
                    clipped_part <- if (
                        nrow(clipped) <= 3L ||
                            geom__polygon_is_rectangle(
                                clipped,
                                angle_tolerance = profile$angle,
                                distance_tolerance = profile$coordinate_distance,
                                planarity_tolerance = profile$planarity_distance
                            )
                    ) {
                        list(clipped)
                    } else {
                        if (!geom__polygon_is_convex(clipped, profile$angle)) {
                            stop(paste(
                                "DeST windows with concave polygons are not",
                                "supported by EnergyPlus subsurface conversion."
                            ))
                        }
                        # The clipped subject and host are convex, so a canonical
                        # first-vertex fan stays inside the polygon and is identical
                        # for reciprocal room-side copies.
                        lapply(
                            seq.int(2L, nrow(clipped) - 1L),
                            function(index) {
                                value <- data.table::copy(
                                    clipped[c(1L, index, index + 1L)]
                                )
                                value[, POINT_NO := 0:2]
                                value
                            }
                        )
                    }
                    metadata <- source[
                        1L,
                        setdiff(names(source), coordinate_columns),
                        with = FALSE
                    ]
                    data.table::rbindlist(lapply(
                        seq_along(clipped_part),
                        function(index) {
                            value <- clipped_part[[index]]
                            part_metadata <- data.table::copy(metadata)
                            part_metadata[, `:=`(
                                PART = part_id,
                                SUBPART = index,
                                SURFACE_NAME = host_part$NAME[[1L]]
                            )]
                            cbind(part_metadata[rep(1L, nrow(value))], value)
                        }
                    ))
                })
                data.table::rbindlist(part, fill = TRUE)
            },
            by = "OUTPUT_ID"
        ]
    }

    # A subsurface completely outside its converted host indicates inconsistent
    # source geometry or an incorrect host mapping. Failing here prevents a
    # valid DeST opening from disappearing silently during clipping.
    missing <- expected[!subsurface, on = "OUTPUT_ID"]
    if (nrow(missing) > 0L) {
        stop(sprintf(
            "Could not place DeST subsurface '%s' on any converted host surface.",
            missing$ORIGINAL_NAME[[1L]]
        ))
    }

    if (!"SUBPART" %in% names(subsurface)) {
        subsurface[, SUBPART := 1L]
    }
    data.table::setorderv(
        subsurface,
        c("OUTPUT_ID", "PART", "SUBPART", "POINT_NO")
    )
    subsurface[, PIECE := data.table::rleid(PART, SUBPART), by = "OUTPUT_ID"]
    subsurface[, PART_COUNT := data.table::uniqueN(PIECE), by = "OUTPUT_ID"]
    subsurface[,
        NAME := data.table::fcase(
            INTERZONE & PART_COUNT > 1L                                 ,
            sprintf("%s [Side %d Part %d]", ORIGINAL_NAME, SIDE, PIECE) ,
            INTERZONE                                                   ,
            sprintf("%s [%d]", ORIGINAL_NAME, SIDE)                     ,
            PART_COUNT > 1L                                             ,
            sprintf("%s [Part %d]", ORIGINAL_NAME, PIECE)               ,
            default = ORIGINAL_NAME
        )
    ]
    subsurface[,
        BOUNDARY_OBJECT := data.table::fcase(
            INTERZONE & PART_COUNT > 1L                                      ,
            sprintf("%s [Side %d Part %d]", ORIGINAL_NAME, 3L - SIDE, PIECE) ,
            INTERZONE                                                        ,
            sprintf("%s [%d]", ORIGINAL_NAME, 3L - SIDE)                     ,
            default = NA_character_
        )
    ]
    subsurface[, OUTPUT_PART_ID := sprintf("%s-%d", OUTPUT_ID, PIECE)]
    data.table::setorderv(subsurface, c("OUTPUT_ID", "PIECE", "POINT_NO"))
    subsurface
}

# Expand one room-facing copy of every DeST subsurface from its SIDE1/SIDE2 source
# columns. A shared implementation keeps exterior and interzone classification
# identical for both directions.
subsurface__expand_side <- function(subsurface, side) {
    checkmate::assert_choice(side, c(1L, 2L))

    # Keep both side mappings explicit. The DeST field names are fixed, and
    # direct data.table column references make the direction of every copied
    # property visible at the call site.
    if (side == 1L) {
        value <- data.table::copy(
            subsurface[!SIDE1_SURFACE_TYPE %in% c(1L, 2L)]
        )
        value[, `:=`(
            ORIGINAL_NAME = NAME,
            SIDE = 1L,
            SURFACE_ID = SIDE1_SURFACE_ID,
            SURFACE_NAME = SIDE1_SURFACE_NAME,
            AZIMUTH = SIDE1_AZIMUTH,
            TILT = SIDE1_TILT,
            INSIDE_CONVECTION_COEFFICIENT = SIDE1_CONVECTION_COEFFICIENT,
            OUTSIDE_CONVECTION_COEFFICIENT = SIDE2_CONVECTION_COEFFICIENT,
            INTERZONE = !SIDE2_SURFACE_TYPE %in% c(1L, 2L),
            BOUNDARY_OBJECT = NA_character_
        )]
    } else {
        value <- data.table::copy(
            subsurface[!SIDE2_SURFACE_TYPE %in% c(1L, 2L)]
        )
        value[, `:=`(
            ORIGINAL_NAME = NAME,
            SIDE = 2L,
            SURFACE_ID = SIDE2_SURFACE_ID,
            SURFACE_NAME = SIDE2_SURFACE_NAME,
            AZIMUTH = SIDE2_AZIMUTH,
            TILT = SIDE2_TILT,
            INSIDE_CONVECTION_COEFFICIENT = SIDE2_CONVECTION_COEFFICIENT,
            OUTSIDE_CONVECTION_COEFFICIENT = SIDE1_CONVECTION_COEFFICIENT,
            INTERZONE = !SIDE1_SURFACE_TYPE %in% c(1L, 2L),
            BOUNDARY_OBJECT = NA_character_
        )]
    }
    if (nrow(value) == 0L) {
        return(value)
    }
    value
}

# Read DeST window geometry and resolve the same detailed or aggregate
# construction fallback used by the construction converter.
window__source_table <- function(dest) {
    window <- DBI::dbGetQuery(
        dest,
        "
        WITH WIN_CONST AS (
            SELECT
                W.ID,
                CASE
                    WHEN W.WINDOW_CONSTRUCTION != 0 THEN W.WINDOW_CONSTRUCTION
                    ELSE D.LONG
                END AS WINDOW_CONSTRUCTION
            FROM WINDOW W
            LEFT JOIN DEFAULT_SETTING D
            ON W.WINDOW_CONSTRUCTION = 0 AND
               D.TABLE_NAME = 'WINDOW' AND D.FIELD_NAME = 'WINDOW_CONSTRUCTION' AND D.TYPE = 2
        )
        SELECT
            W.ID           AS ID,
            W.NAME         AS NAME,
            W.TYPE         AS TYPE,
            SW.CNAME       AS CONSTRUCTION,
            E.SIDE1        AS SIDE1_SURFACE_ID,
            H1.NAME        AS SIDE1_SURFACE_NAME,
            S1.TYPE        AS SIDE1_SURFACE_TYPE,
            S1.AZIMUTH     AS SIDE1_AZIMUTH,
            S1.TILT        AS SIDE1_TILT,
            S1.VENTILATION_COEF AS SIDE1_CONVECTION_COEFFICIENT,
            E.SIDE2        AS SIDE2_SURFACE_ID,
            H2.NAME        AS SIDE2_SURFACE_NAME,
            S2.TYPE        AS SIDE2_SURFACE_TYPE,
            S2.AZIMUTH     AS SIDE2_AZIMUTH,
            S2.TILT        AS SIDE2_TILT,
            S2.VENTILATION_COEF AS SIDE2_CONVECTION_COEFFICIENT,
            L.POINT_NO     AS POINT_NO,
            ROUND(P.X, 3)  AS POINT_X,
            ROUND(P.Y, 3)  AS POINT_Y,
            ROUND(P.Z, 3)  AS POINT_Z
        FROM WINDOW W
        LEFT JOIN MAIN_ENCLOSURE E
        ON W.OF_ENCLOSURE = E.ID
        LEFT JOIN SURFACE H1
        ON E.SIDE1 = H1.SURFACE_ID
        LEFT JOIN SURFACE H2
        ON E.SIDE2 = H2.SURFACE_ID
        LEFT JOIN SURFACE S1
        ON W.SIDE1 = S1.SURFACE_ID
        LEFT JOIN SURFACE S2
        ON W.SIDE2 = S2.SURFACE_ID
        LEFT JOIN WIN_CONST WC
        ON W.ID = WC.ID
        LEFT JOIN SYS_WINDOW SW
        ON WC.WINDOW_CONSTRUCTION = SW.WINDOW_ID
        LEFT JOIN PLANE PL
        ON W.MIDDLE_PLANE = PL.PLANE_ID
        LEFT JOIN GEOMETRY G
        ON PL.GEOMETRY = G.GEOMETRY_ID
        LEFT JOIN LOOP_POINT L
        ON G.BOUNDARY_LOOP_ID = L.LOOP_ID
        LEFT JOIN POINT P
        ON L.POINT = P.POINT_ID
        ORDER BY W.ID, L.POINT_NO
        "
    )

    data.table::setDT(window)

    # Replace the detailed SYS_WINDOW construction only when the referenced
    # WINDOW_TYPE_DATA record supplies valid aggregate EnergyPlus inputs. The
    # shared resolver preserves the same fallback decision used by construction
    # conversion when type data are missing or invalid.
    window_type <- const__window_type_performance(dest)
    valid_type <- window_type[TYPE_DATA_VALID == TRUE]
    if (nrow(valid_type) > 0L) {
        construction <- stats::setNames(
            valid_type$TYPE_CONSTRUCTION_NAME,
            valid_type$WINDOW_ID
        )
        matched <- window$ID %in% valid_type$WINDOW_ID
        window[matched, CONSTRUCTION := construction[as.character(ID)]]
    }

    window
}

# Read DeST door geometry and resolve default construction references.
door__source_table <- function(dest) {
    door <- DBI::dbGetQuery(
        dest,
        "
        WITH DOOR_CONST AS (
            SELECT
                DR.ID,
                CASE
                    WHEN DR.DOOR_CONSTRUCTION != 0 THEN DR.DOOR_CONSTRUCTION
                    ELSE D.LONG
                END AS DOOR_CONSTRUCTION
            FROM DOOR DR
            LEFT JOIN DEFAULT_SETTING D
            ON DR.DOOR_CONSTRUCTION = 0 AND
               D.TABLE_NAME = 'DOOR' AND
               D.FIELD_NAME = 'DOOR_CONSTRUCTION' AND
               D.TYPE = 2
        )
        SELECT
            DR.ID          AS ID,
            DR.NAME        AS NAME,
            SD.CNAME       AS CONSTRUCTION,
            E.SIDE1        AS SIDE1_SURFACE_ID,
            H1.NAME        AS SIDE1_SURFACE_NAME,
            H1.TYPE        AS SIDE1_SURFACE_TYPE,
            H1.AZIMUTH     AS SIDE1_AZIMUTH,
            H1.TILT        AS SIDE1_TILT,
            H1.ABSORB_COEF AS SIDE1_SOLAR_ABSORPTANCE,
            H1.BLACKNESS   AS SIDE1_THERMAL_ABSORPTANCE,
            H1.VENTILATION_COEF AS SIDE1_CONVECTION_COEFFICIENT,
            E.SIDE2        AS SIDE2_SURFACE_ID,
            H2.NAME        AS SIDE2_SURFACE_NAME,
            H2.TYPE        AS SIDE2_SURFACE_TYPE,
            H2.AZIMUTH     AS SIDE2_AZIMUTH,
            H2.TILT        AS SIDE2_TILT,
            H2.ABSORB_COEF AS SIDE2_SOLAR_ABSORPTANCE,
            H2.BLACKNESS   AS SIDE2_THERMAL_ABSORPTANCE,
            H2.VENTILATION_COEF AS SIDE2_CONVECTION_COEFFICIENT,
            L.POINT_NO     AS POINT_NO,
            ROUND(P.X, 3)  AS POINT_X,
            ROUND(P.Y, 3)  AS POINT_Y,
            ROUND(P.Z, 3)  AS POINT_Z
        FROM DOOR DR
        LEFT JOIN MAIN_ENCLOSURE E
        ON DR.OF_ENCLOSURE = E.ID
        LEFT JOIN SURFACE H1
        ON E.SIDE1 = H1.SURFACE_ID
        LEFT JOIN SURFACE H2
        ON E.SIDE2 = H2.SURFACE_ID
        LEFT JOIN DOOR_CONST DC
        ON DR.ID = DC.ID
        LEFT JOIN SYS_DOOR SD
        ON DC.DOOR_CONSTRUCTION = SD.DOOR_ID
        LEFT JOIN PLANE PL
        ON DR.MIDDLE_PLANE = PL.PLANE_ID
        LEFT JOIN GEOMETRY G
        ON PL.GEOMETRY = G.GEOMETRY_ID
        LEFT JOIN LOOP_POINT L
        ON G.BOUNDARY_LOOP_ID = L.LOOP_ID
        LEFT JOIN POINT P
        ON L.POINT = P.POINT_ID
        ORDER BY DR.ID, L.POINT_NO
        "
    )
    data.table::setDT(door)
    door
}

# Convert one row group for each final subsurface part into the ordered fields
# expected by FenestrationSurface:Detailed.
subsurface__object_values <- function(subsurface, surface_type) {
    checkmate::assert_choice(surface_type, c("Window", "Door"))
    subsurface[,
        by = "OUTPUT_PART_ID",
        list(
            value = list(c(
                list(
                    # 01: Name
                    name = NAME[[1L]],
                    # 02: Surface Type
                    surface_type = surface_type,
                    # 03: Construction Name
                    construction_name = CONSTRUCTION[[1L]],
                    # 04: Building Surface Name
                    building_surface_name = SURFACE_NAME[[1L]],
                    # 05: Outside Boundary Condition Object
                    outside_boundary_condition_object = if (
                        !is.na(BOUNDARY_OBJECT[[1L]])
                    ) {
                        BOUNDARY_OBJECT[[1L]]
                    },
                    # 06: View Factor to Ground
                    view_factor_to_ground = NULL,
                    # 07: Frame and Divider Name
                    frame_and_divider_name = NULL,
                    # 08: Multiplier
                    multiplier = NULL,
                    # 09: Number of Vertices
                    number_of_vertices = max(POINT_NO) + 1L
                ),
                # Vertices
                geom__eplus_vertex_values(.SD)
            ))
        )
    ]$value
}

# Assign deterministic construction variants for opaque subsurface faces.
subsurface_property__assign_constructions <- function(subsurface) {
    property_fields <- c(
        "SIDE1_SOLAR_ABSORPTANCE",
        "SIDE1_THERMAL_ABSORPTANCE",
        "SIDE2_SOLAR_ABSORPTANCE",
        "SIDE2_THERMAL_ABSORPTANCE"
    )
    if (!all(property_fields %in% names(subsurface))) {
        return(subsurface)
    }

    dt_force_numeric(subsurface, property_fields)
    side1 <- subsurface$SIDE == 1L
    subsurface[, `:=`(
        INSIDE_SOLAR_ABSORPTANCE = data.table::fifelse(
            side1,
            SIDE1_SOLAR_ABSORPTANCE,
            SIDE2_SOLAR_ABSORPTANCE
        ),
        INSIDE_THERMAL_ABSORPTANCE = data.table::fifelse(
            side1,
            SIDE1_THERMAL_ABSORPTANCE,
            SIDE2_THERMAL_ABSORPTANCE
        ),
        OUTSIDE_SOLAR_ABSORPTANCE = data.table::fifelse(
            side1,
            SIDE2_SOLAR_ABSORPTANCE,
            SIDE1_SOLAR_ABSORPTANCE
        ),
        OUTSIDE_THERMAL_ABSORPTANCE = data.table::fifelse(
            side1,
            SIDE2_THERMAL_ABSORPTANCE,
            SIDE1_THERMAL_ABSORPTANCE
        )
    )]

    values <- unlist(
        subsurface[,
            .(
                INSIDE_SOLAR_ABSORPTANCE,
                INSIDE_THERMAL_ABSORPTANCE,
                OUTSIDE_SOLAR_ABSORPTANCE,
                OUTSIDE_THERMAL_ABSORPTANCE
            )
        ],
        use.names = FALSE
    )
    if (any(!is.finite(values) | values < 0.0 | values > 1.0)) {
        stop(
            "Invalid DeST subsurface absorptance or blackness.",
            call. = FALSE
        )
    }

    # EnergyPlus thermal absorptance excludes exact zero and one.
    thermal_fields <- c(
        "INSIDE_THERMAL_ABSORPTANCE",
        "OUTSIDE_THERMAL_ABSORPTANCE"
    )
    for (field in thermal_fields) {
        data.table::set(
            subsurface,
            j = field,
            value = pmin(pmax(subsurface[[field]], 1e-6), 0.99999)
        )
    }

    subsurface[, BASE_CONSTRUCTION := CONSTRUCTION]
    subsurface[,
        CONSTRUCTION := sprintf(
            "%s [DeST i-a%.15g-e%.15g o-a%.15g-e%.15g]",
            BASE_CONSTRUCTION,
            INSIDE_SOLAR_ABSORPTANCE,
            INSIDE_THERMAL_ABSORPTANCE,
            OUTSIDE_SOLAR_ABSORPTANCE,
            OUTSIDE_THERMAL_ABSORPTANCE
        )
    ]
    subsurface[
        INSIDE_SOLAR_ABSORPTANCE == 0.7 &
            INSIDE_THERMAL_ABSORPTANCE == 0.9 &
            OUTSIDE_SOLAR_ABSORPTANCE == 0.7 &
            OUTSIDE_THERMAL_ABSORPTANCE == 0.9,
        CONSTRUCTION := BASE_CONSTRUCTION
    ]
    subsurface
}

# Convert the fixed film coefficients on both DeST opening faces to one
# SurfaceProperty object for every final EnergyPlus subsurface piece.
subsurface_property__convection_values <- function(subsurface) {
    fields <- c(
        "OUTPUT_PART_ID",
        "NAME",
        "INTERZONE",
        "INSIDE_CONVECTION_COEFFICIENT",
        "OUTSIDE_CONVECTION_COEFFICIENT"
    )
    checkmate::assert_subset(fields, names(subsurface))
    object <- unique(subsurface[, fields, with = FALSE])
    checkmate::assert_integerish(
        object[, .N, by = "OUTPUT_PART_ID"]$N,
        lower = 1L,
        upper = 1L,
        .var.name = "one coefficient record per exported DeST subsurface"
    )
    dt_force_numeric(
        object,
        c(
            "INSIDE_CONVECTION_COEFFICIENT",
            "OUTSIDE_CONVECTION_COEFFICIENT"
        )
    )
    checkmate::assert_numeric(
        object$INSIDE_CONVECTION_COEFFICIENT,
        lower = 0.1,
        finite = TRUE,
        any.missing = FALSE,
        .var.name = "DeST subsurface inside convection coefficients"
    )
    checkmate::assert_numeric(
        object[INTERZONE == FALSE, OUTSIDE_CONVECTION_COEFFICIENT],
        lower = 0.1,
        finite = TRUE,
        any.missing = FALSE,
        .var.name = "DeST subsurface outside convection coefficients"
    )

    lapply(seq_len(nrow(object)), function(index) {
        row <- object[index]
        value <- list(
            surface_name = row$NAME,
            convection_coefficient_1_location = "Inside",
            convection_coefficient_1_type = "Value",
            convection_coefficient_1 = row$INSIDE_CONVECTION_COEFFICIENT
        )
        # Each interzone copy already represents its own room-facing film. An
        # exterior coefficient is required only for an outdoor-facing opening.
        if (!row$INTERZONE) {
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

# Convert one DeST opening table into room-facing EnergyPlus subsurfaces.
subsurface__convert <- function(
    source,
    dest,
    ep,
    surface_type,
    surface = NULL,
    geometry_profile = eplus_geom__profile(ep$version())
) {
    checkmate::assert_data_table(source, min.rows = 1L)
    checkmate::assert_choice(surface_type, c("Window", "Door"))

    # One middle-plane polygon becomes one room-facing copy for an exterior
    # opening and two reciprocal copies for an interzone opening.
    side1 <- subsurface__expand_side(source, 1L)
    side2 <- subsurface__expand_side(source, 2L)
    subsurface <- data.table::rbindlist(list(side1, side2), fill = TRUE)
    subsurface[, OUTPUT_ID := sprintf("%s-%d", ID, SIDE)]
    # DeST layers follow the same SIDE1-to-SIDE2 direction as their enclosure.
    # SIDE1 therefore references the explicit reversed construction stack.
    subsurface[
        SIDE == 1L,
        CONSTRUCTION := sprintf("%s [Reverse]", CONSTRUCTION)
    ]
    subsurface <- subsurface_property__assign_constructions(subsurface)
    data.table::setorderv(subsurface, c("OUTPUT_ID", "POINT_NO"))

    # Apply the parent-surface direction metadata before and after clipping so
    # every emitted piece faces away from its owning EnergyPlus zone.
    south_direction <- geom__south_direction(dest)
    subsurface <- subsurface[,
        geom__orient_surface_polygon(
            .SD,
            south_direction,
            geometry_profile
        ),
        by = "OUTPUT_ID"
    ]
    subsurface <- subsurface__split_by_surface(
        subsurface,
        surface,
        geometry_profile
    )
    subsurface <- subsurface[,
        geom__orient_surface_polygon(
            .SD,
            south_direction,
            geometry_profile
        ),
        by = "OUTPUT_PART_ID"
    ]
    assert_unique_name(
        subsurface$NAME[subsurface$POINT_NO == 0L],
        tolower(surface_type)
    )

    value <- subsurface__object_values(subsurface, surface_type)
    opening <- conv__add_objects(
        dest,
        ep,
        "FenestrationSurface:Detailed",
        value
    )
    convection <- conv__add_objects(
        dest,
        ep,
        "SurfaceProperty:ConvectionCoefficients",
        subsurface_property__convection_values(subsurface)
    )
    out <- conv__combine_outputs(
        list(opening = opening, convection = convection),
        table = subsurface
    )
    attr(out, "table") <- subsurface
    out
}

# Warn when DeST window-level transmitted-solar fractions cannot be projected
# to an equivalent EnergyPlus input. The warning prevents a documented source
# concept from being omitted silently while the mapping remains unresolved.
window__warn_transmitted_solar_distribution <- function(dest) {
    fields <- "SUN_TRANS_DIST_MODE"
    distribution_fields <- c(
        "DIST_MODE_ID",
        "DIST_AIR",
        "DIST_ROOF",
        "DIST_FLOOR",
        "DIST_AROUND"
    )
    if (
        !db_has_rows(dest, "WINDOW") ||
            !db_has_rows(dest, "DIST_MODE") ||
            !db_has_fields(dest, "WINDOW", fields) ||
            !db_has_fields(dest, "DIST_MODE", distribution_fields)
    ) {
        return(invisible(FALSE))
    }

    # Restrict the warning to distribution records referenced by actual windows;
    # unrelated gain-distribution modes use the same DeST table.
    modes <- data.table::as.data.table(DBI::dbGetQuery(
        dest,
        "
        SELECT DISTINCT
            D.DIST_MODE_ID,
            D.DIST_AIR,
            D.DIST_ROOF,
            D.DIST_FLOOR,
            D.DIST_AROUND
        FROM WINDOW W
        INNER JOIN DIST_MODE D
        ON W.SUN_TRANS_DIST_MODE = D.DIST_MODE_ID
        WHERE W.SUN_TRANS_DIST_MODE IS NOT NULL
        ORDER BY D.DIST_MODE_ID
    "
    ))
    if (nrow(modes) == 0L) {
        return(invisible(FALSE))
    }

    warn(
        sprintf(
            paste(
                "DeST window transmitted-solar distribution mode(s) [%s]",
                "are not converted. WINDOW.SUN_TRANS_DIST_MODE and the",
                "referenced DIST_MODE air, roof, floor, and surrounding-surface",
                "fractions have no established direct EnergyPlus projection;",
                "the converted model uses the EnergyPlus building-level",
                "solar-distribution algorithm."
            ),
            fmt_integer_sample(modes$DIST_MODE_ID)
        ),
        class = "destep_unsupported_solar_distribution"
    )
    invisible(TRUE)
}

# WINDOW -> FenestrationSurface:Detailed.
window__convert <- function(
    dest,
    ep,
    surface = NULL,
    geometry_profile = eplus_geom__profile(ep$version())
) {
    if (!db_has_rows(dest, "WINDOW")) {
        return(NULL)
    }

    window__warn_transmitted_solar_distribution(dest)

    # Detailed and aggregate window constructions share the same geometry path.
    source <- window__source_table(dest)
    subsurface__convert(
        source,
        dest,
        ep,
        "Window",
        surface,
        geometry_profile
    )
}

# DOOR -> FenestrationSurface:Detailed.
door__convert <- function(
    dest,
    ep,
    surface = NULL,
    geometry_profile = eplus_geom__profile(ep$version())
) {
    if (!db_has_rows(dest, "DOOR")) {
        return(NULL)
    }

    source <- door__source_table(dest)
    subsurface__convert(
        source,
        dest,
        ep,
        "Door",
        surface,
        geometry_profile
    )
}
