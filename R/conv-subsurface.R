# Clip one planar window polygon against one convex host part. Intersections are
# interpolated in 3-D so the result stays on the original DeST middle plane even
# when the stable clipping projection omits a non-constant coordinate.
destep_clip_window_polygon <- function(
    window, host, tolerance = 1e-10, distance_tolerance = 0.01
) {
    normal <- destep_surface_normal(host)
    projection <- setdiff(1:3, which.max(abs(normal)))
    host_xy <- as.matrix(host[, .(POINT_X, POINT_Y, POINT_Z)])[
        , projection, drop = FALSE
    ]
    host_area <- sum(
        host_xy[, 1L] * host_xy[c(2:nrow(host_xy), 1L), 2L] -
            host_xy[c(2:nrow(host_xy), 1L), 1L] * host_xy[, 2L]
    ) / 2.0
    if (host_area < 0.0) host_xy <- host_xy[nrow(host_xy):1L, , drop = FALSE]

    xyz <- as.matrix(window[, .(POINT_X, POINT_Y, POINT_Z)])
    value <- cbind(xyz, xyz[, projection, drop = FALSE])
    cross_2d <- function(a, b, point) {
        (b[[1L]] - a[[1L]]) * (point[[2L]] - a[[2L]]) -
            (b[[2L]] - a[[2L]]) * (point[[1L]] - a[[1L]])
    }
    intersection <- function(start, end, clip_start, clip_end) {
        edge <- clip_end - clip_start
        segment <- end[4:5] - start[4:5]
        denominator <- edge[[1L]] * segment[[2L]] - edge[[2L]] * segment[[1L]]
        if (abs(denominator) <= tolerance) return(start)
        relative <- start[4:5] - clip_start
        position <- -(
            edge[[1L]] * relative[[2L]] - edge[[2L]] * relative[[1L]]
        ) / denominator
        start + position * (end - start)
    }

    # Sutherland-Hodgman clipping is valid here because every host part passed
    # by surface conversion is either one convex face or one triangle.
    host_next <- c(seq.int(2L, nrow(host_xy)), 1L)
    for (edge_index in seq_len(nrow(host_xy))) {
        if (nrow(value) == 0L) break
        clip_start <- host_xy[edge_index, ]
        clip_end <- host_xy[host_next[[edge_index]], ]
        input <- value
        value <- matrix(numeric(), nrow = 0L, ncol = 5L)
        previous <- input[nrow(input), ]
        previous_inside <- cross_2d(clip_start, clip_end, previous[4:5]) >= -tolerance
        for (index in seq_len(nrow(input))) {
            current <- input[index, ]
            current_inside <- cross_2d(
                clip_start, clip_end, current[4:5]
            ) >= -tolerance
            if (current_inside) {
                if (!previous_inside) {
                    value <- rbind(value, intersection(
                        previous, current, clip_start, clip_end
                    ))
                }
                value <- rbind(value, current)
            } else if (previous_inside) {
                value <- rbind(value, intersection(
                    previous, current, clip_start, clip_end
                ))
            }
            previous <- current
            previous_inside <- current_inside
        }
    }
    if (nrow(value) < 3L) return(NULL)

    # Remove numerical duplicates introduced when a window corner lies exactly
    # on a host diagonal. Sub-centimetre slivers cannot survive EnergyPlus input
    # processing and are rejected instead of becoming degenerate windows.
    repeat {
        following <- seq_len(nrow(value)) %% nrow(value) + 1L
        short <- which(sqrt(rowSums(
            (value[, 1:3, drop = FALSE] - value[following, 1:3, drop = FALSE]) ^ 2
        )) < distance_tolerance)
        if (length(short) == 0L) break
        if (nrow(value) <= 3L) return(NULL)
        value <- value[-following[short[[1L]]], , drop = FALSE]
    }

    out <- data.table::data.table(
        POINT_X = value[, 1L],
        POINT_Y = value[, 2L],
        POINT_Z = value[, 3L]
    )
    out[, POINT_NO := seq_len(.N) - 1L]
    out
}

# Split windows with the exact host parts produced by surface conversion. The
# same deterministic part number is used on both sides of an interzone opening,
# allowing reciprocal FenestrationSurface references after clipping.
destep_split_window_by_surface <- function(window, surface = NULL) {
    expected <- unique(window[, .(OUTPUT_ID, ORIGINAL_NAME)])
    if (is.null(surface)) {
        window[, PART := 1L]
    } else {
        coordinate_columns <- c("POINT_NO", "POINT_X", "POINT_Y", "POINT_Z")
        window <- window[, {
            source <- data.table::copy(.SD)
            host <- surface[ID == source$SURFACE_ID[[1L]]]
            if (nrow(host) == 0L) {
                stop(sprintf(
                    "Could not find the EnergyPlus host for DeST window '%s'.",
                    source$ORIGINAL_NAME[[1L]]
                ))
            }
            part <- lapply(unique(host$PART), function(part_id) {
                host_part <- host[PART == part_id]
                clipped <- destep_clip_window_polygon(source, host_part)
                if (is.null(clipped)) return(NULL)
                # EnergyPlus may replace a clipped four-sided non-rectangle by
                # an equivalent rectangle. Fan every clipped polygon into exact
                # triangles so neither area nor position is silently changed.
                clipped_part <- if (nrow(clipped) <= 3L) {
                    list(clipped)
                } else {
                    lapply(seq.int(2L, nrow(clipped) - 1L), function(index) {
                        value <- data.table::copy(clipped[c(1L, index, index + 1L)])
                        value[, POINT_NO := 0:2]
                        value
                    })
                }
                metadata <- source[1L,
                    setdiff(names(source), coordinate_columns), with = FALSE
                ]
                data.table::rbindlist(lapply(seq_along(clipped_part), function(index) {
                    value <- clipped_part[[index]]
                    part_metadata <- data.table::copy(metadata)
                    part_metadata[, `:=`(
                        PART = part_id,
                        SUBPART = index,
                        SURFACE_NAME = host_part$NAME[[1L]]
                    )]
                    cbind(part_metadata[rep(1L, nrow(value))], value)
                }))
            })
            data.table::rbindlist(part, fill = TRUE)
        }, by = "OUTPUT_ID"]
    }

    # A window completely outside its converted host indicates inconsistent
    # source geometry or an incorrect host mapping. Failing here prevents a
    # valid DeST opening from disappearing silently during clipping.
    missing <- expected[!window, on = "OUTPUT_ID"]
    if (nrow(missing) > 0L) {
        stop(sprintf(
            "Could not place DeST window '%s' on any converted host surface.",
            missing$ORIGINAL_NAME[[1L]]
        ))
    }

    if (!"SUBPART" %in% names(window)) window[, SUBPART := 1L]
    data.table::setorderv(window, c("OUTPUT_ID", "PART", "SUBPART", "POINT_NO"))
    window[, PIECE := data.table::rleid(PART, SUBPART), by = "OUTPUT_ID"]
    window[, PART_COUNT := data.table::uniqueN(PIECE), by = "OUTPUT_ID"]
    window[, NAME := data.table::fcase(
        INTERZONE & PART_COUNT > 1L,
            sprintf("%s [Side %d Part %d]", ORIGINAL_NAME, SIDE, PIECE),
        INTERZONE,
            sprintf("%s [%d]", ORIGINAL_NAME, SIDE),
        PART_COUNT > 1L,
            sprintf("%s [Part %d]", ORIGINAL_NAME, PIECE),
        default = ORIGINAL_NAME
    )]
    window[, BOUNDARY_OBJECT := data.table::fcase(
        INTERZONE & PART_COUNT > 1L,
            sprintf("%s [Side %d Part %d]", ORIGINAL_NAME, 3L - SIDE, PIECE),
        INTERZONE,
            sprintf("%s [%d]", ORIGINAL_NAME, 3L - SIDE),
        default = NA_character_
    )]
    window[, OUTPUT_PART_ID := sprintf("%s-%d", OUTPUT_ID, PIECE)]
    data.table::setorderv(window, c("OUTPUT_ID", "PIECE", "POINT_NO"))
    window
}

# TODO: handle window shading
# WINDOW -> FenestrationSurface:Detailed
destep_conv_window <- function(dest, ep, surface = NULL) {
    if (!destep_has_rows(dest, "WINDOW")) return(NULL)

    # TODO: Does DeST support polygon windows other than rectangles?
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
            S1.NAME        AS SIDE1_SURFACE_NAME,
            S1.TYPE        AS SIDE1_SURFACE_TYPE,
            S1.AZIMUTH     AS SIDE1_AZIMUTH,
            S1.TILT        AS SIDE1_TILT,
            E.SIDE2        AS SIDE2_SURFACE_ID,
            S2.NAME        AS SIDE2_SURFACE_NAME,
            S2.TYPE        AS SIDE2_SURFACE_TYPE,
            S2.AZIMUTH     AS SIDE2_AZIMUTH,
            S2.TILT        AS SIDE2_TILT,
            L.POINT_NO     AS POINT_NO,
            ROUND(P.X, 3)  AS POINT_X,
            ROUND(P.Y, 3)  AS POINT_Y,
            ROUND(P.Z, 3)  AS POINT_Z
        FROM WINDOW W
        LEFT JOIN MAIN_ENCLOSURE E
        ON W.OF_ENCLOSURE = E.ID
        LEFT JOIN SURFACE S1
        ON E.SIDE1 = S1.SURFACE_ID
        LEFT JOIN SURFACE S2
        ON E.SIDE2 = S2.SURFACE_ID
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
    window_type <- destep_window_type_performance(dest)
    valid_type <- window_type[TYPE_DATA_VALID == TRUE]
    if (nrow(valid_type) > 0L) {
        construction <- stats::setNames(
            valid_type$TYPE_CONSTRUCTION_NAME,
            valid_type$WINDOW_ID
        )
        matched <- window$ID %in% valid_type$WINDOW_ID
        window[matched, CONSTRUCTION := construction[as.character(ID)]]
    }

    # DeST stores one window polygon on an enclosure middle plane. An exterior
    # window belongs only to the room side, whereas an interzone window must be
    # emitted once for each room and the two EnergyPlus objects must reference
    # each other explicitly.
    side1 <- window[!SIDE1_SURFACE_TYPE %in% c(1L, 2L)]
    side2 <- window[!SIDE2_SURFACE_TYPE %in% c(1L, 2L)]
    if (nrow(side1) > 0L) {
        side1[, `:=`(
            ORIGINAL_NAME = NAME,
            SIDE = 1L,
            SURFACE_ID = SIDE1_SURFACE_ID,
            SURFACE_NAME = SIDE1_SURFACE_NAME,
            AZIMUTH = SIDE1_AZIMUTH,
            TILT = SIDE1_TILT,
            INTERZONE = !SIDE2_SURFACE_TYPE %in% c(1L, 2L),
            BOUNDARY_OBJECT = NA_character_
        )]
    }
    if (nrow(side2) > 0L) {
        side2[, `:=`(
            ORIGINAL_NAME = NAME,
            SIDE = 2L,
            SURFACE_ID = SIDE2_SURFACE_ID,
            SURFACE_NAME = SIDE2_SURFACE_NAME,
            AZIMUTH = SIDE2_AZIMUTH,
            TILT = SIDE2_TILT,
            INTERZONE = !SIDE1_SURFACE_TYPE %in% c(1L, 2L),
            BOUNDARY_OBJECT = NA_character_
        )]
    }
    window <- data.table::rbindlist(list(side1, side2), fill = TRUE)
    window[, OUTPUT_ID := sprintf("%s-%d", ID, SIDE)]
    data.table::setorderv(window, c("OUTPUT_ID", "POINT_NO"))
    # Use the same DeST direction metadata as the parent surface so a window
    # cannot silently face into its zone when enclosure side ordering varies.
    south_direction <- destep_south_direction(dest)
    window <- window[
        , destep_orient_surface_polygon(.SD, south_direction), by = "OUTPUT_ID"
    ]
    window <- destep_split_window_by_surface(window, surface)
    window <- window[
        , destep_orient_surface_polygon(.SD, south_direction), by = "OUTPUT_PART_ID"
    ]
    assert_unique_name(window$NAME[window$POINT_NO == 0L], "window")

    value <- window[,
        by = "OUTPUT_PART_ID",
        list(value = list(c(
            list(
                # 01: Name
                name = NAME[[1L]],
                # 02: Surface Type
                surface_type = "Window",
                # 03: Construction Name
                construction_name = CONSTRUCTION[[1L]],
                # 04: Building Surface Name
                building_surface_name = SURFACE_NAME[[1L]],
                # 05: Outside Boundary Condition Object
                outside_boundary_condition_object = if (!is.na(BOUNDARY_OBJECT[[1L]])) {
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
            as.list(as.double(vapply(POINT_NO + 1L,
                FUN.VALUE = double(3),
                function(ind) {
                    c(POINT_X[ind], POINT_Y[ind], POINT_Z[ind])
                }
            )))
        )))
    ]$value

    out <- eval(as.call(c(
        destep_add, dest, ep,
        lapply(value, function(val) bquote("FenestrationSurface:Detailed" := .(val)))
    )))

    attr(out, "table") <- window

    out
}
