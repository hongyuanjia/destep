# TODO: handle window shading
# TODO: handle window type
# WINDOW -> FenestrationSurface:Detailed
destep_conv_window <- function(dest, ep) {
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
            CASE
                WHEN S1.TYPE NOT IN (1, 2) THEN S1.NAME
                WHEN S2.TYPE NOT IN (1, 2) THEN S2.NAME
                ELSE COALESCE(S1.NAME, S2.NAME)
            END            AS SURFACE_NAME,
            CASE
                WHEN S1.TYPE NOT IN (1, 2) THEN 1
                WHEN S2.TYPE NOT IN (1, 2) THEN 2
                ELSE 1
            END            AS SIDE,
            CASE
                WHEN S1.TYPE NOT IN (1, 2) THEN S1.AZIMUTH
                ELSE S2.AZIMUTH
            END            AS AZIMUTH,
            CASE
                WHEN S1.TYPE NOT IN (1, 2) THEN S1.TILT
                ELSE S2.TILT
            END            AS TILT,
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

    assert_unique_name(window$NAME[window$POINT_NO == 0L], "window")
    data.table::setDT(window)

    # Use the same DeST direction metadata as the parent surface so a window
    # cannot silently face into its zone when enclosure side ordering varies.
    south_direction <- destep_south_direction(dest)
    window <- window[, destep_orient_surface_polygon(.SD, south_direction), by = "ID"]

    value <- window[,
        by = "ID",
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
                outside_boundary_condition_object = NULL,
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
