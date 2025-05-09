# TODO: handle window shading
# TODO: handle window type
# WINDOW -> FenestrationSurface:Detailed
destep_conv_window <- function(dest, ep) {
    return(dest)
    # TODO: Does DeST support polygon windows other than rectangles?

    DBI::dbGetQuery(
        dest,
        "
            SELECT
                E.ENCLOSE_ID AS ENCLOSE_ID,
                S.SURFACE_ID AS SURFACE_ID,
                S.NAME       AS SURFACE_NAME
            FROM (
                -- get both side surfaces
                SELECT ID AS ENCLOSE_ID, SIDE1 AS SURFACE_ID FROM MAIN_ENCLOSURE
                UNION
                SELECT ID AS ENCLOSE_ID, SIDE2 AS SURFACE_ID FROM MAIN_ENCLOSURE
            ) E
            LEFT JOIN SURFACE S
            ON E.SURFACE_ID = S.SURFACE_ID
    "
    )

    DBI::dbListFields(dest, "SURFACE")
    DBI::dbGetQuery(dest, "SELECT * FROM SURFACE", n = 5)
    DBI::dbGetQuery(dest, "SELECT NAME FROM WINDOW", n = 5)

    DBI::dbGetQuery(
        dest,
        n = 5,
        "
        SELECT
            W.ID           AS ID,
            W.NAME         AS NAME,
            W.TYPE         AS TYPE,
            S.SURFACE_NAME AS SURFACE_NAME
        FROM WINDOW W
        LEFT JOIN (
            -- get surface name
            SELECT
                E.ENCLOSE_ID  AS ENCLOSE_ID,
                SR.SURFACE_ID AS SURFACE_ID,
                SR.NAME       AS SURFACE_NAME
            FROM (
                SELECT ID AS ENCLOSE_ID, SIDE1 AS SURFACE_ID FROM MAIN_ENCLOSURE
                UNION
                SELECT ID AS ENCLOSE_ID, SIDE2 AS SURFACE_ID FROM MAIN_ENCLOSURE
            ) E
            LEFT JOIN SURFACE SR
            ON E.SURFACE_ID = SR.SURFACE_ID
        ) S
        ON W.OF_ENCLOSURE = S.ENCLOSE_ID
        "
    )

    DBI::dbGetQuery(
        dest,
        n = 5,
        "
        SELECT
            W.ID           AS ID,
            W.NAME         AS NAME,
            W.TYPE         AS TYPE,
            S.SURFACE_NAME AS SURFACE_NAME
        FROM WINDOW W
        LEFT JOIN (
            -- get surface name
            SELECT
                E.ENCLOSE_ID  AS ENCLOSE_ID,
                SR.SURFACE_ID AS SURFACE_ID,
                SR.NAME       AS SURFACE_NAME
            FROM (
                -- get both side surfaces
                SELECT ID AS ENCLOSE_ID, SIDE1 AS SURFACE_ID FROM MAIN_ENCLOSURE
                UNION
                SELECT ID AS ENCLOSE_ID, SIDE2 AS SURFACE_ID FROM MAIN_ENCLOSURE
            ) E
            LEFT JOIN SURFACE SR
            ON E.SURFACE_ID = SR.SURFACE_ID
        ) S
        ON W.OF_ENCLOSURE = S.ENCLOSE_ID
        "
    )

    DBI::dbGetQuery(
        dest,
        n = 5,
        "
        SELECT
            W.ID           AS ID,
            W.NAME         AS NAME,
            W.TYPE         AS TYPE,
            SW.CNAME       AS CONSTRUCTION,
            S.SURFACE_NAME AS SURFACE_NAME,
            L.POINT_NO     AS POINT_NO,
            ROUND(P.X, 3)  AS POINT_X,
            ROUND(P.Y, 3)  AS POINT_Y,
            ROUND(P.Z, 3)  AS POINT_Z
        FROM WINDOW W
        LEFT JOIN (
            -- get surface name
            SELECT
                S.SURFACE_ID AS SURFACE_ID,
                S.NAME       AS SURFACE_NAME
            FROM (
                -- get both side surfaces
                SELECT SIDE1 AS SURFACE_ID, NAME AS SURFACE_NAME FROM MAIN_ENCLOSURE
                UNION
                SELECT SIDE2 AS SURFACE_ID, NAME AS SURFACE_NAME FROM MAIN_ENCLOSURE
            ) E
            LEFT JOIN SURFACE S
            ON E.SURFACE_ID = S.SURFACE_ID
        ) S
        ON W.OF_ENCLOSURE = S.SURFACE_ID
        LEFT JOIN SYS_WINDOW SW
        ON W.ID = SW.WINDOW_ID
        LEFT JOIN PLANE P
        ON W.MIDDLE_PLANE = P.PLANE_ID
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
}
