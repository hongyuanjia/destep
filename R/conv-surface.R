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
            E.SURFACE_ID                                   AS ID,
            E.PLANE                                        AS PLANE,
            S.NAME                                         AS NAME,
            E.KIND                                         AS KIND_ENCLOSURE,
            S.TYPE                                         AS TYPE_SURFACE,
            CASE
                WHEN E.KIND = 1 OR E.KIND = 2 THEN 'Wall'
                WHEN E.KIND = 3 OR E.KIND = 6 THEN 'Roof'
                WHEN E.KIND = 4               THEN 'Floor'
                WHEN E.KIND = 5               THEN 'Ceiling'
            END                                            AS TYPE,
            E.SIDE                                         AS SIDE,
            E.CONSTRUCTION                                 AS CONSTRUCTION,
            COALESCE(ROOM.NAME, OUTSIDE.NAME, GROUND.NAME) AS ROOM,
            CASE
                WHEN E.KIND = 1 OR S.TYPE = 1 THEN 'Outdoors'
                WHEN E.KIND = 4 OR S.TYPE = 2 THEN 'Ground'
                ELSE 'Surface'
            END                                            AS BOUNDARY,
            L.POINT_NO                                     AS POINT_NO,
            ROUND(P.X, 3)                                  AS POINT_X,
            ROUND(P.Y, 3)                                  AS POINT_Y,
            ROUND(P.Z, 3)                                  AS POINT_Z
        FROM (
            SELECT
                SURFACE_ID, S.KIND, SIDE, C.CNAME AS CONSTRUCTION, PLANE
            FROM (
                -- get both side surfaces
                SELECT SIDE1 AS SURFACE_ID, KIND, 1 AS SIDE, CONSTRUCTION, MIDDLE_PLANE AS PLANE FROM MAIN_ENCLOSURE
                UNION
                SELECT SIDE2 AS SURFACE_ID, KIND, 2 AS SIDE, CONSTRUCTION, MIDDLE_PLANE AS PLANE FROM MAIN_ENCLOSURE
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

    # find the adjacent surfaces
    surface[BOUNDARY == "Surface", by = "PLANE", BOUNDARY_OBJECT := rev(NAME)]
    # TODO: reverse the order of the vertices for the adjacent surface
    # If 'MAIN_ENCLOSURE$KIND' = 5, it means that this enclosure can either be a
    # ceiling or floor. The actual surface type is determined by the
    # 'SURFACE$TYPE'. If 'SURFACE$TYPE' = 4, this is a floor. The coordinates
    # should be reversed. If 'SURFACE$TYPE' = 5, this is a ceiling.
    surface[KIND_ENCLOSURE == 5L & TYPE_SURFACE == 4L, by = "ID", `:=`(
        TYPE = "Floor",
        POINT_X = rev(POINT_X), POINT_Y = rev(POINT_Y), POINT_Z = rev(POINT_Z)
    )]
    surface[KIND_ENCLOSURE == 5L & TYPE_SURFACE == 5L, TYPE := "Ceiling"]
    # remove the surface indicating outside environment and grounds
    surface <- surface[!J(c(1L, 2L)), on = "TYPE_SURFACE"]

    # for walls, reverse the order of the vertices if the side is 2
    surface[TYPE == "Wall" & SIDE == 2L, by = "ID", `:=`(
        POINT_X = rev(POINT_X), POINT_Y = rev(POINT_Y), POINT_Z = rev(POINT_Z)
    )]
    # use newall vector to determine the correct side of the surface for ceilings and floors
    # Reference: https://www.khronos.org/opengl/wiki/Calculating_a_Surface_Normal#Newell.27s_Method
    ids_ceiling <- surface[TYPE %in% c("Ceiling", "Roof"), by = "ID", {
        nx <- seq_len(.N) %% .N + 1L
        list(z = sum((POINT_Y + POINT_Y[nx]) * (POINT_X - POINT_X[nx])))
    }][z < 0.0, ID]
    ids_floor <- surface[TYPE %in% c("Floor", "Ground"), by = "ID", {
        nx <- seq_len(.N) %% .N + 1L
        list(z = sum((POINT_Y + POINT_Y[nx]) * (POINT_X - POINT_X[nx])))
    }][z > 0.0, ID]
    ids <- c(ids_ceiling, ids_floor)
    if (length(ids) > 0L) {
        surface[J(ids), on = "ID", by = "ID", `:=`(
            POINT_X = rev(POINT_X), POINT_Y = rev(POINT_Y), POINT_Z = rev(POINT_Z)
        )]
    }

    # Use one minimal vertex sequence for each polygon. EnergyPlus simplifies
    # adjacent polygons internally; leaving redundant collinear points can make
    # the two otherwise identical sides end up with different vertex counts.
    surface <- surface[, destep_simplify_surface_polygon(.SD), by = "ID"]

    # TODO: how does DeST handle the case when the surface is both a floor and a ceiling?
    # TODO: how does EnergyPlus handle "empty floor slab"?

    value <- surface[,
        by = "ID",
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
                sun_exposure = NULL,
                # 09: Wind Exposure
                wind_exposure = NULL,
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

# Remove redundant vertices from one ordered DeST surface polygon while
# retaining turns and at least the three vertices needed for a valid face.
destep_simplify_surface_polygon <- function(surface, tolerance = 1e-8) {
    surface <- data.table::copy(surface)

    repeat {
        n_vertex <- nrow(surface)
        if (n_vertex <= 3L) break

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
        # A vertex is redundant only when the path continues straight through
        # it. A collinear reversal is retained because it changes the polygon.
        redundant <- incoming_length <= tolerance |
            outgoing_length <= tolerance |
            (
                sqrt(rowSums(cross_product ^ 2)) <=
                    tolerance * pmax(1, incoming_length * outgoing_length) &
                rowSums(incoming * outgoing) > 0
            )

        if (!any(redundant) || n_vertex - sum(redundant) < 3L) break
        surface <- surface[!redundant]
    }

    data.table::set(surface, NULL, "POINT_NO", seq_len(nrow(surface)) - 1L)
    surface
}
