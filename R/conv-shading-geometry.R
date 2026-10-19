# Read active window faces and their stored shade polygons in one query. The
# library dimensions alone do not establish that DeST rebuilt these polygons.
shading__stored_geometry <- function(dest) {
    required <- list(
        WINDOW = c("ID", "SIDE1", "SIDE2", "SC", "SHADINGID"),
        SURFACE = c("SURFACE_ID", "TYPE", "OF_SHADING", "GEOMETRY"),
        GEOMETRY = c("GEOMETRY_ID", "BOUNDARY_LOOP_ID"),
        LOOP_POINT = c("LOOP_ID", "POINT_NO", "POINT"),
        POINT = c("POINT_ID", "X", "Y", "Z")
    )
    if (
        !all(names(required) %in% DBI::dbListTables(dest)) ||
            !all(vapply(
                names(required),
                function(table) {
                    db_has_fields(dest, table, required[[table]])
                },
                logical(1L)
            ))
    ) {
        stop(
            "Active DeST shading requires stored window and shading geometry.",
            call. = FALSE
        )
    }
    DBI::dbGetQuery(
        dest,
        "
        WITH FACES AS (
            SELECT W.ID AS WINDOW_ID, S.SURFACE_ID, S.GEOMETRY,
                   'window' AS KIND
            FROM WINDOW W JOIN SURFACE S
            ON S.SURFACE_ID IN (W.SIDE1, W.SIDE2) AND S.TYPE = 1
            WHERE W.SC > 0
            UNION ALL
            SELECT W.ID AS WINDOW_ID, S.SURFACE_ID, S.GEOMETRY,
                   'shade' AS KIND
            FROM WINDOW W JOIN SURFACE S ON S.OF_SHADING = W.SHADINGID
            WHERE W.SC > 0
        )
        SELECT F.WINDOW_ID, F.SURFACE_ID, F.KIND, L.POINT_NO,
               P.X AS POINT_X, P.Y AS POINT_Y, P.Z AS POINT_Z
        FROM FACES F
        LEFT JOIN GEOMETRY G ON F.GEOMETRY = G.GEOMETRY_ID
        LEFT JOIN LOOP_POINT L ON G.BOUNDARY_LOOP_ID = L.LOOP_ID
        LEFT JOIN POINT P ON L.POINT = P.POINT_ID
        ORDER BY F.WINDOW_ID, F.KIND, F.SURFACE_ID, L.POINT_NO
    "
    )
}

# Require a one-to-one match after moving the source exterior window frame to
# the target middle-plane frame. This tests relative shade geometry; it does
# not assert that moving the whole window frame preserves solar response.
shading__check_stored_geometry <- function(
    window,
    parameter,
    objects,
    source,
    profile
) {
    source <- source[source$WINDOW_ID == parameter$WINDOW_ID, , drop = FALSE]
    # All missing or inconsistent source polygons stop conversion with the
    # affected window name and the same required source-rebuild action.
    fail <- function() {
        stop(
            sprintf(
                paste(
                    "Stored DeST shading geometry does not match the active",
                    "library parameters for window '%s'. Rebuild the source",
                    "shading geometry in DeST and verify it before conversion."
                ),
                parameter$WINDOW_NAME
            ),
            call. = FALSE
        )
    }
    if (nrow(source) == 0L || anyNA(source)) {
        fail()
    }
    coordinate_names <- c("POINT_X", "POINT_Y", "POINT_Z")
    if (!all(is.finite(as.matrix(source[coordinate_names])))) {
        fail()
    }
    source_window <- source[source$KIND == "window", , drop = FALSE]
    if (
        length(unique(source_window$SURFACE_ID)) != 1L ||
            anyDuplicated(source_window$POINT_NO)
    ) {
        fail()
    }
    original <- as.matrix(source_window[coordinate_names])
    converted <- geom__coordinates(window)
    if (nrow(original) != nrow(converted) || nrow(original) != 4L) {
        fail()
    }
    shift <- colMeans(converted) - colMeans(original)

    # Window coordinates are rounded to 1 mm upstream. Comparing relative
    # vertices can accumulate at most 1 mm from two independently rounded
    # coordinates; the geometric intersection tolerance covers floating error.
    tolerance <- 0.001 + profile$intersection
    # Vertex order can differ between source and target. Require one match
    # per row and column so coincident or ambiguously close vertices fail.
    same_vertices <- function(left, right) {
        if (nrow(left) != nrow(right)) {
            return(FALSE)
        }
        distance <- pmax(
            abs(outer(left[, 1L], right[, 1L], "-")),
            abs(outer(left[, 2L], right[, 2L], "-")),
            abs(outer(left[, 3L], right[, 3L], "-"))
        )
        near <- distance <= tolerance
        all(rowSums(near) == 1L) && all(colSums(near) == 1L)
    }
    if (!same_vertices(sweep(original, 2L, shift, "+"), converted)) {
        fail()
    }
    shades <- source[source$KIND == "shade", , drop = FALSE]
    ids <- unique(shades$SURFACE_ID)
    if (length(ids) != length(objects) || length(ids) == 0L) {
        fail()
    }
    used <- rep(FALSE, length(objects))
    target <- lapply(objects, function(object) {
        matrix(
            as.double(unlist(object[-seq_len(4L)], use.names = FALSE)),
            ncol = 3L,
            byrow = TRUE
        )
    })
    # The supported family contains at most three panels per window. Keeping
    # each polygon intact avoids introducing any subdivision or new surfaces.
    for (id in ids) {
        rows <- shades[shades$SURFACE_ID == id, , drop = FALSE]
        if (anyDuplicated(rows$POINT_NO) || nrow(rows) != 4L) {
            fail()
        }
        points <- sweep(as.matrix(rows[coordinate_names]), 2L, shift, "+")
        frame <- geom__polygon_frame(points, profile$normal_magnitude)
        if (!frame$valid) {
            fail()
        }
        matched <- which(vapply(
            target,
            function(candidate) {
                same_vertices(points, candidate)
            },
            logical(1L)
        ))
        if (length(matched) != 1L || used[[matched]]) {
            fail()
        }
        used[[matched]] <- TRUE
    }
    if (!all(used)) {
        fail()
    }
    invisible(NULL)
}
