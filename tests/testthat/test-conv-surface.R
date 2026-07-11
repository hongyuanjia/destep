test_that("can convert 'BuildingSurface:Detailed'", {
    skip_on_cran()

    ep <- ensure_empty_idf()
    dest <- ensure_dest_sqlite_file(TRUE)
    destep_update_name(dest)

    # can convert 'BuildingSurface:Detailed'
    expect_type(surface <- destep_conv_surface(dest, ep), "list")
    expect_named(surface, c("object", "value"))
    expect_equal(unique(surface$object$class_name), "BuildingSurface:Detailed")
    expect_s3_class(attr(surface, "table"), "data.table")
})

test_that("surface polygon simplification removes only redundant vertices", {
    polygon <- data.table::data.table(
        POINT_NO = 0:5,
        POINT_X = c(0, 1, 2, 2, 1, 0),
        POINT_Y = c(0, 0, 0, 1, 1, 1),
        POINT_Z = 0
    )

    simplified <- destep_simplify_surface_polygon(polygon)

    expect_equal(nrow(simplified), 4L)
    expect_equal(simplified$POINT_NO, 0:3)
    expect_equal(
        simplified[, .(POINT_X, POINT_Y, POINT_Z)],
        data.table::data.table(
            POINT_X = c(0, 2, 2, 0),
            POINT_Y = c(0, 0, 1, 1),
            POINT_Z = 0
        )
    )
})

test_that("surface coordinates follow EnergyPlus's vertex tolerance", {
    point <- data.table::data.table(
        ID = rep(1:2, each = 2L),
        POINT_NO = rep(0:1, 2L),
        POINT_X = c(188.175, 190.192, 188.183, 190.192),
        POINT_Y = c(-16.750, -14.488, -16.742, -14.480),
        POINT_Z = 2.9
    )

    snapped <- destep_snap_surface_coordinates(point)
    expect_equal(snapped[1L, .(POINT_X, POINT_Y, POINT_Z)],
        snapped[3L, .(POINT_X, POINT_Y, POINT_Z)])
    expect_equal(snapped[2L, .(POINT_X, POINT_Y, POINT_Z)],
        snapped[4L, .(POINT_X, POINT_Y, POINT_Z)])

    sliver <- data.table::data.table(
        POINT_NO = 0:2,
        POINT_X = c(1712.530, 1712.531, 1712.531),
        POINT_Y = c(-1088.336, -1088.334, -1041.716),
        POINT_Z = -10.2
    )
    expect_equal(nrow(destep_simplify_surface_polygon(sliver)), 0L)
})

# Calculate one polygon's unit normal and area for geometry-invariant tests.
surface_metrics <- function(surface) {
    following <- seq_len(nrow(surface)) %% nrow(surface) + 1L
    normal <- c(
        sum((surface$POINT_Y - surface$POINT_Y[following]) *
            (surface$POINT_Z + surface$POINT_Z[following])),
        sum((surface$POINT_Z - surface$POINT_Z[following]) *
            (surface$POINT_X + surface$POINT_X[following])),
        sum((surface$POINT_X - surface$POINT_X[following]) *
            (surface$POINT_Y + surface$POINT_Y[following]))
    )
    magnitude <- sqrt(sum(normal ^ 2))
    list(NX = normal[[1L]] / magnitude, NY = normal[[2L]] / magnitude,
        NZ = normal[[3L]] / magnitude, AREA = magnitude / 2.0,
        N_VERTEX = nrow(surface))
}

test_that("windows are clipped to exact host surface parts", {
    host <- data.table::rbindlist(list(
        data.table::data.table(
            ID = 20L, PART = 1L, NAME = "Wall [1]", POINT_NO = 0:2,
            POINT_X = c(0, 4, 4), POINT_Y = 0, POINT_Z = c(0, 0, 4)
        ),
        data.table::data.table(
            ID = 20L, PART = 2L, NAME = "Wall [2]", POINT_NO = 0:2,
            POINT_X = c(0, 4, 0), POINT_Y = 0, POINT_Z = c(0, 4, 4)
        )
    ))
    window <- data.table::data.table(
        OUTPUT_ID = "200-1", ID = 200L, SURFACE_ID = 20L,
        ORIGINAL_NAME = "Window", NAME = "Window", SIDE = 1L,
        INTERZONE = FALSE, BOUNDARY_OBJECT = NA_character_, POINT_NO = 0:3,
        POINT_X = c(1, 3, 3, 1), POINT_Y = 0, POINT_Z = c(1, 1, 3, 3)
    )

    split <- destep_split_window_by_surface(window, host)
    metric <- split[, surface_metrics(.SD), by = "OUTPUT_PART_ID"]

    expect_equal(data.table::uniqueN(split$OUTPUT_PART_ID), 2L)
    expect_true(all(metric$N_VERTEX == 3L))
    expect_equal(sum(metric$AREA), 4.0, tolerance = 1e-8)
    expect_setequal(unique(split$SURFACE_NAME), c("Wall [1]", "Wall [2]"))

    outside <- data.table::copy(window)
    outside[, POINT_X := POINT_X + 10]
    expect_error(
        destep_split_window_by_surface(outside, host),
        "Could not place DeST window"
    )
})

test_that("real DeST surfaces preserve orientation, adjacency, area, and closure", {
    skip_on_cran()

    src <- ensure_dest_sqlite_file()
    on.exit(DBI::dbDisconnect(src), add = TRUE)
    path_tmp <- tempfile(fileext = ".sql")
    dest <- DBI::dbConnect(RSQLite::SQLite(), path_tmp)
    on.exit({
        DBI::dbDisconnect(dest)
        unlink(path_tmp)
    }, add = TRUE)
    RSQLite::sqliteCopyDatabase(src, dest)
    destep_update_name(dest)

    surface <- attr(destep_conv_surface(dest, ensure_empty_idf()), "table")
    metric <- surface[, surface_metrics(.SD), by = .(
        OUTPUT_ID, ID, PLANE, NAME, ORIGINAL_NAME, TYPE, KIND_ENCLOSURE,
        BOUNDARY, BOUNDARY_OBJECT, AZIMUTH, TILT, ROOM
    )]
    metric[, EXPECTED := list(list(destep_expected_surface_normal(
        AZIMUTH, TILT, destep_south_direction(dest)
    ))), by = "OUTPUT_ID"]
    metric[, ALIGNMENT := NX * vapply(EXPECTED, `[[`, numeric(1L), 1L) +
        NY * vapply(EXPECTED, `[[`, numeric(1L), 2L) +
        NZ * vapply(EXPECTED, `[[`, numeric(1L), 3L)]
    expect_true(all(metric$ALIGNMENT > 1.0 - 1e-6))

    original <- unique(metric, by = "ID")
    expect_equal(nrow(original[KIND_ENCLOSURE == 3L & TYPE == "Roof" & BOUNDARY == "Outdoors"]), 18L)
    expect_equal(nrow(original[KIND_ENCLOSURE == 6L & TYPE == "Floor" & BOUNDARY == "Outdoors"]), 2L)
    expect_true(all(original[KIND_ENCLOSURE == 6L]$NZ < 0.0))

    paired <- metric[BOUNDARY == "Surface"]
    peer <- match(paired$BOUNDARY_OBJECT, metric$NAME)
    expect_false(anyNA(peer))
    expect_false(any(paired$NAME == paired$BOUNDARY_OBJECT))
    expect_equal(metric$BOUNDARY_OBJECT[peer], paired$NAME)
    expect_equal(paired$N_VERTEX, metric$N_VERTEX[peer])
    expect_true(all(
        paired$NX * metric$NX[peer] + paired$NY * metric$NY[peer] +
            paired$NZ * metric$NZ[peer] < -1.0 + 1e-6
    ))

    edge <- surface[, {
        following <- seq_len(.N) %% .N + 1L
        start <- sprintf("%.3f|%.3f|%.3f", POINT_X, POINT_Y, POINT_Z)
        end <- sprintf(
            "%.3f|%.3f|%.3f",
            POINT_X[following], POINT_Y[following], POINT_Z[following]
        )
        list(EDGE = ifelse(
            start < end, paste(start, end, sep = "/"), paste(end, start, sep = "/")
        ))
    }, by = .(OUTPUT_ID, ROOM)]
    expect_true(all(edge[, .N, by = .(ROOM, EDGE)]$N == 2L))

    raw <- data.table::as.data.table(DBI::dbGetQuery(dest, "
        SELECT P.PLANE_ID AS PLANE, L.POINT_NO,
            ROUND(PT.X, 3) AS POINT_X, ROUND(PT.Y, 3) AS POINT_Y,
            ROUND(PT.Z, 3) AS POINT_Z
        FROM PLANE P
        INNER JOIN GEOMETRY G ON P.GEOMETRY = G.GEOMETRY_ID
        INNER JOIN LOOP_POINT L ON G.BOUNDARY_LOOP_ID = L.LOOP_ID
        INNER JOIN POINT PT ON L.POINT = PT.POINT_ID
        ORDER BY P.PLANE_ID, L.POINT_NO
    "))
    raw_area <- raw[PLANE %in% surface$PLANE,
        .(SOURCE_AREA = surface_metrics(.SD)$AREA), by = "PLANE"]
    converted_area <- metric[, .(CONVERTED_AREA = sum(AREA)), by = .(ID, PLANE)]
    area <- merge(converted_area, raw_area, by = "PLANE")
    expect_lt(max(abs(area$CONVERTED_AREA - area$SOURCE_AREA)), 1e-6)
})

test_that("converted real geometry passes EnergyPlus detailed diagnostics", {
    skip_on_cran()

    dest <- ensure_dest_sqlite_file()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    expect_warning(
        idf <- to_eplus(dest, 23.1),
        "Skipped 9 ROOM row\\(s\\)"
    )
    idf$add("Output:Diagnostics" := list(key_1 = "DisplayExtraWarnings"))
    idf$save(tempfile(fileext = ".idf"))
    job <- idf$run(
        eplusr::path_eplus_weather(
            23.1,
            "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
        ),
        dir = tempdir()
    )
    errors <- job$errors()

    expect_false(any(errors$level %in% c("Severe", "Fatal")))
    expect_false(any(grepl(
        paste(
            c(
                "not fully enclosed", "vertex size mismatch",
                "invalid Building Surface Name", "floor area.*differs",
                "zone volume.*differs", "degenerate", "non-?convex",
                "possibly coincident", "collinear"
            ),
            collapse = "|"
        ),
        errors$message,
        ignore.case = TRUE
    )))
})
