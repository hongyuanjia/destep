test_that("can convert 'BuildingSurface:Detailed'", {
    skip_on_cran()

    ep <- ensure_empty_idf()
    dest <- ensure_dest_sqlite_file(TRUE)
    conv__update_names(dest)

    # can convert 'BuildingSurface:Detailed'
    expect_type(surface <- surface__convert(dest, ep), "list")
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

    simplified <- surface__simplify_polygon(polygon)

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

test_that("EnergyPlus geometry assumptions are versioned in one profile", {
    reference <- expect_silent(eplus_geom__profile("23.1"))
    expect_true(reference$validated)
    expect_equal(reference$coordinate_distance, 0.01)
    expect_equal(reference$closure_vertex_distance, 0.0127)

    expect_warning(
        later <- eplus_geom__profile("24.1"),
        "validated against EnergyPlus 23.1"
    )
    expect_false(later$validated)
    expect_equal(later$reference_version, numeric_version("23.1"))
})

test_that("adjacent triangles merge into one convex surface", {
    # Two triangles are an auxiliary mesh of one rectangular DeST face. Their
    # common diagonal may be deleted without changing its boundary or area.
    surface <- data.table::rbindlist(list(
        data.table::data.table(
            PART = 1L, POINT_NO = 0:2,
            POINT_X = c(0, 4, 4), POINT_Y = c(0, 0, 3), POINT_Z = 0
        ),
        data.table::data.table(
            PART = 2L, POINT_NO = 0:2,
            POINT_X = c(0, 4, 0), POINT_Y = c(0, 3, 3), POINT_Z = 0
        )
    ))

    merged <- surface__merge_convex_parts(surface)

    expect_equal(data.table::uniqueN(merged$PART), 1L)
    expect_equal(nrow(merged), 4L)
    following <- seq_len(nrow(merged)) %% nrow(merged) + 1L
    expect_equal(abs(sum(
        merged$POINT_X * merged$POINT_Y[following] -
            merged$POINT_X[following] * merged$POINT_Y
    )) / 2.0, 12.0)
})

test_that("convex-part merging rejects a non-planar polygon", {
    # The two triangles are individually planar, but deleting their shared
    # diagonal would create a quadrilateral with one vertex 5 mm off plane.
    surface <- data.table::rbindlist(list(
        data.table::data.table(
            PART = 1L, POINT_NO = 0:2,
            POINT_X = c(0, 1, 1), POINT_Y = c(0, 0, 1),
            POINT_Z = c(0, 0, 0.005)
        ),
        data.table::data.table(
            PART = 2L, POINT_NO = 0:2,
            POINT_X = c(0, 1, 0), POINT_Y = c(0, 1, 1),
            POINT_Z = c(0, 0.005, 0)
        )
    ))

    merged <- surface__merge_convex_parts(surface)

    expect_equal(data.table::uniqueN(merged$PART), 2L)
})

test_that("convex-part merging is independent of part traversal order", {
    # This concave polygon has several legal local merges. A stable merge policy
    # must not emit a different number of surfaces when part rows are reordered.
    polygon <- data.table::data.table(
        VERTEX = 1:10,
        POINT_NO = 0:9,
        POINT_X = c(
            0.989748214576005, 0.585767730908300, -0.816434295808996,
            -2.094515643110917, -2.547365040702336, 1.300509053897897,
            1.800024425395703, 1.145867268715982, 2.272045004267075,
            1.060572752934251
        ),
        POINT_Y = c(
            0.406676496443011, 1.837457261328847, 2.070688628786034,
            1.486738100706347, -2.118904161038813, -1.350773412036306,
            -1.142463925382292, -0.553086176099802, -0.853681349296615,
            -0.197666698611673
        ),
        POINT_Z = 0
    )
    profile <- eplus_geom__profile()
    profile$coordinate_distance <- 1e-5
    triangle <- surface__triangulate_polygon(polygon, profile = profile)
    reorder_parts <- function(order) {
        data.table::rbindlist(lapply(order, function(part) {
            triangle[PART == part]
        }))
    }

    first <- surface__merge_convex_parts(
        reorder_parts(c(5L, 7L, 6L, 1L, 3L, 2L, 4L, 8L)),
        distance_tolerance = 1e-5
    )
    second <- surface__merge_convex_parts(
        reorder_parts(c(3L, 5L, 2L, 1L, 6L, 4L, 8L, 7L)),
        distance_tolerance = 1e-5
    )

    expect_equal(
        data.table::uniqueN(first$PART),
        data.table::uniqueN(second$PART)
    )
})

test_that("EnergyPlus-compatible closure accepts a harmless T-junction", {
    # One wall splits the roof's long edge at x=1. EnergyPlus inserts that
    # collinear point during its second closure pass, so no exported split is
    # required. Moving the point off the edge creates a genuine open shell.
    face <- function(name, type, coordinate) {
        data.table::data.table(
            ROOM = "Room", OUTPUT_ID = name, TYPE = type,
            POINT_NO = seq_len(nrow(coordinate)) - 1L,
            POINT_X = coordinate[, 1L], POINT_Y = coordinate[, 2L],
            POINT_Z = coordinate[, 3L]
        )
    }
    shell <- data.table::rbindlist(list(
        face("Floor", "Floor", rbind(
            c(0, 0, 0), c(0, 1, 0), c(2, 1, 0), c(2, 0, 0)
        )),
        face("Roof", "Roof", rbind(
            c(0, 0, 1), c(2, 0, 1), c(2, 1, 1), c(0, 1, 1)
        )),
        face("Front", "Wall", rbind(
            c(0, 0, 0), c(2, 0, 0), c(2, 0, 1),
            c(1, 0, 1), c(0, 0, 1)
        )),
        face("Back", "Wall", rbind(
            c(2, 1, 0), c(0, 1, 0), c(0, 1, 1), c(2, 1, 1)
        )),
        face("Left", "Wall", rbind(
            c(0, 1, 0), c(0, 0, 0), c(0, 0, 1), c(0, 1, 1)
        )),
        face("Right", "Wall", rbind(
            c(2, 0, 0), c(2, 1, 0), c(2, 1, 1), c(2, 0, 1)
        ))
    ))

    expect_length(surface__energyplus_unclosed_rooms(shell), 0L)
    open_shell <- data.table::copy(shell)
    open_shell[OUTPUT_ID == "Front" & POINT_NO == 3L, POINT_Z := 0.95]
    expect_equal(surface__energyplus_unclosed_rooms(open_shell), "Room")
})

test_that("surface coordinates follow EnergyPlus's vertex tolerance", {
    point <- data.table::data.table(
        ID = rep(1:2, each = 2L),
        POINT_NO = rep(0:1, 2L),
        POINT_X = c(188.175, 190.192, 188.183, 190.192),
        POINT_Y = c(-16.750, -14.488, -16.742, -14.480),
        POINT_Z = 2.9
    )

    snapped <- surface__snap_coordinates(point)
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
    expect_equal(nrow(surface__simplify_polygon(sliver)), 0L)
})

# Calculate one polygon's unit normal and area for geometry-invariant tests.
surface__metrics <- function(surface) {
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

    split <- window__split_by_surface(window, host)
    metric <- split[, surface__metrics(.SD), by = "OUTPUT_PART_ID"]

    expect_equal(data.table::uniqueN(split$OUTPUT_PART_ID), 2L)
    expect_true(all(metric$N_VERTEX == 3L))
    expect_equal(sum(metric$AREA), 4.0, tolerance = 1e-8)
    expect_setequal(unique(split$SURFACE_NAME), c("Wall [1]", "Wall [2]"))

    outside <- data.table::copy(window)
    outside[, POINT_X := POINT_X + 10]
    expect_error(
        window__split_by_surface(outside, host),
        "Could not place DeST window"
    )
})

test_that("window-aware host triangulation avoids sub-centimetre slivers", {
    host <- data.table::data.table(
        POINT_NO = 0:4,
        POINT_X = c(99.324, 99.421, 99.421, 81.774, 81.774),
        POINT_Y = 9.075,
        POINT_Z = c(9.0, 9.0, 13.5, 13.5, 9.0)
    )
    window <- data.table::data.table(
        POINT_NO = 0:3,
        POINT_X = c(85.604, 95.591, 95.591, 85.604),
        POINT_Y = 9.075,
        POINT_Z = c(9.977, 9.977, 12.523, 12.523)
    )

    triangulated <- surface__triangulate_polygon(host, window)
    clipped <- lapply(unique(triangulated$PART), function(part) {
        window__clip_polygon(window, triangulated[PART == part])
    })
    clipped <- Filter(Negate(is.null), clipped)

    host_metric <- triangulated[, surface__metrics(.SD), by = "PART"]
    clipped_area <- sum(vapply(
        clipped, function(value) surface__metrics(value)$AREA, numeric(1L)
    ))
    expect_equal(sum(host_metric$AREA), surface__metrics(host)$AREA, tolerance = 1e-8)
    expect_equal(clipped_area, surface__metrics(window)$AREA, tolerance = 1e-6)
})

test_that("window-clear triangulation search has a deterministic state cap", {
    polygon <- data.table::data.table(
        POINT_NO = 0:5,
        POINT_X = c(0, 3, 3, 1, 1, 0),
        POINT_Y = c(0, 0, 1, 1, 3, 3),
        POINT_Z = 0
    )
    avoid <- data.table::data.table(
        POINT_X = 0.5, POINT_Y = 0.5, POINT_Z = 0
    )
    profile <- eplus_geom__profile()
    profile$triangulation_max_states <- 0L

    triangle <- surface__triangulate_polygon(polygon, avoid, profile)

    expect_equal(data.table::uniqueN(triangle$PART), nrow(polygon) - 2L)
    expect_equal(
        sum(triangle[, surface__metrics(.SD)$AREA, by = "PART"]$V1),
        surface__metrics(polygon)$AREA,
        tolerance = 1e-8
    )
})

test_that("typical-storey overlap preserves an already convex polygon", {
    # Build one reciprocal floor/ceiling pair whose common footprint is a
    # rectangle. The typical-storey transformation should retain that exact
    # four-vertex polygon instead of introducing two unnecessary triangles.
    make_face <- function(
        id, name, type, z, azimuth, side, construction, peer
    ) {
        data.table::data.table(
            ID = id, PLANE = id, NAME = name, ORIGINAL_NAME = name,
            KIND_ENCLOSURE = 5L, TYPE_SURFACE = 0L, TYPE = type,
            SIDE = side, CONSTRUCTION = construction, ROOM = "Room",
            BOUNDARY = "Surface", BOUNDARY_OBJECT = peer,
            STOREY_ID = 1L, STOREY_NAME = "Typical",
            STOREY_MULTIPLIER = 5L, AZIMUTH = azimuth, TILT = 0.0,
            OUTPUT_ID = sprintf("%d-1", id), PART = 1L, PART_COUNT = 1L,
            POINT_NO = 0:3, POINT_X = c(0, 4, 4, 0),
            POINT_Y = c(0, 0, 3, 3), POINT_Z = z
        )
    }
    surface <- data.table::rbindlist(list(
        make_face(
            1L, "Floor", "Floor", 0.0, 999.0, 1L,
            "Slab [Reverse]", "Ceiling"
        ),
        make_face(
            2L, "Ceiling", "Ceiling", 3.0, -999.0, 2L,
            "Slab", "Floor"
        )
    ))

    converted <- surface__apply_typical_storey_boundaries(surface)
    object <- converted[, .(
        N_VERTEX = .N,
        AREA = surface__metrics(.SD)$AREA,
        BOUNDARY_OBJECT = BOUNDARY_OBJECT[[1L]],
        BOUNDARY_MODE = BOUNDARY_MODE[[1L]]
    ), by = .(OUTPUT_ID, NAME)]

    expect_equal(object$N_VERTEX, c(4L, 4L))
    expect_equal(object$AREA, c(12.0, 12.0))
    expect_equal(object$BOUNDARY_MODE, rep("typical_cycle", 2L))
    expect_equal(object$BOUNDARY_OBJECT, c("Ceiling", "Floor"))
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
    conv__update_names(dest)

    surface <- attr(surface__convert(dest, ensure_empty_idf()), "table")
    # Keep the fixture close to DeST's 560 SURFACE rows while allowing the
    # separate room-side objects required by EnergyPlus interzone boundaries.
    expect_lte(data.table::uniqueN(surface$OUTPUT_ID), 700L)
    metric <- surface[, surface__metrics(.SD), by = .(
        OUTPUT_ID, ID, PLANE, NAME, ORIGINAL_NAME, TYPE, KIND_ENCLOSURE,
        BOUNDARY, BOUNDARY_OBJECT, AZIMUTH, TILT, ROOM, BOUNDARY_MODE,
        SOURCE_TYPE, SOURCE_BOUNDARY, STOREY_MULTIPLIER, TYPICAL_PAIR_ID
    )]
    metric[, EXPECTED := list(list(geom__expected_surface_normal(
        AZIMUTH, TILT, geom__south_direction(dest)
    ))), by = "OUTPUT_ID"]
    metric[, ALIGNMENT := NX * vapply(EXPECTED, `[[`, numeric(1L), 1L) +
        NY * vapply(EXPECTED, `[[`, numeric(1L), 2L) +
        NZ * vapply(EXPECTED, `[[`, numeric(1L), 3L)]
    expect_true(all(metric$ALIGNMENT > 1.0 - 1e-6))

    original <- unique(metric, by = "ID")
    # Trace metadata preserves DeST's source classification even when a middle
    # storey's roof or exposed floor becomes part of the cyclic typical layer.
    expect_equal(data.table::uniqueN(metric[
        SOURCE_TYPE == "Roof" & SOURCE_BOUNDARY == "Outdoors"
    ]$ID), 18L)
    expect_equal(data.table::uniqueN(metric[
        SOURCE_TYPE == "Floor" & SOURCE_BOUNDARY == "Outdoors"
    ]$ID), 2L)
    expect_equal(data.table::uniqueN(metric[
        TYPE == "Roof" & BOUNDARY == "Outdoors"
    ]$ID), 11L)
    expect_equal(data.table::uniqueN(metric[
        TYPE == "Floor" & BOUNDARY == "Outdoors"
    ]$ID), 0L)
    expect_true(all(original[KIND_ENCLOSURE == 6L]$NZ < 0.0))

    self <- metric[BOUNDARY == "Surface" & NAME == BOUNDARY_OBJECT]
    expect_true(nrow(self) > 0L)
    expect_true(all(self$BOUNDARY_MODE == "typical_cut_adiabatic"))

    paired <- metric[BOUNDARY == "Surface" & NAME != BOUNDARY_OBJECT]
    peer <- match(paired$BOUNDARY_OBJECT, metric$NAME)
    expect_false(anyNA(peer))
    expect_equal(metric$BOUNDARY_OBJECT[peer], paired$NAME)
    expect_equal(paired$N_VERTEX, metric$N_VERTEX[peer])
    expect_true(all(
        paired$NX * metric$NX[peer] + paired$NY * metric$NY[peer] +
            paired$NZ * metric$NZ[peer] < -1.0 + 1e-6
    ))
    # EnergyPlus applies zone multipliers after solving the representative
    # zones, so every non-adiabatic peer pair must conserve A * multiplier.
    expect_equal(
        paired$AREA * paired$STOREY_MULTIPLIER,
        metric$AREA[peer] * metric$STOREY_MULTIPLIER[peer],
        tolerance = 1e-8
    )
    typical <- paired[BOUNDARY_MODE == "typical_cycle"]
    expect_true(nrow(typical) > 0L)
    expect_true(all(typical$STOREY_MULTIPLIER == 5L))
    expect_true(all(!is.na(typical$TYPICAL_PAIR_ID)))

    # Match EnergyPlus's own two-pass closure test instead of requiring every
    # harmless T-junction to be physically inserted into exported polygons.
    expect_length(surface__energyplus_unclosed_rooms(surface), 0L)

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
        .(SOURCE_AREA = surface__metrics(.SD)$AREA), by = "PLANE"]
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
                "possibly coincident", "collinear",
                "InterZone Surface Areas do not match as expected",
                "Base surface does not surround subsurface",
                "Distance between two vertices < \\.01",
                "same materials in the reverse order"
            ),
            collapse = "|"
        ),
        errors$message,
        ignore.case = TRUE
    )))
})
