# Isolate the 55-face room that exposed a cleanup-stage enclosure failure in
# the 26.1 prototype diagnostic. The CSV contains geometry only; synthetic
# self-references make this a non-repeated room without external dependencies.
surface_test__cleanup_room <- function() {
    surface <- data.table::fread(testthat::test_path(
        "fixture",
        "cleanup-junction-room.csv"
    ))
    value <- list(
        ID = surface$FACE,
        PLANE = surface$FACE,
        NAME = paste0("Face ", surface$FACE),
        OUTPUT_ID = as.character(surface$FACE),
        ORIGINAL_NAME = paste0("Face ", surface$FACE),
        ROOM = "Cleanup room",
        TYPE_SURFACE = 0L,
        SIDE = 1L,
        CONSTRUCTION = "Slab",
        BOUNDARY = "Surface",
        BOUNDARY_OBJECT = paste0("Face ", surface$FACE),
        STOREY_ID = 1L,
        STOREY_MULTIPLIER = 1L,
        AZIMUTH = 0.0,
        TILT = 90.0,
        PART = 1L,
        PART_COUNT = 1L
    )
    for (name in names(value)) {
        data.table::set(surface, j = name, value = value[[name]])
    }
    surface
}

test_that("convex source faces stay whole across harmless straight junctions", {
    # A perpendicular face meets a rectangle at its straight-through vertex.
    # A redundant collinear point may be cleaned without splitting either face.
    surface <- data.table::data.table(
        ID = c(rep(1L, 5L), rep(2L, 4L)),
        PLANE = c(rep(1L, 5L), rep(2L, 4L)),
        NAME = c(rep("Base", 5L), rep("Incident", 4L)),
        TYPE_SURFACE = 0L,
        ROOM = "Room",
        BOUNDARY_OBJECT = NA_character_,
        POINT_NO = c(0:4, 0:3),
        POINT_X = c(0, 1, 2, 2, 0, 1, 1, 1, 1),
        POINT_Y = c(0, 0, 0, 1, 1, 0, 0, 1, 1),
        POINT_Z = c(rep(0, 5L), 0, 1, 1, 0)
    )
    original <- data.table::copy(surface)
    converted <- surface__normalize_topology(surface)
    expect_equal(data.table::uniqueN(converted$OUTPUT_ID), 2L)
    expect_true(all(converted$PART_COUNT == 1L))
    expect_equal(geom__polygon_area(converted[converted$ID == 1L, ]), 2)
    expect_equal(surface, original)
})

test_that("closure detects breaks exposed by removable vertex cleanup", {
    surface <- surface_test__cleanup_room()
    original <- data.table::copy(surface)
    expect_equal(surface__energyplus_unclosed_rooms(surface), "Cleanup room")
    expect_equal(surface, original)
    # A collapsed face must not be silently removed from the room inventory.
    collapsed <- surface[1:3]
    data.table::set(collapsed, j = "OUTPUT_ID", value = "Collapsed")
    data.table::set(collapsed, j = "POINT_X", value = c(0, 0.005, 0))
    data.table::set(collapsed, j = "POINT_Y", value = c(0, 0, 0.005))
    data.table::set(collapsed, j = "POINT_Z", value = 0.0)
    expect_equal(surface__energyplus_unclosed_rooms(collapsed), "Cleanup room")
    expect_length(surface__energyplus_unclosed_rooms(surface[0L]), 0L)
})

test_that("source-closed near-edge rooms are preserved with explicit diagnostics", {
    surface <- surface_test__cleanup_room()
    original <- data.table::copy(surface)
    before <- surface[, .(area = geom__polygon_area(.SD)), by = ID]
    expect_warning(
        repaired <- surface__preserve_boundaries(surface),
        class = "destep_geometry_closure_tolerance"
    )
    source_profile <- eplus_geom__profile()
    source_profile$closure_vertex_distance <- source_profile$plane_distance
    expect_length(
        surface__energyplus_unclosed_rooms(repaired, source_profile),
        0L
    )
    expect_equal(
        data.table::uniqueN(repaired$OUTPUT_ID),
        data.table::uniqueN(surface$OUTPUT_ID)
    )
    pieces <- repaired[,
        .(area = geom__polygon_area(.SD)),
        by = .(ID, OUTPUT_ID)
    ]
    after <- pieces[, .(area = sum(area)), by = ID]
    data.table::setorder(before, ID)
    data.table::setorder(after, ID)
    expect_equal(after, before, tolerance = 1e-8)
    expect_true(all(repaired$BOUNDARY == "Surface"))
    expect_true(all(repaired$BOUNDARY_OBJECT == repaired$NAME))
    expect_true(all(repaired$CONSTRUCTION == "Slab"))
    expect_true(all(repaired$STOREY_MULTIPLIER == 1L))
    expect_true(all(repaired$BOUNDARY_MODE == "source"))
    expect_equal(surface, original)
})

test_that("an independent open room does not pull source-closed rooms into repair", {
    closed <- surface_test__cleanup_room()
    open <- data.table::copy(closed[
        closed$OUTPUT_ID == closed$OUTPUT_ID[[1L]],
    ])
    data.table::set(open, j = "ROOM", value = "Actual gap")
    data.table::set(open, j = "OUTPUT_ID", value = "open-face")
    data.table::set(open, j = "NAME", value = "Open face")
    data.table::set(open, j = "BOUNDARY", value = "Outdoors")
    data.table::set(open, j = "BOUNDARY_OBJECT", value = NA_character_)
    visited <- character()
    split_edges <- surface__split_edges
    local_mocked_bindings(
        surface__split_edges = function(surface, all_coordinates, profile) {
            visited <<- c(visited, surface$ROOM[[1L]])
            split_edges(surface, all_coordinates, profile)
        },
        surface__local_cuts = function(face, profile) list()
    )
    profile <- eplus_geom__profile()
    profile$closure_vertex_distance <- profile$plane_distance
    expect_error(
        surface__repair_source_edges(
            rbind(closed, open),
            data.table::data.table(),
            profile
        ),
        "Actual gap"
    )
    expect_true(length(visited) > 0L)
    expect_true(all(visited == "Actual gap"))
})
