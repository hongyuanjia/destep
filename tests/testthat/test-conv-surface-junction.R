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

test_that("convex source faces retain protected collinear junctions", {
    # A perpendicular face meets a rectangle at its straight-through vertex.
    # The shared vertex is retained, but does not force either face into parts.
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
    expect_true(any(
        converted$ID == 1L & converted$POINT_X == 1 & converted$POINT_Y == 0
    ))
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

test_that("non-repeated rooms receive local cleanup-aware junction repair", {
    surface <- surface_test__cleanup_room()
    original <- data.table::copy(surface)
    before <- surface[, .(area = geom__polygon_area(.SD)), by = ID]
    repaired <- surface__preserve_boundaries(surface)
    expect_length(surface__energyplus_unclosed_rooms(repaired), 0L)
    expect_gt(
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
