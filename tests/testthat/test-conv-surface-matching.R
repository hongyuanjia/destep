# A rectangular repeated-floor fixture isolates matching from source boundary
# interpretation. Its floor/ceiling construction and multiplier are explicit.
surface_test__matching_pair <- function() {
    face <- function(id, z, direction) {
        data.table::data.table(
            ID = id,
            PLANE = id,
            NAME = paste0("Face", id),
            ORIGINAL_NAME = paste0("Face", id),
            KIND_ENCLOSURE = 5L,
            TYPE_SURFACE = 0L,
            TYPE = if (direction == 999) "Floor" else "Ceiling",
            SIDE = if (direction == 999) 1L else 2L,
            CONSTRUCTION = if (direction == 999) "Slab [Reverse]" else "Slab",
            ROOM = paste0("Room", id),
            BOUNDARY = "Surface",
            BOUNDARY_OBJECT = paste0("Face", 3L - id),
            STOREY_ID = id,
            STOREY_NAME = "Typical",
            STOREY_MULTIPLIER = if (id == 1L) 5L else 1L,
            AZIMUTH = direction,
            TILT = 0,
            OUTPUT_ID = paste0(id, "-1"),
            PART = 1L,
            PART_COUNT = 1L,
            POINT_NO = 0:3,
            POINT_X = c(0, 4, 4, 0),
            POINT_Y = c(0, 0, 3, 3),
            POINT_Z = z
        )
    }
    data.table::rbindlist(list(face(1L, 0, 999), face(2L, 0, -999)))
}

test_that("source peers and exterior boundaries survive unequal multipliers", {
    value <- surface_test__matching_pair()
    original <- data.table::copy(value)
    result <- surface__preserve_boundaries(value)
    expect_identical(value, original)
    expect_identical(result$BOUNDARY_OBJECT, value$BOUNDARY_OBJECT)
    expect_identical(result$CONSTRUCTION, value$CONSTRUCTION)
    expect_equal(nrow(result), nrow(value))
    expect_true(all(result$BOUNDARY_MODE == "source"))
    for (boundary in c("Outdoors", "Ground")) {
        exterior <- data.table::copy(value)
        data.table::set(exterior, j = "BOUNDARY", value = boundary)
        data.table::set(exterior, j = "BOUNDARY_OBJECT", value = NA_character_)
        converted <- surface__preserve_boundaries(exterior)
        expect_identical(converted$BOUNDARY, exterior$BOUNDARY)
        expect_identical(converted$CONSTRUCTION, exterior$CONSTRUCTION)
        expect_true(all(is.na(converted$BOUNDARY_OBJECT)))
    }
})

test_that("multiplier diagnostics count source pairs once across parts", {
    value <- surface_test__matching_pair()
    original <- data.table::copy(value)
    diagnostic <- surface__multiplier_pairs(value)
    expect_equal(nrow(diagnostic), 1L)
    expect_equal(diagnostic$source_surface, 1L)
    expect_equal(diagnostic$peer_source_surface, 2L)
    expect_equal(diagnostic$multiplier, 5L)
    expect_equal(diagnostic$peer_multiplier, 1L)
    # Parts retain the original source ID but have reciprocal output names.
    parts <- data.table::copy(value)
    data.table::set(parts, j = "NAME", value = paste0(parts$NAME, " Part 2"))
    data.table::set(
        parts,
        j = "BOUNDARY_OBJECT",
        value = paste0(parts$BOUNDARY_OBJECT, " Part 2")
    )
    expect_identical(
        surface__multiplier_pairs(data.table::rbindlist(list(value, parts))),
        diagnostic
    )
    expect_identical(value, original)
    data.table::set(value, j = "STOREY_MULTIPLIER", value = 1L)
    expect_equal(nrow(surface__multiplier_pairs(value)), 0L)
    expect_equal(nrow(surface__multiplier_pairs(value[0])), 0L)
})

test_that("broken source peer references are rejected", {
    value <- surface_test__matching_pair()
    data.table::set(value, i = 1:4, j = "BOUNDARY_OBJECT", value = "Missing")
    expect_error(
        surface__preserve_boundaries(value),
        "references must exist and be reciprocal"
    )
    data.table::set(
        value,
        i = 1:4,
        j = "BOUNDARY_OBJECT",
        value = NA_character_
    )
    expect_error(
        surface__preserve_boundaries(value),
        "references must exist and be reciprocal"
    )
})
