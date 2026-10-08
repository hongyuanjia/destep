# Small orthogonal polygons expose removable junctions and staged reflex cuts.
partition_test__polygon <- function(x, y) {
    data.table::data.table(
        POINT_X = x,
        POINT_Y = y,
        POINT_Z = 0.0,
        POINT_NO = seq_along(x) - 1L
    )
}

test_that("direct partitions preserve strict corners and source area", {
    cases <- list(
        rectangle = partition_test__polygon(c(0, 3, 3, 0), c(0, 0, 2, 2)),
        junction = partition_test__polygon(c(0, 1, 3, 3, 0), c(0, 0, 0, 2, 2)),
        paired = partition_test__polygon(
            c(0, 1, 2, 3, 3, 2, 1, 0),
            c(0, 0, 0, 0, 2, 2, 2, 2)
        ),
        concave = partition_test__polygon(
            c(0, 3, 3, 1, 1, 0),
            c(0, 0, 1, 1, 3, 3)
        ),
        notched = partition_test__polygon(
            c(0, 4, 4, 3, 3, 1, 1, 0),
            c(0, 0, 4, 4, 1, 1, 4, 4)
        )
    )
    expected <- c(rectangle = 1L, junction = 2L, paired = 3L, concave = 2L)
    for (name in names(cases)) {
        for (reverse in c(FALSE, TRUE)) {
            input <- data.table::copy(cases[[name]])
            if (reverse) {
                input <- input[nrow(input):1L]
            }
            original <- data.table::copy(input)
            value <- surface__partition_by_diagonals(input)
            expect_false(is.null(value), info = name)
            if (is.null(value)) {
                next
            }
            expect_identical(input, original)
            area <- value[, .(area = geom__polygon_area(.SD)), by = PART]
            expect_equal(
                sum(area$area),
                geom__polygon_area(input),
                tolerance = 1e-10
            )
            expect_true(all(
                value[, !any(surface__redundant_vertices(.SD)), by = PART]$V1
            ))
            expect_true(all(
                value[, geom__polygon_is_convex(.SD, 1e-6), by = PART]$V1
            ))
            expect_equal(
                nrow(unique(value[, .(POINT_X, POINT_Y, POINT_Z)])),
                nrow(input)
            )
            if (name %in% names(expected)) {
                expect_equal(data.table::uniqueN(value$PART), expected[[name]])
            }
        }
    }
})

test_that("unsupported direct partitions select the existing fallback", {
    input <- partition_test__polygon(c(0, 3, 3, 0), c(0, 0, 2, 2))
    expect_null(surface__partition_by_diagonals(input[0L]))
    expect_null(surface__partition_by_diagonals(input[1:2]))
    missing <- data.table::copy(input)
    data.table::set(missing, j = "POINT_X", value = NA_real_)
    expect_null(surface__partition_by_diagonals(missing))
    data.table::set(missing, i = 1L, j = "POINT_X", value = 0.0)
    expect_null(surface__partition_by_diagonals(missing))
    expect_null(surface__partition_by_diagonals(input[c(1L, 1L, 2:4)]))
    expect_null(surface__partition_by_diagonals(input[rep(1:4, 33L)]))
    expect_null(surface__partition_by_diagonals(input, input))
    nonplanar <- data.table::copy(input)
    data.table::set(nonplanar, i = 1L, j = "POINT_Z", value = 1.0)
    expect_null(surface__partition_by_diagonals(nonplanar))
    # Openings must retain the established near-corner clearance policy.
    opening <- partition_test__polygon(c(1, 2, 2, 1), c(0.5, 0.5, 1.5, 1.5))
    expect_equal(
        surface__partition_polygon(input, opening),
        surface__triangulate_polygon(input, opening)
    )
})
