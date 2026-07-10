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
