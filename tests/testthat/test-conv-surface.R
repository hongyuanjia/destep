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
