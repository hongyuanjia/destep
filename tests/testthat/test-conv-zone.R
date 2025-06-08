test_that("can convert 'Zone'", {
    skip_on_cran()

    ep <- ensure_empty_idf()
    dest <- ensure_dest_sqlite_file(TRUE)
    destep_update_name(dest)

    # can convert 'Zone'
    expect_type(zone <- destep_conv_zone(dest, ep), "list")
    expect_named(zone, c("object", "value"))
    expect_equal(unique(zone$object$class_name), "Zone")
    expect_s3_class(attr(zone, "table"), "data.table")
})
