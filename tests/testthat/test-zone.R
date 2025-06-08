test_that("can convert 'Zone'", {
    skip_on_cran()

    ep <- eplusr::empty_idf(23.1)
    eplusr::use_idd(23.1, "auto")

    path <- ensure_dest_test_file()
    dest <- read_dest(path, verbose = TRUE)

    # can convert 'Zone', 'ZoneList', 'ZoneGroup'
    expect_type(zn <- destep_conv_zone(dest, ep), "list")
    expect_named(zn, c("object", "value"))
    expect_equal(unique(zn$object$class_name), c("Zone", "ZoneList", "ZoneGroup"))
    expect_s3_class(attr(zn, "table"), "data.table")
})
