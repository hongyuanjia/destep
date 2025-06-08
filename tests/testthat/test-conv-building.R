test_that("can convert 'Building'", {
    skip_on_cran()

    ep <- eplusr::empty_idf(23.1)
    eplusr::use_idd(23.1, "auto")

    path <- ensure_dest_test_file()
    dest <- read_dest(path, verbose = TRUE)

    # can convert 'Building'
    expect_type(bld <- destep_conv_building(dest, ep), "list")
    expect_named(bld, c("object", "value"))
    expect_s3_class(attr(bld, "table"), "data.table")
    # can specify which building to extract
    expect_error(destep_conv_building(dest, ep, TRUE), "integer or character")
    expect_equal(destep_conv_building(dest, ep, 1), destep_conv_building(dest, ep, "国管局1#"))
})
