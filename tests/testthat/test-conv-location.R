test_that("can convert 'Location'", {
    skip_on_cran()

    eplusr::use_idd(23.1, "auto")
    ep <- eplusr::empty_idf(23.1)

    path <- ensure_dest_test_file()
    dest <- read_dest(path, verbose = TRUE)

    expect_type(loc <- destep_conv_location(dest, ep), "list")
    expect_named(loc, c("object", "value"))
    expect_s3_class(attr(loc, "table"), "data.table")
    expect_equal(loc$value$value_chr[[1L]], "DefaultEnvironment")
})
