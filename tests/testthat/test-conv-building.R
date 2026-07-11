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

test_that("DeST south direction maps to EnergyPlus north axis", {
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    DBI::dbWriteTable(dest, "ENVIRONMENT", data.frame(SOUTH_DIRECTION = 270))

    expect_equal(destep_north_axis(dest), 0)
    DBI::dbExecute(dest, "UPDATE ENVIRONMENT SET SOUTH_DIRECTION = 0")
    expect_equal(destep_north_axis(dest), 90)
    expect_equal(
        destep_expected_surface_normal(0, 90, 0),
        c(-1, 0, 0),
        tolerance = 1e-12
    )
})
