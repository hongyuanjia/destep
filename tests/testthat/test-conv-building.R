test_that("can convert 'Building'", {
    skip_on_cran()

    ep <- eplusr::empty_idf(23.1)
    eplusr::use_idd(23.1, "auto")

    path <- ensure_dest_test_file()
    dest <- read_dest(path, verbose = TRUE)

    # can convert 'Building'
    expect_type(bld <- building__convert(dest, ep), "list")
    expect_named(bld, c("object", "value"))
    expect_s3_class(attr(bld, "table"), "data.table")
    # can specify which building to extract
    expect_error(building__convert(dest, ep, TRUE), "integer or character")
    expect_equal(building__convert(dest, ep, 1), building__convert(dest, ep, "国管局1#"))
})

test_that("DeST south direction maps to EnergyPlus north axis", {
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    DBI::dbWriteTable(dest, "ENVIRONMENT", data.frame(SOUTH_DIRECTION = 270))

    expect_equal(geom__north_axis(dest), 0)
    DBI::dbExecute(dest, "UPDATE ENVIRONMENT SET SOUTH_DIRECTION = 0")
    expect_equal(geom__north_axis(dest), 90)
    expect_equal(
        geom__expected_surface_normal(0, 90, 0),
        c(-1, 0, 0),
        tolerance = 1e-12
    )
})
