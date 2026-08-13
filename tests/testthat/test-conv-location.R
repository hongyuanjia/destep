test_that("can convert 'Location'", {
    skip_on_cran()

    eplusr::use_idd(23.1, "auto")
    ep <- eplusr::empty_idf(23.1)

    path <- ensure_dest_test_file()
    dest <- read_dest(path, verbose = TRUE)

    expect_type(loc <- location__convert(dest, ep), "list")
    expect_named(loc, c("object", "value"))
    expect_s3_class(attr(loc, "table"), "data.table")
    expect_equal(loc$value$value_chr[[1L]], "DefaultEnvironment")
})

test_that("preserves site precision and decodes the DeST standard meridian", {
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    DBI::dbWriteTable(dest, "ENVIRONMENT", data.table::data.table(
        ENVIRONMENT_ID = 5L,
        NAME = "Denver Intl AP",
        LATITUDE = 39.833,
        LONGITUDE = -104.65,
        ELEVATION = 1650,
        PROPERTY = 25500L
    ))
    ep <- eplusr::empty_idf(23.1)

    location <- location__convert(dest, ep)$value

    expect_equal(location[field_name == "Latitude", value_num], 39.833)
    expect_equal(location[field_name == "Longitude", value_num], -104.65)
    expect_equal(location[field_name == "Time Zone", value_num], -7)
    expect_equal(location[field_name == "Elevation", value_num], 1650)
})
