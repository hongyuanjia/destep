# Create a minimal DeST environment fixture for ground-reflectance conversion.
ground_reflectance_test__db <- function(value = 0.3, rows = 1L) {
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    if (length(value) == 1L) value <- rep(value, rows)
    DBI::dbWriteTable(dest, "ENVIRONMENT", data.table::data.table(
        ENVIRONMENT_ID = seq_len(rows),
        GROUND_REFLECT_COEF = value
    ))
    dest
}

# Extract the twelve generated reflectance values in EnergyPlus IDD order.
ground_reflectance_test__values <- function(out) {
    as.numeric(out$value$value_num[
        out$value$class_name == "Site:GroundReflectance" &
            grepl("Ground Reflectance", out$value$field_name, fixed = TRUE)
    ])
}

test_that("converts DeST ground reflectance to all EnergyPlus months", {
    ep <- eplusr::empty_idf(23.1)
    dest <- ground_reflectance_test__db(0.3)
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    ground <- ground_reflectance__convert(dest, ep)

    expect_equal(unique(ground$object$class_name), "Site:GroundReflectance")
    expect_equal(ground_reflectance_test__values(ground), rep(0.3, 12L))
    expect_equal(attr(ground, "table")$GROUND_REFLECTANCE, 0.3)
})

test_that("supports EnergyPlus 9.0.1 ground reflectance fields", {
    skip_if_not("9.0.1" %in% eplusr::avail_eplus())
    dest <- ground_reflectance_test__db(0.3)
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    ground <- ground_reflectance__convert(
        dest, eplusr::empty_idf("9.0.1")
    )

    expect_equal(ground_reflectance_test__values(ground), rep(0.3, 12L))
})

test_that("skips unavailable DeST ground reflectance", {
    ep <- eplusr::empty_idf(23.1)

    missing_table <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(missing_table), add = TRUE)
    expect_null(ground_reflectance__convert(missing_table, ep))

    missing_field <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(missing_field), add = TRUE)
    DBI::dbWriteTable(missing_field, "ENVIRONMENT", data.table::data.table(
        ENVIRONMENT_ID = 1L
    ))
    expect_null(ground_reflectance__convert(missing_field, ep))

    missing_value <- ground_reflectance_test__db(NA_real_)
    on.exit(DBI::dbDisconnect(missing_value), add = TRUE)
    expect_null(ground_reflectance__convert(missing_value, ep))
})

test_that("rejects ambiguous or invalid DeST ground reflectance", {
    ep <- eplusr::empty_idf(23.1)

    multiple <- ground_reflectance_test__db(c(0.2, 0.3), rows = 2L)
    on.exit(DBI::dbDisconnect(multiple), add = TRUE)
    expect_error(
        ground_reflectance__convert(multiple, ep),
        "Expected one ENVIRONMENT row"
    )

    for (value in c(-0.01, 1.01, Inf)) {
        invalid <- ground_reflectance_test__db(value)
        expect_error(
            ground_reflectance__convert(invalid, ep),
            "must be within \\[0, 1\\]"
        )
        DBI::dbDisconnect(invalid)
    }
})
