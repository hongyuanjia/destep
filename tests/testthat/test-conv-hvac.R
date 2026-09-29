test_that("maps verified DeST outdoor-air control types", {
    expect_identical(hvac__economizer_type(1L), "NoEconomizer")
    expect_identical(hvac__economizer_type(5L), "DifferentialDryBulb")
    expect_identical(hvac__economizer_type(6L), "DifferentialEnthalpy")
    expect_error(hvac__economizer_type(2L), "DeST FRESH_AIR_TYPE")
})

test_that("selects fan parameters for each two-zone operating mode", {
    coefficients <- paste0("return_fan_power_coefficient_", seq_len(5L))

    expect_false(any(coefficients %in% hvac__required_options("two_zone_cav")))
    expect_true(all(coefficients %in% hvac__required_options("two_zone_vav")))
})

test_that("reconciles only small VAV minimum-flow closure gaps", {
    source <- c(0.09438998, 0.14155992)

    expect_warning(
        reconciled <- hvac__reconcile_minimum_supply_flows(source, 0.23597),
        class = "destep_normalized_hvac_minimum_flow"
    )
    expect_equal(sum(reconciled), 0.23597, tolerance = 1e-12)
    expect_equal(
        reconciled[[1L]] / reconciled[[2L]],
        source[[1L]] / source[[2L]]
    )

    expect_error(
        hvac__reconcile_minimum_supply_flows(source, 0.24),
        class = "destep_invalid_hvac_air_balance"
    )
})
