test_that("HVAC source units are converted to SI", {
    expect_equal(
        hvac__flow_m3_h_to_m3_s(c(0, 3600, 11380)),
        c(0, 1, 11380 / 3600)
    )
    expect_equal(hvac__capacity_kw_to_w(c(0, 1, 93.8)), c(0, 1000, 93800))
    expect_equal(hvac__length_mm_to_m(c(0, 500, 1000)), c(0, 0.5, 1))
})

test_that("real DeST HVAC equipment relations are resolved", {
    skip_on_cran()

    model_path <- destep_test_fixture_file(
        "example.accdb",
        "DESTEP_TEST_HVAC_ACCDB"
    )
    equipment_path <- destep_test_fixture_file(
        "devlib.accdb",
        "DESTEP_TEST_EQUIPMENT_ACCDB"
    )
    if (!file.exists(model_path) || !file.exists(equipment_path)) {
        skip(paste(
            "Real DeST HVAC fixtures are not available.",
            "Set DESTEP_TEST_HVAC_ACCDB and DESTEP_TEST_EQUIPMENT_ACCDB."
        ))
    }

    # Read only the source tables required by the relation resolver.
    model <- read_dest(
        model_path,
        tables = c("AC_SYS", "AHU", "DUCTNET", "FAN", "LIB_CURVE")
    )
    on.exit(DBI::dbDisconnect(model), add = TRUE)
    equipment <- read_dest(
        equipment_path,
        tables = c("_Coil_Cooling", "Esp1CCoil")
    )
    on.exit(DBI::dbDisconnect(equipment), add = TRUE)

    result <- hvac__read_source_equipment(model, equipment)
    expect_true(all(vapply(result, data.table::is.data.table, logical(1))))

    expect_equal(nrow(result$systems), 3L)
    expect_true(all(result$systems$ac_system_type == 3L))
    expect_equal(length(unique(result$systems$ahu_id)), 3L)
    expect_equal(length(unique(result$systems$duct_network_id)), 3L)
    expect_setequal(result$systems$ahu_id, c(17675L, 17677L, 17679L))
    expect_setequal(
        result$systems$duct_network_id,
        c(24039L, 24040L, 24041L)
    )

    expect_equal(nrow(result$fan_links), 12L)
    expect_equal(
        nrow(result$fan_links[resolution_status == "model_record"]),
        6L
    )
    expect_equal(
        nrow(result$fan_links[resolution_status == "automatic"]),
        6L
    )
    expect_equal(nrow(result$fans), 6L)
    expect_true(all(result$fans$rated_flow_m3_s == 10000 / 3600))
    expect_true(all(result$fans$pressure_rise_pa == 700))
    expect_equal(
        result$fans$rated_efficiency,
        rep(0.7, 6L),
        tolerance = 1e-7
    )
    expect_true(all(result$fans$pressure_curve_id == 101L))
    expect_true(all(result$fans$efficiency_curve_id == 102L))

    expect_setequal(result$curves$curve_id, c(101L, 102L))
    expect_true(all(result$curves$coefficient_count == 3L))
    expect_equal(
        result$curves[curve_id == 101L, coefficient_a],
        1.6252,
        tolerance = 1e-6
    )
    expect_equal(
        result$curves[curve_id == 102L, coefficient_a],
        0.40089,
        tolerance = 1e-6
    )

    expect_equal(nrow(result$cooling_coils), 1L)
    expect_equal(result$cooling_coils$cooling_coil_id, 14L)
    expect_equal(result$cooling_coils$product_name, "JW20-4")
    expect_equal(result$cooling_coils$row_count, 6L)
    expect_equal(
        result$cooling_coils$rated_capacity_w,
        93800,
        tolerance = 0.01
    )
    expect_equal(
        result$cooling_coils$rated_air_flow_m3_s,
        11380 / 3600,
        tolerance = 1e-8
    )
    expect_true(result$cooling_coils$specific_parameters_match)

    expect_true(all(result$systems$ahu_rated_air_flow_m3_h == 11380))
    expect_true(all(result$systems$cooling_coil_air_flow_matches))
    expect_false(any(
        result$systems$ahu_rated_air_flow_m3_h %in% result$fans$fan_id
    ))

})
