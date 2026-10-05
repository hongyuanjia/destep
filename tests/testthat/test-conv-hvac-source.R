test_that("HVAC source units are converted to SI", {
    expect_equal(
        hvac__flow_m3_h_to_m3_s(c(0, 3600, 11380)),
        c(0, 1, 11380 / 3600)
    )
    expect_equal(hvac__capacity_kw_to_w(c(0, 1, 93.8)), c(0, 1000, 93800))
    expect_equal(hvac__length_mm_to_m(c(0, 500, 1000)), c(0, 0.5, 1))
})

# This fixture reproduces the GUI-confirmed AE201 property layout, including
# legacy TYPE=0 on DATA_DOUBLE powers and a disconnected property chain.
hvac_test__property_model <- function() {
    model <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbWriteTable(
        model,
        "AHU",
        data.frame(
            AHU_ID = c(4898L, 9999L),
            EXT_PROPERTY = c(4903L, 9000L)
        )
    )
    DBI::dbWriteTable(
        model,
        "EXT_PROPERTY",
        data.frame(
            PROPERTY_ID = c(4903L, 4904L, 8280L, 8281L, 9000L),
            NEXT_PROPERTY = c(4904L, 8280L, 8281L, 0L, 9000L),
            NAME = c(
                "AHU_TWO_PIPE_WATER_SCH",
                "AHU_WATER_TYPE",
                "AHU_POWER_OF_SUPPLY_FAN",
                "AHU_POWER_OF_RETURN_FAN",
                "UNUSED"
            ),
            TYPE = rep(0L, 5L),
            DATA_LONG = c(52L, 1L, 0L, 0L, 0L),
            DATA_DOUBLE = c(0, 0, 0.201455, 0.100728, 0)
        )
    )
    model
}

test_that("AHU property chains preserve source controls and power units", {
    model <- hvac_test__property_model()
    on.exit(DBI::dbDisconnect(model))
    out <- hvac__read_ahu_properties(model, 4898L)
    expect_identical(out$property_id, c(4903L, 4904L, 8280L, 8281L))
    expect_identical(out$property_order, 1:4)
    expect_identical(out$declared_type, rep(0L, 4L))
    expect_equal(out$fan_power_w, c(NA, NA, 201.455, 100.728))
    expect_identical(
        out$schedule_id,
        c(52L, NA_integer_, NA_integer_, NA_integer_)
    )
    expect_equal(out$data_long[[2L]], 1)
    expect_equal(out$data_double[[3L]], 0.201455)
    expect_error(
        hvac__read_ahu_properties(model),
        class = "destep_unresolved_hvac_property"
    )
    expect_error(
        hvac__read_ahu_properties(model, 42L),
        class = "destep_unresolved_hvac_property"
    )
    expect_equal(nrow(hvac__read_ahu_properties(model, integer())), 0L)
})

test_that("AHU property chains reject broken reachable identities", {
    model <- hvac_test__property_model()
    on.exit(DBI::dbDisconnect(model))
    DBI::dbExecute(
        model,
        "UPDATE EXT_PROPERTY SET NEXT_PROPERTY=42 WHERE PROPERTY_ID=8281"
    )
    expect_error(
        hvac__read_ahu_properties(model, 4898L),
        class = "destep_unresolved_hvac_property"
    )
    DBI::dbExecute(
        model,
        "UPDATE EXT_PROPERTY SET NEXT_PROPERTY=0 WHERE PROPERTY_ID=8281"
    )
    DBI::dbExecute(
        model,
        "INSERT INTO EXT_PROPERTY SELECT * FROM EXT_PROPERTY WHERE PROPERTY_ID=4904"
    )
    expect_error(
        hvac__read_ahu_properties(model, 4898L),
        class = "destep_unresolved_hvac_property"
    )
    DBI::dbExecute(model, "DELETE FROM EXT_PROPERTY WHERE PROPERTY_ID=4904")
    expect_error(
        hvac__read_ahu_properties(model, 4898L),
        class = "destep_unresolved_hvac_property"
    )
})

test_that("absent AHU property roots remain absent", {
    model <- hvac_test__property_model()
    on.exit(DBI::dbDisconnect(model))
    DBI::dbExecute(model, "UPDATE AHU SET EXT_PROPERTY=NULL")
    expect_equal(nrow(hvac__read_ahu_properties(model)), 0L)
    DBI::dbExecute(model, "DROP TABLE EXT_PROPERTY")
    DBI::dbExecute(model, "UPDATE AHU SET EXT_PROPERTY=0")
    expect_equal(nrow(hvac__read_ahu_properties(model)), 0L)
    DBI::dbExecute(model, "UPDATE AHU SET EXT_PROPERTY=4903 WHERE AHU_ID=4898")
    expect_error(
        hvac__read_ahu_properties(model),
        class = "destep_invalid_hvac_source_schema"
    )
})

test_that("AHU property values retain explicit failure boundaries", {
    model <- hvac_test__property_model()
    on.exit(DBI::dbDisconnect(model))
    DBI::dbExecute(
        model,
        "UPDATE EXT_PROPERTY SET NEXT_PROPERTY=NULL WHERE PROPERTY_ID=8281"
    )
    expect_error(
        hvac__read_ahu_properties(model, 4898L),
        class = "destep_unresolved_hvac_property"
    )
    DBI::dbExecute(
        model,
        "UPDATE EXT_PROPERTY SET NEXT_PROPERTY=0, DATA_DOUBLE=-1 WHERE PROPERTY_ID=8281"
    )
    expect_error(
        hvac__read_ahu_properties(model, 4898L),
        "not >= 0"
    )
    DBI::dbExecute(
        model,
        "UPDATE EXT_PROPERTY SET DATA_DOUBLE=NULL WHERE PROPERTY_ID=8281"
    )
    expect_error(hvac__read_ahu_properties(model, 4898L), "missing")
    DBI::dbExecute(
        model,
        "UPDATE EXT_PROPERTY SET DATA_DOUBLE=0, NAME='AHU_POWER_OF_SUPPLY_FAN' WHERE PROPERTY_ID=8281"
    )
    expect_error(
        hvac__read_ahu_properties(model, 4898L),
        class = "destep_unresolved_hvac_property"
    )
})

# Check the linked references against a real Access model; this is a source
# extraction test, not evidence for a particular EnergyPlus plant topology.
test_that("AE201 AHU controls are read from the original Access input", {
    skip_on_cran()
    path <- Sys.getenv("DESTEP_TEST_HVAC_SINGLE_ACCDB", unset = "")
    if (!nzchar(path) || !file.exists(path)) {
        skip("The real AE201 DeST model is required.")
    }
    model <- read_dest(path, tables = c("AHU", "EXT_PROPERTY", "SCHEDULE_YEAR"))
    on.exit(DBI::dbDisconnect(model))
    out <- hvac__read_ahu_properties(model, 4898L)
    expect_identical(out$property_id, c(4903L, 4904L, 8280L, 8281L))
    expect_identical(out$schedule_id[[1L]], 52L)
    expect_equal(out$data_long[[2L]], 1)
    expect_equal(out$fan_power_w[3:4], c(201.455, 100.728))
    schedule <- DBI::dbGetQuery(
        model,
        "SELECT NAME, DATA FROM SCHEDULE_YEAR WHERE SCHEDULE_ID=52"
    )
    expect_identical(
        schedule__decode(schedule$DATA[[1L]], schedule$NAME[[1L]]),
        rep(60, 8760)
    )
})

test_that("HVAC representation follows effective source state", {
    expect_identical(destep_opts()$hvac, "auto")
    for (entry in list(
        list(state = "unconditioned", resolved = "none"),
        list(state = "load_only", resolved = "ideal_loads"),
        list(state = "system_defined", resolved = "physical")
    )) {
        expect_identical(
            hvac__resolve_representation("auto", list(state = entry$state)),
            entry$resolved
        )
    }
    expect_error(
        hvac__resolve_representation(
            "auto",
            list(state = "mixed_load_and_system")
        ),
        class = "destep_incomplete_hvac_source"
    )
    expect_error(
        hvac__resolve_representation("physical", list(state = "load_only")),
        class = "destep_incomplete_hvac_source"
    )
    expect_identical(
        hvac__resolve_representation("ideal_loads", list(state = "incomplete")),
        "ideal_loads"
    )
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

    # The same real model remains useful as source evidence while its type-3
    # systems must be rejected by the EnergyPlus conversion entry point.
    complete_model <- read_dest(model_path)
    on.exit(DBI::dbDisconnect(complete_model), add = TRUE)
    inventory <- hvac__source_inventory(complete_model)
    expect_equal(inventory$components[TABLE == "FAN", ROW_COUNT], 6L)
    expect_equal(inventory$components[TABLE == "CHILLER", ROW_COUNT], 0L)
    expect_error(
        to_idf(complete_model, "9.0.1"),
        regexp = paste0(
            "supports AC_SYS_TYPE values 0 and 1 only: ",
            "17674 \\(NAME=2-1, AC_SYS_TYPE=3\\)"
        ),
        class = "destep_unsupported_hvac_system_type"
    )

    # Removing only the room references leaves type-3 library records in the
    # source; those unowned records must not force an HVAC representation.
    DBI::dbExecute(
        complete_model,
        "UPDATE ROOM_GROUP SET OF_AC_SYS = 0 WHERE OF_AC_SYS > 0"
    )
    detached <- hvac__source_inventory(complete_model)
    expect_equal(nrow(detached$systems), 0L)
    expect_identical(detached$state, "load_only")
    expect_invisible(hvac__assert_supported_system_types(
        complete_model,
        detached
    ))
})

test_that("real type-0 and type-1 models can omit unreferenced duct networks", {
    skip_on_cran()

    equipment_path <- Sys.getenv("DESTEP_TEST_EQUIPMENT_ACCDB", unset = "")
    type0_path <- Sys.getenv("DESTEP_TEST_HVAC_TYPE0_ACCDB", unset = "")
    type1_path <- Sys.getenv("DESTEP_TEST_HVAC_TYPE1_ACCDB", unset = "")
    if (!all(file.exists(c(equipment_path, type0_path, type1_path)))) {
        skip(
            "Real type-0/type-1 DeST models and equipment library are required."
        )
    }

    equipment <- read_dest(
        equipment_path,
        tables = c("_Coil_Cooling", "Esp1CCoil")
    )
    on.exit(DBI::dbDisconnect(equipment), add = TRUE)

    # The selected AHUs point to product 10 but leave DUCTNET, FAN, and
    # LIB_CURVE empty. These tables are valid when their records are unused.
    for (entry in list(
        list(path = type0_path, type = 0L),
        list(path = type1_path, type = 1L)
    )) {
        model <- read_dest(
            entry$path,
            tables = c("AC_SYS", "AHU", "DUCTNET", "FAN", "LIB_CURVE")
        )
        result <- hvac__read_source_equipment(model, equipment)
        DBI::dbExecute(
            model,
            paste(
                "INSERT INTO AC_SYS",
                "(AC_SYS_ID, NAME, AC_SYS_TYPE, FRESH_AIR_TYPE)",
                "VALUES (999999, 'Unused template', 3, 1)"
            )
        )
        DBI::dbExecute(
            model,
            paste(
                "INSERT INTO AC_SYS",
                "(AC_SYS_ID, NAME, AC_SYS_TYPE, FRESH_AIR_TYPE)",
                "VALUES (999999, 'Unused duplicate', 3, 1)"
            )
        )
        filtered <- hvac__read_source_equipment(
            model,
            equipment,
            system_ids = result$systems$ac_system_id
        )
        expect_equal(nrow(filtered$systems), 1L)
        expect_identical(
            filtered$systems$ac_system_id,
            result$systems$ac_system_id
        )
        expect_error(
            hvac__read_source_equipment(model, equipment, system_ids = 999998L),
            class = "destep_unresolved_hvac_system_relation"
        )
        DBI::dbDisconnect(model)

        complete_model <- read_dest(entry$path)
        inventory <- hvac__source_inventory(complete_model)
        source <- hvac__multizone_airside_source(
            complete_model,
            inventory$systems$AC_SYS_ID[[1L]]
        )
        expect_identical(source$system$source_reheat_type[[1L]], 0L)
        expect_error(
            suppressWarnings(to_idf(complete_model, "9.0.1")),
            regexp = "9.1.0 or newer"
        )
        DBI::dbDisconnect(complete_model)
        expect_identical(inventory$state, "system_defined")
        expect_equal(
            sum(inventory$rooms$SOURCE_HVAC_STATE == "system_reference"),
            2L
        )
        expect_identical(inventory$systems$AC_SYS_TYPE, entry$type)
        expect_identical(
            inventory$systems$COOLING_COIL_STATE,
            "selected_unverified"
        )
        expect_true(all(
            inventory$components[SOURCE_ROLE == "model_component", ROW_COUNT] ==
                0L
        ))
        expect_true(all(
            inventory$components[SOURCE_ROLE == "catalogue", ROW_COUNT] > 0L
        ))

        expect_equal(nrow(result$systems), 1L)
        expect_identical(result$systems$ac_system_type, entry$type)
        expect_identical(result$systems$cooling_coil_id, 10L)
        expect_equal(result$systems$ahu_rated_air_flow_m3_s, 4598 / 3600)
        expect_true(result$systems$cooling_coil_air_flow_matches)
        expect_equal(nrow(result$networks), 0L)
        expect_equal(nrow(result$fan_links), 0L)
        expect_equal(nrow(result$fans), 0L)
        expect_equal(nrow(result$curves), 0L)
        expect_identical(result$cooling_coils$cooling_coil_id, 10L)
        expect_true(result$cooling_coils$specific_parameters_match)
    }
})

test_that("selected model-local plant products resolve to their source rows", {
    skip_on_cran()
    model_path <- Sys.getenv("DESTEP_TEST_HVAC_PLANT_ACCDB", unset = "")
    if (!nzchar(model_path) || !file.exists(model_path)) {
        skip("A DeST model with selected plant products is required.")
    }

    model <- read_dest(
        model_path,
        tables = c(
            "CHILLER",
            "LIB_CHILLER",
            "BOILER",
            "LIB_BOILER",
            "COOLINGTOWER",
            "LIB_COOLINGTOWER"
        )
    )
    on.exit(DBI::dbDisconnect(model), add = TRUE)
    plant <- hvac__read_model_plant(model)
    expect_equal(nrow(plant$chillers), 2L)
    expect_true(all(plant$chillers$SELECTION_STATE == "selected"))
    expect_setequal(plant$chillers$LIB_CHILLER_ID, 23L)
    expect_equal(plant$chillers$RATED_COP, c(5, 5))
    expect_equal(
        plant$chillers$CAPACITY_KW,
        rep(1225.5104, 2L),
        tolerance = 1e-4
    )
    expect_equal(nrow(plant$boilers), 1L)
    expect_identical(plant$boilers$LIB_BOILER_ID, 17L)
    expect_equal(plant$boilers$RATED_EFFICIENCY, 1)
    expect_equal(nrow(plant$cooling_towers), 2L)
    expect_true(all(plant$cooling_towers$LIB_COOLINGTOWER_ID == 15L))
    expect_equal(plant$cooling_towers$FAN_POWER_KW, rep(12.944877, 2L))

    # A model-selected ID with no matching library row is an input error;
    # substituting a different catalogue product would invent equipment.
    DBI::dbExecute(
        model,
        "UPDATE CHILLER SET LIB_CHILLER_ID = 999999 WHERE CHILLER_ID = 40766"
    )
    expect_error(
        hvac__read_model_plant(model),
        class = "destep_unresolved_hvac_plant_product"
    )
})
