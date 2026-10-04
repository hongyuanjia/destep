# Equal seasonal sensible coefficients are the supported source subset; a
# total-heat coefficient cannot determine a sensible/latent pair uniquely.
test_that("heat recovery distinguishes seasonal and latent coefficients", {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbWriteTable(con, "AHU", data.frame(AHU_ID = 1L))
    handler <- data.frame(
        AHU_ID = 1L,
        AHURES = 0L,
        HEAT_RECOVER = 2L,
        MIN_T_EX_COEF = 0,
        MAX_T_EX_COEF = 0.4,
        MIN_D_EX_COEF = 0,
        MAX_D_EX_COEF = 0.4
    )
    source <- hvac__heat_recovery_source(con, handler)
    expect_identical(source$type, "Sensible")
    expect_identical(source$sensible_effectiveness, 0.4)
    expect_identical(source$latent_effectiveness, 0)
    expect_match(
        source$target_assumptions,
        "pressure loss is not separately mapped",
        fixed = TRUE
    )
    handler$HEAT_RECOVER <- 1L
    expect_error(
        hvac__heat_recovery_source(con, handler),
        class = "destep_unsupported_hvac_heat_recovery"
    )
    handler$HEAT_RECOVER <- 2L
    handler$MAX_D_EX_COEF <- 0.6
    expect_error(
        hvac__heat_recovery_source(con, handler),
        class = "destep_unsupported_hvac_heat_recovery"
    )
    handler$MAX_D_EX_COEF <- 0.4
    handler$MIN_T_EX_COEF <- 0.1
    expect_error(
        hvac__heat_recovery_source(con, handler),
        class = "destep_unsupported_hvac_heat_recovery"
    )
    handler$MIN_T_EX_COEF <- 0
    handler$AHURES <- 10L
    expect_error(
        hvac__heat_recovery_source(con, handler),
        class = "destep_unsupported_hvac_heat_recovery"
    )
    handler$AHURES <- 0L
    handler$MAX_D_EX_COEF <- NA_real_
    expect_error(hvac__heat_recovery_source(con, handler))
    handler$MAX_D_EX_COEF <- 1.2
    expect_error(hvac__heat_recovery_source(con, handler))
    handler$MAX_D_EX_COEF <- 0.4
    DBI::dbExecute(con, "ALTER TABLE AHU ADD EXT_PROPERTY INTEGER")
    DBI::dbExecute(con, "UPDATE AHU SET EXT_PROPERTY=10")
    DBI::dbWriteTable(
        con,
        "EXT_PROPERTY",
        data.frame(
            PROPERTY_ID = 10L,
            NEXT_PROPERTY = 0L,
            NAME = "AHU_HEAT_RECOVER_RESISTANCE",
            TYPE = 0L,
            DATA_LONG = 0L,
            DATA_DOUBLE = 20
        )
    )
    expect_error(
        hvac__heat_recovery_source(con, handler),
        class = "destep_unsupported_hvac_heat_recovery"
    )
    DBI::dbExecute(con, "UPDATE EXT_PROPERTY SET DATA_DOUBLE=0, DATA_LONG=20")
    expect_error(
        hvac__heat_recovery_source(con, handler),
        class = "destep_unsupported_hvac_heat_recovery"
    )
    DBI::dbExecute(con, "UPDATE EXT_PROPERTY SET DATA_LONG=0")
    expect_identical(hvac__heat_recovery_source(con, handler)$type, "Sensible")
    handler$HEAT_RECOVER <- 0L
    expect_identical(hvac__heat_recovery_source(con, handler)$type, "None")
})

# Exercise the public path and inspect the expanded graph. Template defaults
# must not add an efficiency increment or unrequested auxiliary electricity.
test_that("constant sensible recovery survives expansion and transition", {
    skip_on_cran()
    path <- Sys.getenv("DESTEP_TEST_HVAC_SINGLE_SQLITE", unset = "")
    skip_if(!file.exists(path), "Archived single-zone source is required")
    original <- DBI::dbConnect(
        RSQLite::SQLite(),
        path,
        flags = RSQLite::SQLITE_RO
    )
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    RSQLite::sqliteCopyDatabase(original, con)
    DBI::dbDisconnect(original)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbExecute(
        con,
        paste(
            "UPDATE AHU SET HUMIDIFIER=0, HEAT_RECOVER=2,",
            "MIN_T_EX_COEF=0, MIN_D_EX_COEF=0, MAX_T_EX_COEF=0.4, MAX_D_EX_COEF=0.4"
        )
    )
    model <- suppressWarnings(to_eplus(
        con,
        "9.1",
        options = destep_opts(run_period = c(1, 1))
    ))
    expect_true(model$is_valid())
    hx <- model$objects_in_class("HeatExchanger:AirToAir:SensibleAndLatent")
    expect_length(hx, 1L)
    table <- hx[[1L]]$to_table()
    sensible <- grepl("Sensible Effectiveness", table$field)
    latent <- grepl("Latent Effectiveness", table$field)
    expect_equal(sum(sensible), 4L)
    expect_equal(as.numeric(table$value[sensible]), rep(0.4, 4L))
    expect_equal(sum(latent), 4L)
    expect_equal(as.numeric(table$value[latent]), rep(0, 4L))
    expect_equal(
        as.numeric(unlist(hx[[1L]]$value("nominal_electric_power"))),
        0
    )
    expect_identical(
        unname(unlist(hx[[1L]]$value("supply_air_outlet_temperature_control"))),
        "No"
    )
    managers <- model$to_table(class = "SetpointManager:Scheduled", wide = TRUE)
    expect_false(any(
        managers[["Setpoint Node or NodeList Name"]] ==
            unname(unlist(hx[[1L]]$value("supply_air_outlet_node_name")))
    ))
    exhaust <- model$objects_in_class("Fan:ZoneExhaust")
    expect_length(exhaust, 1L)
    expect_identical(
        unname(unlist(hx[[1L]]$value("exhaust_air_inlet_node_name"))),
        unname(unlist(exhaust[[1L]]$value("air_outlet_node_name")))
    )
    expect_identical(
        attr(model, "conversion")$hvac$air_treatment[[1L]]$heat_recovery$status,
        "constant_sensible"
    )
    upgraded <- conv__transition(model, numeric_version("23.1.0"))
    expect_true(upgraded$is_valid())
    expect_identical(
        unname(unlist(upgraded$objects_in_class(
            "HeatExchanger:AirToAir:SensibleAndLatent"
        )[[1L]]$value("supply_air_outlet_temperature_control"))),
        "No"
    )
    expect_length(
        upgraded$objects_in_class("HeatExchanger:AirToAir:SensibleAndLatent"),
        1L
    )
})
