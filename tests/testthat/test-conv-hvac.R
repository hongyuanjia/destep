test_that("maps verified DeST outdoor-air control types", {
    expect_identical(hvac__economizer_type(1L), "NoEconomizer")
    expect_identical(hvac__economizer_type(5L), "DifferentialDryBulb")
    expect_identical(hvac__economizer_type(6L), "DifferentialEnthalpy")
    expect_error(hvac__economizer_type(2L), "DeST FRESH_AIR_TYPE")
})

test_that("selects fan parameters for each multizone operating mode", {
    coefficients <- paste0("return_fan_power_coefficient_", seq_len(5L))

    expect_false(any(coefficients %in% hvac__required_options("multizone_cav")))
    expect_true(all(coefficients %in% hvac__required_options("multizone_vav")))
})

# Main coil technology comes from the water-source model; no plant selection
# or fuel parameter belongs to the resolved graph requirements.
test_that("main water coil does not require user-selected plant technology", {
    required <- hvac__required_options("single_zone_cav")
    expect_true("heating_coil_type" %in% required)
    expect_false(any(
        c("boiler_type", "chiller_nominal_cop", "tower_type") %in% required
    ))
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

# DeST distinguishes an absent heater from a present but unselected one; the
# independent heater presence must not forbid a source main water coil.
test_that("heater presence is checked before adding a physical coil", {
    expect_invisible(hvac__assert_heater_source(-1L, 10L))
    expect_invisible(hvac__assert_heater_source(0L, 10L))
    expect_error(
        hvac__assert_heater_source(12L, 10L),
        class = "destep_unsupported_hvac_heater_state"
    )
    expect_error(
        hvac__assert_heater_source(NA_integer_, 10L),
        class = "destep_unsupported_hvac_heater_state"
    )
})

# An archived single-room DeST air system checks that the guard runs before
# the constant-volume template creates a hot-water heating coil.
test_that("single-room air systems retain source heater and pipe fields", {
    skip_on_cran()
    path <- Sys.getenv("DESTEP_TEST_HVAC_SINGLE_SQLITE", unset = "")
    if (!nzchar(path) || !file.exists(path)) {
        skip("The archived AE101 DeST source database is required.")
    }
    # Use a disposable copy so changing one field cannot affect the archive.
    isolated <- tempfile(fileext = ".sqlite")
    expect_true(file.copy(path, isolated))
    on.exit(unlink(isolated), add = TRUE)
    model <- DBI::dbConnect(RSQLite::SQLite(), isolated)
    on.exit(DBI::dbDisconnect(model), add = TRUE)
    sources <- hvac__system_sources(model, list())
    expect_length(sources, 1L)
    expect_identical(sources[[1L]]$path, "single_zone_cav")
    expect_identical(sources[[1L]]$source$source_heater_id[[1L]], -1L)
    expect_identical(sources[[1L]]$source$source_water_type[[1L]], 0L)

    DBI::dbExecute(model, "UPDATE ROOM SET SET_TERMINAL_MAX=1000")
    expect_error(
        hvac__system_sources(model, list()),
        class = "destep_unsupported_hvac_terminal_type"
    )
    DBI::dbExecute(model, "UPDATE ROOM SET SET_TERMINAL_MAX=0")

    # The only changed input is the heater ID in this independent DBI copy.
    DBI::dbExecute(model, "UPDATE AHU SET HEATER=0")
    expect_length(hvac__system_sources(model), 1L)
})

# A real source model verifies that selected water temperature inputs survive
# expansion without a user-selected heater technology or invented plant.
test_that("single-room main water coil uses the source water boundary", {
    skip_on_cran()
    path <- Sys.getenv("DESTEP_TEST_HVAC_SINGLE_ACCDB", unset = "")
    if (!nzchar(path) || !file.exists(path)) {
        skip("The real AE201 model is required.")
    }
    model <- read_dest(path)
    on.exit(DBI::dbDisconnect(model), add = TRUE)
    DBI::dbExecute(
        model,
        "UPDATE ROOM_TYPE_DATA SET O_DAMP_PER_PERSON=0, E_MAX_HUM=0, E_MIN_HUM=0"
    )
    target <- suppressWarnings(to_eplus(model, "9.0.1"))
    expect_true(target$is_valid())
    expect_equal(target$object_num(class = "Coil:Heating:Water"), 1L)
    expect_equal(
        target$object_num(class = "PlantComponent:TemperatureSource"),
        2L
    )
    expect_false(any(grepl(
        "^(Chiller:|Boiler:|CoolingTower:|DistrictHeating$|DistrictCooling$)",
        target$class_name()
    )))
    expect_equal(target$object_num(class = "Coil:Heating:Electric"), 0L)
    water <- attr(target, "conversion")$hvac$water
    expect_equal(water$schedule_id, c(52L, 52L))
    expect_equal(water$minimum_c, c(60, 60))
    manager <- target$to_table(
        class = "SetpointManager:SingleZone:Heating",
        wide = TRUE
    )
    expect_equal(as.numeric(manager$`Minimum Supply Air Temperature`), 14)
    expect_equal(as.numeric(manager$`Maximum Supply Air Temperature`), 32)
    expect_error(
        suppressWarnings(to_eplus(
            model,
            "9.0.1",
            options = destep_opts(
                hvac_options = list(heating_coil_type = "Electric")
            )
        )),
        class = "destep_conflicting_hvac_equipment_option"
    )
})

# Matching expanders must preserve moisture EMS and source airflow, including
# 9.1's direct terminal and the ADU-wrapped terminals in newer releases.
test_that("modern physical targets retain source moisture and terminal references", {
    skip_on_cran()
    path <- Sys.getenv("DESTEP_TEST_HVAC_SINGLE_ACCDB", unset = "")
    if (!nzchar(path) || !file.exists(path)) {
        skip("The real AE201 model is required.")
    }
    versions <- c("9.1.0", "9.6.0", "23.1.0")
    installed <- vapply(
        versions,
        function(version) {
            directory <- tryCatch(
                eplusr::eplus_config(version)$dir,
                error = function(error) ""
            )
            file.exists(file.path(directory, "ExpandObjects"))
        },
        logical(1L)
    )
    if (!all(installed)) {
        skip("EnergyPlus 9.1.0, 9.6.0 and 23.1.0 ExpandObjects are required.")
    }
    model <- read_dest(path)
    on.exit(DBI::dbDisconnect(model), add = TRUE)
    # Retain all source moisture, unlike the independent 9.0.1 graph check.
    source <- hvac__system_sources(model)[[1L]]$source
    moisture <- DBI::dbGetQuery(
        model,
        paste(
            "SELECT R.NAME, T.O_MAXNUMBER, T.O_DAMP_PER_PERSON FROM ROOM R",
            "JOIN ROOM_TYPE_DATA T ON R.TYPE=T.ID"
        )
    )
    expect_true(any(moisture$O_MAXNUMBER * moisture$O_DAMP_PER_PERSON > 0))
    for (version in versions) {
        target <- suppressWarnings(to_eplus(model, version))
        expect_true(target$is_valid())
        expect_identical(as.character(target$version()), version)
        if (version == "9.1.0") {
            direct <- target$to_table(
                class = "AirTerminal:SingleDuct:Uncontrolled",
                wide = TRUE
            )
            expect_equal(nrow(direct), 1L)
            terminal <- target$object(direct$id[[1L]])
        } else {
            unit <- target$to_table(
                class = "ZoneHVAC:AirDistributionUnit",
                wide = TRUE
            )
            expect_equal(nrow(unit), 1L)
            terminal <- target$object(unit$`Air Terminal Name`[[1L]])
            expect_identical(
                unit$`Air Terminal Object Type`[[1L]],
                "AirTerminal:SingleDuct:ConstantVolume:NoReheat"
            )
        }
        expect_equal(
            as.numeric(unlist(terminal$value("maximum_air_flow_rate"))),
            source$supply_flow_m3_s[[1L]]
        )
        gains <- target$to_table(class = "OtherEquipment", wide = TRUE)
        wet <- gains[gains$`End-Use Subcategory` == "DeST People Moisture", ]
        expect_equal(
            nrow(wet),
            sum(moisture$O_MAXNUMBER * moisture$O_DAMP_PER_PERSON > 0)
        )
        expect_true(all(as.numeric(wet$`Fraction Latent`) == 1))
        managers <- target$to_table(
            class = "EnergyManagementSystem:ProgramCallingManager",
            wide = TRUE
        )
        expect_true(all(
            managers$`EnergyPlus Model Calling Point` ==
                "BeginZoneTimestepBeforeInitHeatBalance"
        ))
        expect_false(any(grepl(
            "^(HVACTemplate:|District)",
            target$class_name()
        )))
    }
})

# AE401 defines terminal capacities even though central AHU reheat is absent.
# Actual expanded objects verify source units, room-specific classes and audit.
test_that("room terminal capacity is independent of central AHU reheat", {
    skip_on_cran()
    path <- Sys.getenv("DESTEP_TEST_HVAC_TYPE1_ACCDB", unset = "")
    if (!nzchar(path) || !file.exists(path)) {
        skip("The real AE401 DeST model is required.")
    }
    installation <- tryCatch(
        eplusr::eplus_config("9.0.1")$dir,
        error = function(error) ""
    )
    if (!file.exists(file.path(installation, "ExpandObjects"))) {
        skip("EnergyPlus 9.0.1 ExpandObjects is required.")
    }
    model <- read_dest(path)
    on.exit(DBI::dbDisconnect(model), add = TRUE)
    system_id <- DBI::dbGetQuery(
        model,
        "SELECT AC_SYS_ID FROM AC_SYS"
    )$AC_SYS_ID[[1L]]
    source <- hvac__multizone_airside_source(model, system_id)
    expect_identical(source$system$source_reheat_type[[1L]], 0L)
    expect_true(all(source$zones$terminal_has_reheat))
    expect_equal(source$zones$terminal_capacity_w, c(10000, 10000))
    options <- hvac__boundary_system_options(source, "multizone_vav")
    expect_false("zone_outdoor_air_flow_m3_s" %in% names(options))
    validated <- hvac__validate_terminal_options(options, source)
    expect_equal(
        sum(validated$zones$outdoor_air_flow_m3_s),
        source$system$outdoor_air_flow_m3_s[[1L]]
    )
    expect_false("outdoor_air_flow_m3_s" %in% names(source$zones))
    conflicting <- options
    conflicting$reheat_coil_type <- "Electric"
    expect_error(
        hvac__validate_terminal_options(conflicting, source),
        class = "destep_conflicting_hvac_equipment_option"
    )
    unsupported <- list(
        system = data.table::copy(source$system),
        zones = source$zones
    )
    for (type in 1:2) {
        data.table::set(unsupported$system, 1L, "source_reheat_type", type)
        expect_error(
            hvac__validate_terminal_options(options, unsupported),
            class = "destep_unsupported_hvac_reheat_type"
        )
    }
    # Moisture is zeroed only in a DBI copy to isolate the existing 9.0.1 graph.
    DBI::dbExecute(
        model,
        "UPDATE ROOM_TYPE_DATA SET O_DAMP_PER_PERSON=0, E_MAX_HUM=0, E_MIN_HUM=0"
    )
    idf <- suppressWarnings(to_eplus(
        model,
        "9.0.1",
        options = destep_opts(hvac = "physical")
    ))
    expect_true(idf$is_valid())
    expect_equal(
        idf$object_num(class = "AirTerminal:SingleDuct:VAV:Reheat"),
        2L
    )
    terminals <- idf$to_table(
        class = "AirTerminal:SingleDuct:VAV:Reheat",
        wide = TRUE
    )
    coils <- idf$to_table(class = "Coil:Heating:Electric", wide = TRUE)
    expect_equal(
        as.numeric(coils$`Nominal Capacity`[match(
            terminals$`Reheat Coil Name`,
            coils$Name
        )]),
        c(10000, 10000)
    )
    audit <- attr(idf, "conversion")$hvac$terminals
    expect_equal(audit$terminal_capacity_w, c(10000, 10000))
    expect_identical(
        unique(audit$terminal_type_origin),
        "converter_default_electric"
    )
    expect_true(any(grepl(
        "capacity=10000 W",
        un_list(idf$Version$comment()),
        fixed = TRUE
    )))
    # A single room capacity change must affect only its own terminal.
    DBI::dbExecute(
        model,
        "UPDATE ROOM SET SET_TERMINAL_MAX=0 WHERE ID=?",
        params = list(source$zones$room_id[[1L]])
    )
    mixed <- suppressWarnings(to_eplus(
        model,
        "9.0.1",
        options = destep_opts(hvac = "physical")
    ))
    expect_true(mixed$is_valid())
    expect_equal(
        mixed$object_num(class = "AirTerminal:SingleDuct:VAV:Reheat"),
        1L
    )
    expect_equal(
        mixed$object_num(class = "AirTerminal:SingleDuct:VAV:NoReheat"),
        1L
    )
    # Central reheat is never substituted by a different set of room terminals.
    DBI::dbExecute(model, "UPDATE AHU SET REHEAT_TYPE=1")
    expect_error(
        suppressWarnings(to_eplus(
            model,
            "9.0.1",
            options = destep_opts(hvac = "physical")
        )),
        class = "destep_unsupported_hvac_reheat_type"
    )
})
