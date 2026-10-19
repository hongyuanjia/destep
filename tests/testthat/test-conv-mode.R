test_that("one options entry point retains source inputs and representation choices", {
    expect_named(
        formals(to_idf),
        c("dest", "ver", "copy", "verbose", "options")
    )
    expect_identical(formals(to_idf)$options, "objects")
    basic <- destep_opts()
    expect_s3_class(basic, "destep_options")
    expect_false(any(
        c("window_model", "window_weather", "window_directory") %in%
            names(basic)
    ))
    expect_identical(basic$surface_convection, "dest")
    expect_identical(conv__resolve_options("objects"), basic)
    expect_identical(conv__resolve_options(basic), basic)
    expect_identical(unserialize(serialize(basic, NULL)), basic)
    expect_false(any(
        c(
            "window_optics",
            "source_distribution",
            "exterior_boundary",
            "weather",
            "directory",
            "sky_radiation"
        ) %in%
            names(basic)
    ))
    custom <- destep_opts(
        surface_convection = "energyplus"
    )
    expect_identical(custom$surface_convection, "energyplus")
    expect_identical(conv__resolve_options(custom), custom)
})

test_that("retired solver adapter options cannot silently alter a model", {
    expect_error(destep_opts("dest"), "preset")
    for (option in c(
        "source_distribution",
        "exterior_boundary",
        "sky_radiation",
        "weather",
        "directory",
        "people_heat",
        "partition_boundary",
        "window_optics",
        "terrain",
        "solar_distribution",
        "shadow_update_days",
        "window_model",
        "window_weather",
        "window_directory"
    )) {
        args <- stats::setNames(list("dest"), option)
        expect_error(
            do.call(destep_opts, args),
            "Unknown or unnamed",
            info = option
        )
    }
    expect_error(destep_opts(c("objects", "dest")), "preset")
    expect_error(destep_opts("objects", "constant"), "Unknown or unnamed")
    expect_s3_class(destep_opts(hvac_options = list()), "destep_options")
    expect_error(
        destep_opts(hvac = "ideal_loads", hvac_options = list()),
        "hvac_options"
    )
    expect_s3_class(destep_opts(hvac = "physical"), "destep_options")
    opts <- destep_opts(
        hvac = "physical",
        hvac_options = list(chiller_nominal_cop = 4)
    )
    expect_equal(opts$hvac_options$chiller_nominal_cop, 4)
    invalid <- destep_opts()
    invalid$terrain <- "Forest"
    expect_error(to_idf(NULL, options = invalid), "fields")
    invalid <- destep_opts()
    invalid$unknown <- TRUE
    expect_error(to_idf(NULL, options = invalid), "fields")
    # A previously prepared option must fail at the public boundary, rather
    # than silently generating the single direct representation instead.
    for (method in c("equivalent", "source_equivalent", "n2_multilayer")) {
        expect_error(destep_opts(window_model = method), "Unknown or unnamed")
        invalid <- destep_opts()
        invalid$window_model <- method
        expect_error(to_idf(NULL, options = invalid), "fields")
    }
    expect_error(to_idf(NULL, options = list()), "destep_opts")
})

test_that("conversion audit labels necessary moisture EMS", {
    ep <- eplusr::empty_idf("23.1")
    basic <- conv__conversion_options(destep_opts())
    expect_equal(nrow(conv__mode_audit(ep, basic)$ems), 0L)
    ep$add(
        "EnergyManagementSystem:Program" := list(
            name = "DeST_Moisture_1_Control",
            program_line_1 = "SET MassRate = 0.01"
        )
    )
    audit <- conv__mode_audit(ep, basic)
    expect_identical(audit$ems$purpose, "equipment_moisture")
    expect_identical(audit$ems$requirement, "source_input")
    expect_match(
        conv__mode_comments(audit)[[2L]],
        "surface_convection=dest",
        fixed = TRUE
    )
    ep$add(
        "EnergyManagementSystem:Program" := list(
            name = "DeST_People_Moisture_1_Control",
            program_line_1 = "SET MassRate = 0.01"
        )
    )
    audit <- conv__mode_audit(ep, basic)
    expect_identical(tail(audit$ems$purpose, 1L), "people_moisture")
    expect_true(all(audit$ems$requirement == "source_input"))
})

# Use a real source database to verify that the public entry point routes both
# presets and custom settings into generated objects, not just audit metadata.
test_that("preset strings and options objects produce equivalent real conversions", {
    skip_on_cran()
    src <- destep_test__unmultiplied_fixture()
    on.exit(DBI::dbDisconnect(src), add = TRUE)
    from_string <- suppressWarnings(to_idf(src, "23.1", options = "objects"))
    opts <- destep_opts()
    from_object <- suppressWarnings(to_idf(src, "23.1", options = opts))
    expect_equal(from_object$to_table(), from_string$to_table())
    expect_equal(
        attr(from_object, "conversion"),
        attr(from_string, "conversion")
    )
    expect_identical(opts, destep_opts())
    custom <- destep_opts(surface_convection = "energyplus")
    converted <- suppressWarnings(to_idf(src, "23.1", options = custom))
    expect_true(converted$is_valid())
    audit <- attr(converted, "conversion")
    expect_identical(audit$options, custom)
    expect_s3_class(audit$hvac$source$components, "data.table")
    expect_true(any(grepl(
        "destep model-local plant records:",
        un_list(converted$Version$comment()),
        fixed = TRUE
    )))
    expect_equal(audit$effective$surface_convection, "energyplus")
    expect_equal(audit$simulation, attr(from_string, "conversion")$simulation)
    expect_true(all(audit$simulation$selection == "retained_default"))
    expect_true(
        "SurfaceProperty:ConvectionCoefficients" %in%
            from_string$to_table()$class
    )
    # Furniture keeps its source exchange definition independently of the
    # envelope setting. Only InternalMass coefficients should remain here.
    fields <- converted$to_table()
    programs <- fields$value[
        fields$class == "EnergyManagementSystem:Program" &
            fields$index == 1L
    ]
    expect_false(any(grepl(
        "SourceCorrectionUpdate|DeSTSkyWeatherUpdate",
        programs
    )))
    expect_null(attr(converted, "source_distribution"))
    expect_null(attr(converted, "exterior_boundary"))
    remaining <- fields$value[
        fields$class == "SurfaceProperty:ConvectionCoefficients" &
            fields$index == 1L
    ]
    furniture <- fields$value[
        fields$class == "InternalMass" & fields$index == 1L
    ]
    expect_setequal(remaining, furniture)
    original <- from_string$to_table()
    expect_gt(
        sum(
            original$class == "SurfaceProperty:ConvectionCoefficients" &
                original$index == 1L
        ),
        length(remaining)
    )
    expect_identical(custom$surface_convection, "energyplus")
})

# Public model/weather names are paired, with no legacy export or wrapper.
test_that("model conversion exports to_idf alongside to_epw", {
    exports <- getNamespaceExports("destep")
    expect_true(all(c("to_idf", "to_epw", "destep_opts") %in% exports))
    expect_false("to_eplus" %in% exports)
    expect_false(exists(
        "to_eplus",
        envir = asNamespace("destep"),
        inherits = FALSE
    ))
})

# HVAC-generated coordinate conversion is a required source input; external
# EMS remains visibly unclassified rather than being attributed to destep.
test_that("conversion audit identifies supply humidity EMS", {
    ep <- eplusr::empty_idf("9.1")
    for (name in c(
        "DeST_Supply_RH_1_Cache",
        "DeST_Supply_RH_1_Convert",
        "External"
    )) {
        ep$add(
            `EnergyManagementSystem:Program` = list(
                name = name,
                program_line_1 = "SET X = 1"
            )
        )
    }
    audit <- conv__mode_audit(ep, list(mode = "objects"))
    expect_identical(
        audit$ems$purpose,
        c("supply_humidity", "supply_humidity", "unclassified")
    )
    expect_identical(
        audit$ems$requirement,
        c("source_input", "source_input", "unclassified")
    )
    expect_match(
        conv__mode_comments(audit)[[3L]],
        "supply_humidity:source_input",
        fixed = TRUE
    )
})

# Public options must make approximations explicit and prevent internally
# inconsistent flow-density assumptions before touching a source database.
test_that("state control choices are explicit and validated", {
    options <- destep_opts(
        ventilation_density = "standard",
        occupant_sensible = "temperature_feedback",
        ventilation_control = "target_prediction"
    )
    expect_identical(conv__resolve_options(options), options)
    expect_identical(destep_opts()$occupant_sensible, "nominal")
    expect_identical(destep_opts()$ventilation_control, "outdoor_band")
    expect_error(
        destep_opts(ventilation_control = "target_prediction"),
        "standard"
    )
    expect_error(
        destep_opts(occupant_sensible = "native_replay"),
        "occupant_sensible"
    )
    db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(db))
    expect_error(conv__generation_version(db, "23.1", options), "26.1")
    expect_equal(
        as.character(conv__generation_version(db, "26.1", options)),
        "26.1.0"
    )
})

# Multiple People in one room share feedback, while a second room retains its
# own state even when the source nominal activity schedule is shared.
test_that("people feedback binds independent room states without duplicate heat", {
    ep <- eplusr::empty_idf("26.1")
    ep$add(
        Zone = list(name = "A"),
        Zone = list(name = "B"),
        `Schedule:Constant` = list(name = "One", hourly_value = 1),
        `Schedule:Constant` = list(name = "Nominal", hourly_value = 61),
        People = list(
            name = "A Minimum",
            zone_or_zonelist_or_space_or_spacelist_name = "A",
            number_of_people_schedule_name = "One",
            number_of_people_calculation_method = "People",
            number_of_people = 1,
            fraction_radiant = .4,
            sensible_heat_fraction = 1,
            activity_level_schedule_name = "Nominal"
        ),
        People = list(
            name = "A Scheduled",
            zone_or_zonelist_or_space_or_spacelist_name = "A",
            number_of_people_schedule_name = "One",
            number_of_people_calculation_method = "People",
            number_of_people = 2,
            fraction_radiant = .4,
            sensible_heat_fraction = 1,
            activity_level_schedule_name = "Nominal"
        ),
        People = list(
            name = "B People",
            zone_or_zonelist_or_space_or_spacelist_name = "B",
            number_of_people_schedule_name = "One",
            number_of_people_calculation_method = "People",
            number_of_people = 3,
            fraction_radiant = .4,
            sensible_heat_fraction = 1,
            activity_level_schedule_name = "Nominal"
        )
    )
    original <- state_controls__rows(ep, "People")
    audit <- state_controls__apply(
        ep,
        destep_opts(occupant_sensible = "temperature_feedback"),
        "ideal_loads"
    )
    actual <- state_controls__rows(ep, "People")
    expect_length(audit$people, 2L)
    expect_identical(actual[[1L]][["10"]], actual[[2L]][["10"]])
    expect_false(identical(actual[[1L]][["10"]], actual[[3L]][["10"]]))
    for (i in seq_along(original)) {
        expect_identical(
            original[[i]][as.character(1:9)],
            actual[[i]][as.character(1:9)]
        )
    }
    expect_length(state_controls__rows(ep, "OtherEquipment"), 0L)
    expect_length(state_controls__rows(ep, "ElectricEquipment"), 0L)
    programs <- unlist(state_controls__rows(
        ep,
        "EnergyManagementSystem:Program"
    ))
    expect_true(any(grepl("IF TimeStepNum == 1", programs, fixed = TRUE)))
    expect_true(any(grepl("5.536", programs, fixed = TRUE)))
    expect_false(audit$source_results_consumed)
    ems <- conv__mode_audit(ep, conv__conversion_options(destep_opts()))$ems
    expect_true(all(ems$purpose == "people_sensible_temperature_feedback"))
})

# Standard density is a real emitted field, not only an R audit label. Reading
# class rows must not leak Version or unrelated classes via data.table masking.
test_that("standard density reaches only ventilation objects", {
    ep <- eplusr::empty_idf("26.1")
    ep$add(
        Zone = list(name = "A"),
        `Schedule:Constant` = list(name = "One", hourly_value = 1),
        `ZoneVentilation:DesignFlowRate` = list(
            name = "Vent",
            zone_or_zonelist_or_space_or_spacelist_name = "A",
            schedule_name = "One",
            design_flow_rate_calculation_method = "AirChanges/Hour",
            air_changes_per_hour = .3
        )
    )
    expect_length(
        state_controls__rows(ep, "ZoneVentilation:DesignFlowRate"),
        1L
    )
    state_controls__apply(
        ep,
        destep_opts(ventilation_density = "standard"),
        "ideal_loads"
    )
    expect_equal(
        state_controls__rows(ep, "ZoneVentilation:DesignFlowRate")[[1L]][[
            "27"
        ]],
        "Standard"
    )
    expect_equal(as.character(ep$version()), "26.1.0")
})
