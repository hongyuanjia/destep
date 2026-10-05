test_that("one options entry point retains source inputs and target settings", {
    expect_named(
        formals(to_eplus),
        c("dest", "ver", "copy", "verbose", "options")
    )
    expect_identical(formals(to_eplus)$options, "objects")
    basic <- destep_opts()
    expect_s3_class(basic, "destep_options")
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
        terrain = "Country",
        solar_distribution = "FullExteriorWithReflections"
    )
    expect_identical(custom$terrain, "Country")
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
        "window_optics"
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
    expect_error(destep_opts(terrain = "Forest"), "terrain")
    expect_error(destep_opts(shadow_update_days = 0), "shadow_update_days")
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
    expect_error(to_eplus(NULL, options = invalid), "terrain")
    invalid <- destep_opts()
    invalid$unknown <- TRUE
    expect_error(to_eplus(NULL, options = invalid), "fields")
    expect_error(to_eplus(NULL, options = list()), "destep_opts")
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
    src <- ensure_dest_sqlite_file()
    on.exit(DBI::dbDisconnect(src), add = TRUE)
    from_string <- suppressWarnings(to_eplus(src, "23.1", options = "objects"))
    opts <- destep_opts()
    from_object <- suppressWarnings(to_eplus(src, "23.1", options = opts))
    expect_equal(from_object$to_table(), from_string$to_table())
    expect_equal(
        attr(from_object, "conversion"),
        attr(from_string, "conversion")
    )
    expect_identical(opts, destep_opts())
    custom <- destep_opts(
        surface_convection = "energyplus",
        terrain = "Country",
        solar_distribution = "FullExteriorWithReflections",
        shadow_update_days = 1L
    )
    converted <- suppressWarnings(to_eplus(src, "23.1", options = custom))
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
    expect_equal(audit$simulation$effective$terrain, "Country")
    expect_equal(
        audit$simulation$effective$solar_distribution,
        "FullExteriorWithReflections"
    )
    expect_equal(audit$simulation$effective$shadow_update_days, 1L)
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
    expect_identical(custom$terrain, "Country")
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
