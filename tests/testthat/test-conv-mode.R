test_that("one options entry point accepts presets and reusable configurations", {
    expect_named(
        formals(to_eplus),
        c("dest", "ver", "copy", "verbose", "options")
    )
    expect_identical(formals(to_eplus)$options, "objects")
    basic <- destep_opts()
    expect_s3_class(basic, "destep_options")
    expect_equal(basic$people_heat, "constant")
    expect_equal(basic$window_optics, "simple_glazing")
    expect_equal(basic$source_distribution, "energyplus")
    expect_equal(basic$surface_convection, "dest")
    expect_equal(basic$exterior_boundary, "energyplus")
    expect_identical(conv__resolve_options("objects"), basic)
    expect_identical(conv__resolve_options(basic), basic)
    expect_identical(conv__resolve_options("dest"), destep_opts("dest"))
    expect_identical(unserialize(serialize(basic, NULL)), basic)

    dest <- destep_opts("dest")
    expect_equal(dest$people_heat, "temperature_dependent")
    expect_equal(dest$window_optics, "dest_solar")
    expect_equal(dest$source_distribution, "dest")
    expect_equal(dest$exterior_boundary, "dest_sky")
    explicit <- destep_opts(
        "dest",
        people_heat = "constant",
        source_distribution = "energyplus",
        exterior_boundary = "energyplus",
        terrain = "Country",
        solar_distribution = "FullExteriorWithReflections",
        shadow_update_days = 1L
    )
    expect_equal(explicit$people_heat, "constant")
    expect_equal(explicit$source_distribution, "energyplus")
    expect_equal(explicit$exterior_boundary, "energyplus")
    expect_equal(explicit$window_optics, "dest_solar")
    expect_identical(conv__resolve_options(explicit), explicit)
    expect_error(to_eplus(NULL, mode = "dest"), "unused argument")
    expect_error(to_eplus(NULL, surface_convection = "dest"), "unused argument")
    expect_error(
        to_eplus(NULL, options = list(preset = "objects")),
        "destep_opts"
    )
})

test_that("configurations reject typos and modified invalid objects before database access", {
    expect_error(destep_opts("unknown"), "preset")
    expect_error(destep_opts(c("objects", "dest")), "preset")
    expect_error(destep_opts(people_hea = "constant"), "Unknown or unnamed")
    expect_error(destep_opts("objects", "constant"), "Unknown or unnamed")
    expect_error(
        destep_opts(source_options = list(exterior_boundary = "dest_sky")),
        "Unknown"
    )
    expect_error(
        destep_opts(simulation_options = list(terrain = "Country")),
        "Unknown"
    )
    expect_error(destep_opts(people_heat = "other"), "people_heat")
    expect_error(destep_opts(terrain = "Forest"), "terrain")
    expect_error(destep_opts(shadow_update_days = 0), "shadow_update_days")
    invalid <- destep_opts()
    invalid$terrain <- "Forest"
    expect_error(to_eplus(NULL, options = invalid), "terrain")
    invalid <- destep_opts()
    invalid$unknown <- TRUE
    expect_error(to_eplus(NULL, options = invalid), "fields")
    invalid <- destep_opts()
    invalid$people_heat <- NULL
    expect_error(to_eplus(NULL, options = invalid), "fields")
    invalid <- destep_opts()
    names(invalid)[2L] <- names(invalid)[1L]
    expect_error(to_eplus(NULL, options = invalid), "fields")
})

test_that("all feature dependencies are checked through the options constructor", {
    sky <- destep_opts(
        window_optics = "dest_solar",
        exterior_boundary = "dest_sky"
    )
    expect_equal(sky$source_distribution, "energyplus")
    expect_error(
        destep_opts("dest", surface_convection = "energyplus"),
        "requires surface_convection"
    )
    expect_error(
        destep_opts(partition_boundary = "dest_air"),
        "partition_boundary"
    )
    expect_error(
        destep_opts(
            source_distribution = "dest",
            surface_convection = "energyplus",
            partition_boundary = "dest_air"
        ),
        "partition_boundary"
    )
    expect_error(destep_opts(sky_radiation = FALSE), "sky_radiation")
    expect_error(destep_opts("dest", sky_radiation = 1), "sky_radiation")
    expect_error(destep_opts(weather = "model.epw"), "weather and directory")
    expect_error(destep_opts(directory = "prepass"), "weather and directory")
    expect_error(destep_opts(hvac_options = list()), "hvac_options")
    expect_error(destep_opts(hvac = "physical"), "hvac_options")
    physical <- destep_opts(
        hvac = "physical",
        hvac_options = list(chiller_nominal_cop = 4)
    )
    expect_equal(physical$hvac_options$chiller_nominal_cop, 4)
    expect_error(
        destep_opts("dest", hvac = "physical", hvac_options = list()),
        "ideal_loads"
    )
    expect_error(destep_opts("dest", weather = 1), "weather")
    expect_error(destep_opts("dest", directory = ""), "directory")
    supplied <- destep_opts(
        "dest",
        weather = "future.epw",
        directory = "prepass",
        sky_radiation = FALSE,
        partition_boundary = "dest_air"
    )
    expect_identical(conv__resolve_options(supplied), supplied)
    ep <- list(version = function() "26.1.0")
    packed <- conv__conversion_options(sky)$source_options
    expect_error(
        source__options(
            packed,
            ep,
            "ideal_loads",
            "dest_solar",
            FALSE,
            "energyplus"
        ),
        "weather"
    )
    expect_error(
        source__options(
            packed,
            ep,
            "ideal_loads",
            "simple_glazing",
            TRUE,
            "energyplus"
        ),
        "window_optics"
    )
})

test_that("conversion audits distinguish required input EMS from optional alignment", {
    ep <- eplusr::empty_idf("23.1")
    basic <- conv__conversion_options(destep_opts())
    expect_equal(nrow(conv__mode_audit(ep, basic)$ems), 0L)
    ep$add(
        "EnergyManagementSystem:Program" := list(
            name = "DeST_Moisture_1_Control",
            program_line_1 = "SET MassRate = 0.01"
        )
    )
    ep$add(
        "EnergyManagementSystem:Program" := list(
            name = "DeST_People_T_1_Control",
            program_line_1 = "SET Sensible = 60"
        )
    )
    audit <- conv__mode_audit(ep, basic)
    expect_equal(
        audit$ems$purpose,
        c("equipment_moisture", "temperature_dependent_people")
    )
    expect_equal(audit$ems$requirement, c("source_input", "optional_alignment"))
    expect_equal(
        audit$equipment_moisture_policy,
        "preserve_source_input_in_both_modes"
    )
    expect_match(
        conv__mode_comments(audit)[[2L]],
        "surface_convection=dest",
        fixed = TRUE
    )
    expect_match(
        conv__mode_comments(audit)[[3L]],
        "equipment_moisture:source_input",
        fixed = TRUE
    )
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
