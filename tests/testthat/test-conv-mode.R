test_that("conversion presets preserve explicit choices and legacy boundaries", {
    basic <- conv__mode_options("objects")
    expect_equal(basic$people_heat, "constant")
    expect_equal(basic$window_optics, "simple_glazing")
    expect_equal(basic$source_distribution, "energyplus")
    expect_equal(basic$surface_convection, "dest")
    expect_equal(basic$exterior_boundary, "energyplus")
    expect_null(basic$source_options)

    dest <- conv__mode_options("dest")
    expect_equal(dest$people_heat, "temperature_dependent")
    expect_equal(dest$window_optics, "dest_solar")
    expect_equal(dest$source_distribution, "dest")
    expect_equal(dest$exterior_boundary, "dest_sky")
    explicit <- conv__mode_options("dest", people_heat = "constant",
        source_distribution = "energyplus", exterior_boundary = "energyplus")
    expect_equal(explicit$people_heat, "constant")
    expect_equal(explicit$source_distribution, "energyplus")
    expect_equal(explicit$exterior_boundary, "energyplus")
    expect_equal(explicit$window_optics, "dest_solar")

    legacy <- conv__mode_options("objects", source_distribution = "dest",
        source_options = list(exterior_boundary = "dest_sky"))
    expect_equal(legacy$exterior_boundary, "dest_sky")
    legacy <- conv__mode_options("dest",
        source_options = list(exterior_boundary = "energyplus"))
    expect_equal(legacy$exterior_boundary, "energyplus")
    expect_error(conv__mode_options("dest", exterior_boundary = "energyplus",
        source_options = list(exterior_boundary = "dest_sky")), "Conflicting")
})

test_that("independent feature selection enforces real dependencies", {
    sky <- conv__mode_options("objects", window_optics = "dest_solar",
        exterior_boundary = "dest_sky")
    expect_equal(sky$source_distribution, "energyplus")
    expect_equal(sky$exterior_boundary, "dest_sky")
    expect_error(conv__mode_options("dest", surface_convection = "energyplus"),
        "requires surface_convection")
    expect_error(conv__mode_options("objects", source_options = list()), "source_options")
    expect_error(conv__mode_options("objects", source_distribution = "dest",
        surface_convection = "energyplus", source_options = list(partition_boundary = "dest_air")),
        "partition_boundary")
    ep <- list(version = function() "26.1.0")
    expect_error(source__options(sky$source_options, ep, "ideal_loads", "dest_solar",
        FALSE, "energyplus"), "weather")
    expect_error(source__options(sky$source_options, ep, "ideal_loads", "simple_glazing",
        TRUE, "energyplus"), "window_optics")
})

test_that("conversion audits distinguish required input EMS from optional alignment", {
    ep <- eplusr::empty_idf("23.1")
    basic <- conv__mode_options("objects")
    expect_equal(nrow(conv__mode_audit(ep, basic)$ems), 0L)
    ep$add("EnergyManagementSystem:Program" := list(
        name = "DeST_Moisture_1_Control", program_line_1 = "SET MassRate = 0.01"))
    ep$add("EnergyManagementSystem:Program" := list(
        name = "DeST_People_T_1_Control", program_line_1 = "SET Sensible = 60"))
    audit <- conv__mode_audit(ep, basic)
    expect_equal(audit$ems$purpose, c("equipment_moisture", "temperature_dependent_people"))
    expect_equal(audit$ems$requirement, c("source_input", "optional_alignment"))
    expect_equal(audit$equipment_moisture_policy, "preserve_source_input_in_both_modes")
    expect_match(conv__mode_comments(audit)[[2L]], "surface_convection=dest", fixed = TRUE)
    expect_match(conv__mode_comments(audit)[[3L]], "equipment_moisture:source_input", fixed = TRUE)
})
