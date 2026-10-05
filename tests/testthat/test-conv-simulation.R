test_that("simulation settings reject malformed or ambiguous input early", {
    expect_identical(simulation__options(NULL), list())
    expect_identical(simulation__options(list()), list())
    expect_error(
        to_eplus(NULL, options = destep_opts(terrain = "Forest")),
        "terrain"
    )
    expect_error(simulation__options(list(unused = TRUE)), "Unknown")
    expect_error(simulation__options(list("Country")))
    expect_error(simulation__options(list(terrain = NULL)), "terrain")
    expect_error(simulation__options(list(
        terrain = "Country",
        terrain = "City"
    )))
    expect_error(
        simulation__options(list(solar_distribution = "dest")),
        "solar_distribution"
    )
    expect_error(
        simulation__options(list(shadow_update_days = 0)),
        "shadow_update_days"
    )
    expect_error(
        simulation__options(list(shadow_update_days = 1.5)),
        "shadow_update_days"
    )
})

test_that("omitted simulation settings preserve the generated object fields", {
    ep <- eplusr::empty_idf("23.1")
    ep$add(Building = list(name = "Keep defaults"))
    before <- ep$to_table()
    simulation__apply(ep, simulation__options(NULL))
    audit <- simulation__audit(ep, list())
    expect_identical(ep$to_table(), before)
    expect_equal(audit$effective$terrain, "Suburbs")
    expect_equal(audit$effective$solar_distribution, "FullExterior")
    expect_equal(audit$effective$shadow_update_days, 20L)
    expect_equal(audit$effective$shadow_update_method, "Periodic")
    expect_true(all(audit$selection == "retained_default"))
    expect_match(
        simulation__comments(audit)[[2L]],
        "retained defaults",
        fixed = TRUE
    )
})

test_that("explicit simulation settings update only their owning fields", {
    ep <- eplusr::empty_idf("23.1")
    ep$add(
        Building = list(
            name = "Preserve name",
            north_axis = 17,
            maximum_number_of_warmup_days = 27
        ),
        ShadowCalculation = list(
            shading_calculation_update_frequency_method = "Timestep",
            sky_diffuse_modeling_algorithm = "DetailedSkyDiffuseModeling"
        )
    )
    options <- simulation__options(list(
        terrain = "Country",
        solar_distribution = "FullInteriorAndExterior",
        shadow_update_days = 1L
    ))
    simulation__apply(ep, options)
    audit <- simulation__audit(ep, options)
    expect_equal(
        audit$effective,
        c(options, list(shadow_update_method = "Periodic"))
    )
    expect_equal(audit$requested, options)
    expect_true(all(audit$selection == "user"))
    expect_equal(ep$Building$value("North Axis")[[1L]], 17)
    expect_equal(ep$Building$value("Maximum Number of Warmup Days")[[1L]], 27L)
    expect_equal(
        ep$ShadowCalculation$value("Sky Diffuse Modeling Algorithm")[[1L]],
        "DetailedSkyDiffuseModeling"
    )
    expect_match(
        simulation__comments(audit)[[1L]],
        "shadow_update_days=1",
        fixed = TRUE
    )
})

test_that("shadow frequency uses actual current or legacy schema fields", {
    legacy <- simulation__shadow_fields(c(
        "Calculation Method",
        "Calculation Frequency"
    ))
    expect_equal(legacy$periodic, "AverageOverDaysInFrequency")
    expect_equal(legacy$frequency, "Calculation Frequency")
    current <- simulation__shadow_fields(c(
        "Shading Calculation Method",
        "Shading Calculation Update Frequency Method",
        "Shading Calculation Update Frequency"
    ))
    expect_equal(current$periodic, "Periodic")
    expect_equal(current$frequency, "Shading Calculation Update Frequency")
    expect_error(simulation__shadow_fields("Unknown Layout"), "Unsupported")
})
