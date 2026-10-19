test_that("omitted simulation settings preserve the generated object fields", {
    ep <- eplusr::empty_idf("23.1")
    ep$add(Building = list(name = "Keep defaults"))
    before <- ep$to_table()
    audit <- simulation__audit(ep)
    expect_identical(ep$to_table(), before)
    expect_equal(audit$effective$terrain, "Suburbs")
    expect_equal(audit$effective$solar_distribution, "FullExterior")
    expect_equal(audit$effective$shadow_update_days, 20L)
    expect_equal(audit$effective$shadow_update_method, "Periodic")
    expect_true(all(audit$selection == "retained_default"))
    expect_match(
        simulation__comments(audit)[1L],
        "retained defaults",
        fixed = TRUE
    )
})

test_that("response boundaries audit actual timesteps without changing outputs", {
    ep <- eplusr::empty_idf("23.1")
    ep$add(Building = list(name = "Response audit"))
    ep$add(Timestep = list(number_of_timesteps_per_hour = 12))
    before <- ep$to_table()
    audit <- simulation__audit(ep)
    expect_identical(ep$to_table(), before)
    expect_equal(audit$response$target_zone_timesteps_per_hour, 12L)
    expect_equal(audit$response$native_time_step_hours, 1L)
    comments <- simulation__comments(audit)
    expect_true(any(grepl("no automatic time shift", comments, fixed = TRUE)))
    expect_true(any(grepl("averaged hourly rates", comments, fixed = TRUE)))
    expect_true(any(grepl(
        "every unexplained residual",
        comments,
        fixed = TRUE
    )))
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
