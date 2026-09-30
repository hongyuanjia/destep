test_that("sky settings distinguish saved values from explicit run overrides", {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    expect_error(sky__enabled(con), "Missing CAL_SKY")
    expect_identical(sky__enabled(con, TRUE)$effective, TRUE)
    DBI::dbWriteTable(con, "OPTION", data.frame(KEYWORD = "CAL_SKY_RADIATION", OPTION_STRING = "0"))
    expect_identical(sky__enabled(con), list(saved = FALSE, effective = FALSE, overridden = FALSE, override = NULL))
    expect_identical(sky__enabled(con, TRUE), list(saved = FALSE, effective = TRUE, overridden = TRUE, override = TRUE))
    DBI::dbExecute(con, "UPDATE OPTION SET OPTION_STRING = '1'")
    expect_true(sky__enabled(con)$effective)
    DBI::dbExecute(con, "UPDATE OPTION SET OPTION_STRING = 'unknown'")
    expect_error(sky__enabled(con, TRUE), "Invalid or ambiguous")
    ep <- list(version = function() "26.1.0")
    expect_error(source__options(list(sky_radiation = TRUE), ep, "ideal_loads", "dest_solar", FALSE),
        "requires exterior_boundary")
})

test_that("sky input mapping retains per-face fields and clipped identities", {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbWriteTable(con, "MAIN_ENCLOSURE", data.frame(ID = 1:2, SIDE1 = c(10L, 21L), SIDE2 = c(11L, 20L)))
    DBI::dbWriteTable(con, "WINDOW", data.frame(ID = 3L, SIDE1 = 31L, SIDE2 = 30L))
    DBI::dbWriteTable(con, "SURFACE", data.frame(SURFACE_ID = c(10L, 11L, 20L, 21L, 30L, 31L),
        TYPE = rep(c(1L, 0L), 3), TILT = c(90, 90, 0, 180, 90, 90),
        VENTILATION_COEF = c(23.3, 3.5, 8, 1, 3.5, 3.5), SKY_RADIA_COEF = c(4, 0, 5, 0, 2, 0)))
    surfaces <- data.frame(ID = c(11L, 21L, 21L), NAME = c("Wall", "Roof [1]", "Roof [2]"),
        TYPE = c("Wall", "Roof", "Roof"), BOUNDARY = "Outdoors", BOUNDARY_MODE = "source")
    windows <- data.frame(ID = 3L, NAME = "Window")
    faces <- sky__faces(con, surfaces, windows, TRUE)
    expect_equal(faces$HSKY, c(2, 5, 5, 1))
    expect_equal(faces$VENTILATION_COEF, c(23.3, 8, 8, 3.5))
    expect_true(all(sky__faces(con, surfaces, windows, FALSE)$HSKY == 0))
    surfaces$TYPE[2:3] <- "Floor"
    expect_equal(sky__faces(con, surfaces, windows, TRUE)$HSKY, c(2, 0, 0, 1))
    DBI::dbExecute(con, "UPDATE SURFACE SET TILT = 45 WHERE SURFACE_ID = 10")
    expect_error(sky__faces(con, surfaces, windows, TRUE), "vertical and horizontal")
    expect_equal(sky__output_key("Roof [2].+"), "Roof \\[2\\]\\.\\+")
    expect_equal(sky__output_key("Wall"), "Wall")
})

test_that("equivalent sky temperature exactly preserves the imposed heat flux", {
    air <- c(-18, 5, 32)
    sky <- c(-30, -4, 21)
    surface <- c(-3, 12, 40)
    for (hc in c(3.5, 23.3)) {
        for (hs in c(0, 2.0335, 4.067)) {
            equivalent <- sky__equivalent(air, sky, hc, hs)
            expect_equal((hc + hs) * (equivalent - surface),
                hc * (air - surface) + hs * (sky - surface), tolerance = 1e-12)
        }
    }
    expect_error(sky__equivalent(1:2, 3, 23.3, 4), "Invalid")
})

# A shared opaque material appears on both sides, so an outdoor modification
# must create a separate material and leave the inside original untouched.
sky__test_objects <- function() {
    list(c("Material", "Opaque", "Smooth", .2, 1, 1000, 1000, .85, .6, .6),
        c("Construction", "Wall Construction", "Opaque", "Opaque"),
        c("BuildingSurface:Detailed", "Wall", "Wall", "Wall Construction", "Room", "",
            "Outdoors", "", "SunExposed", "WindExposed", .5, 4,
            0, 0, 0, 5, 0, 0, 5, 0, 2, 0, 0, 2),
        c("SurfaceProperty:ConvectionCoefficients", "Wall", "Inside", "Value", 3.5,
            "", "", "Outside", "Value", 23.3),
        c("SurfaceProperty:SolarIncidentInside", "Input", "Wall", "Wall Construction", "Power"))
}

test_that("sky projection preserves thermal and solar fields and interior emissivity", {
    objects <- sky__test_objects()
    faces <- data.frame(ID = 1L, NAME = "Wall", OUTSIDE_ID = 2L, TILT = 90,
        VENTILATION_COEF = 23.3, SKY_RADIA_COEF = 4, FACTOR = .5, HSKY = 2)
    result <- sky__project(objects, faces, list(ENVIRONMENT = c(0, 10), WALL = c(10, 20)))
    expect_identical(result$objects[[1L]], objects[[1L]])
    material <- result$objects[[6L]]
    expect_equal(material[-c(2L, 8L)], objects[[1L]][-c(2L, 8L)])
    expect_equal(as.double(material[[8L]]), 1e-8)
    construction <- result$objects[[7L]]
    expect_equal(construction[[4L]], "Opaque")
    expect_equal(result$objects[[3L]][[4L]], construction[[2L]])
    expect_equal(result$objects[[5L]][[4L]], construction[[2L]])
    expect_equal(as.double(result$objects[[4L]][[10L]]), 25.3)
    expect_equal(result$objects[[4L]][[5L]], "3.5")
    expect_equal(result$columns[[1L]], (23.3 * c(10, 20) + 2 * c(0, 10)) / 25.3)
    expect_false(any(vapply(result$objects, function(o) startsWith(o[[1L]], "EnergyManagementSystem:"), logical(1L))))
    faces$HSKY <- 0
    disabled <- sky__project(objects, faces)
    expect_length(disabled$columns, 1L)
    expect_equal(disabled$columns[[1L]], 0)
    node <- Filter(function(o) o[[1L]] == "OutdoorAir:Node", disabled$objects)[[1L]]
    expect_equal(node[[4L]], node[[5L]])
    expect_equal(as.double(disabled$objects[[4L]][[10L]]), 23.3)
    objects[[2L]] <- objects[[2L]][1:3]
    expect_error(sky__project(objects, faces), "Single-layer")
})

test_that("window sky schedules use supported current-step weather actuators", {
    objects <- sky__test_objects()[1:4]
    objects <- c(list(c("Version", "26.1"), c("Zone", "Room"),
        c("Building", "Sky Test"),
        c("GlobalGeometryRules", "UpperLeftCorner", "CounterClockWise", "World")), objects,
        list(c("WindowMaterial:Glazing", "Glass", "SpectralAverage", "", .003,
            .7, .1, .1, .7, .1, .1, 0, .8, .8, 1),
            c("WindowMaterial:Gas", "Gap", "Air", .012),
            c("Construction", "Glazing", "Glass", "Gap", "Glass"),
            c("FenestrationSurface:Detailed", "Window", "Window", "Glazing", "Wall", "",
                .5, "", 1, 4, 1, 0, 0, 3, 0, 0, 3, 0, 1, 1, 0, 1),
            c("SurfaceProperty:ConvectionCoefficients", "Window", "Inside", "Value", 3.5,
                "", "", "Outside", "Value", 23.3)))
    faces <- data.frame(NAME = c("Wall", "Window"), VENTILATION_COEF = 23.3, HSKY = 2)
    result <- sky__project(objects, faces, list(ENVIRONMENT = c(0, 10),
        WALL = c(10, 20), WINDOW = c(15, 25)))
    expect_identical(result$faces[[2L]]$representation, "current_step_schedule_actuator")
    expect_false(result$faces[[2L]]$surface_temperature_feedback)
    sensors <- Filter(function(o) o[[1L]] == "EnergyManagementSystem:Sensor", result$objects)
    expect_length(sensors, 1L)
    expect_equal(sensors[[1L]][3:4], c(result$faces[[2L]]$schedule, "Schedule Value"))
    environments <- Filter(function(o) o[[1L]] == "SurfaceProperty:LocalEnvironment", result$objects)
    expect_equal(environments[[1L]][[3L]], "Wall")
    manager <- Filter(function(o) o[[1L]] == "EnergyManagementSystem:ProgramCallingManager", result$objects)
    expect_equal(manager[[1L]][[3L]], "BeginZoneTimestepBeforeInitHeatBalance")
    # Load against the official IDD: the prior local-environment window
    # reference failed this check even though the engine accepted it.
    for (name in names(result$columns)) result$objects[[length(result$objects) + 1L]] <-
        c("Schedule:Constant", name, "", 20)
    model <- source__model(result$objects, "26.1")
    expect_true(model$is_valid(level = "final"),
        info = paste(capture.output(model$validate(level = "final")), collapse = "\n"))
    actuator <- c("EnergyManagementSystem:Actuator", "Existing", "Window", "Surface",
        "Outdoor Air Drybulb Temperature")
    expect_error(sky__project(c(objects, list(actuator)), faces), "weather actuators conflict")
})
