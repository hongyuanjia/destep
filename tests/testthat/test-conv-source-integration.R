# A deliberately unequal wall/window pair tests gross-to-net area conversion.
# Positional fields use the EnergyPlus 26.1 schema required by the public mode.
source__integration_geometry <- function() {
    list(
        c("Material", "Opaque", "Smooth", .2, 1, 1000, 1000, .9, .6, .6),
        c("WindowMaterial:Glazing", "Glass", "SpectralAverage", "", .003,
            .7, .1, .1, .7, .1, .1, 0, .8, .8, 1),
        c("Construction", "Wall Construction", "Opaque", "Opaque"),
        c("Construction", "Window Construction", "Glass"),
        c("BuildingSurface:Detailed", "Wall", "Wall", "Wall Construction", "Room", "",
            "Outdoors", "", "SunExposed", "WindExposed", .5, 4,
            0, 0, 0, 5, 0, 0, 5, 0, 2, 0, 0, 2),
        c("FenestrationSurface:Detailed", "Window", "Window", "Window Construction", "Wall", "",
            .5, "", 1, 4, 1, 0, 0, 3, 0, 0, 3, 0, 1, 1, 0, 1))
}

test_that("automatic surface inventory uses net geometry and inside emissivity", {
    objects <- source__integration_geometry()
    faces <- surface__source_faces(objects)
    expect_equal(faces$area, c(8, 2))
    expect_equal(faces$epsilon, c(.9, .8))
    expect_equal(faces$zone, rep("Room", 2))
    expect_equal(sum(faces$area), 10)
    # An opening larger than its host must never create a negative recipient.
    objects[[6L]][11:22] <- as.character(c(0, 0, 0, 6, 0, 0, 6, 0, 2, 0, 0, 2))
    expect_error(surface__source_faces(objects), "receiving-face")
    objects <- source__integration_geometry()
    objects[[6L]][[3L]] <- "Door"
    expect_error(surface__source_faces(objects), "door receiving")
})

test_that("window sources retain database fractions and split-piece identities", {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con))
    DBI::dbWriteTable(con, "WINDOW", data.frame(ID = 1:2, SUN_TRANS_DIST_MODE = 1:2))
    DBI::dbWriteTable(con, "DIST_MODE", data.frame(DIST_MODE_ID = 1:2,
        DIST_AIR = c(.1, .2), DIST_AROUND = c(.4, .3), DIST_FLOOR = 0, DIST_ROOF = 0))
    faces <- surface__source_faces(source__integration_geometry())
    extra <- faces[2L, ]
    extra$name <- "Second window"
    faces <- rbind(faces, extra)
    windows <- data.frame(ID = 1:2, NAME = c("Window", "Second window"))
    streams <- solar__source_specs(con, windows, faces)
    expect_length(streams, 2L)
    expect_equal(streams[[1L]]$mode$wall, .4)
    expect_equal(sum(unlist(streams[[1L]]$mode)), .5)
    expect_equal(streams[[2L]]$mode$air, .2)
    DBI::dbExecute(con, "UPDATE WINDOW SET SUN_TRANS_DIST_MODE = 99 WHERE ID = 2")
    expect_error(solar__source_specs(con, windows, faces), "Cannot resolve")
})

test_that("public mode preserves defaults and rejects unsupported options early", {
    expect_identical(formals(to_eplus)$source_distribution, quote(c("energyplus", "dest")))
    ep <- list(version = function() "26.1.0")
    expect_identical(source__options(NULL, ep, "ideal_loads", "simple_glazing", FALSE),
        list(partition_boundary = "energyplus"))
    expect_error(source__options(list(unused = TRUE), ep, "ideal_loads", "dest_solar", FALSE), "Unknown")
    expect_error(source__options(NULL, ep, "physical", "dest_solar", FALSE), "ideal_loads")
    expect_error(source__options(NULL, ep, "ideal_loads", "simple_glazing", TRUE), "window_optics")
    expect_error(to_eplus(NULL, source_options = list()), "source_options")
})

test_that("generated objects reload without parsing fields as file paths", {
    objects <- list(c("Version", "23.1"),
        c("Schedule:Constant", "输入功率", "", "0.123456789012345"),
        c("EnergyManagementSystem:Program", "LongSourceProgram",
            rep("SET SourcePower = 0.123456789012345", 400)))
    # Long EMS programs previously triggered a path-length warning when the
    # complete IDF text was sent through the string-or-file loading interface.
    expect_warning(model <- source__model(objects, "23.1"), NA)
    actual <- unname(source__objects(model))
    expect_equal(actual, objects)
    expect_warning(reloaded <- source__model(actual, "23.1"), NA)
    expect_equal(unname(source__objects(reloaded)), objects)
})

test_that("source allocation preserves coupled partitions unless explicitly selected", {
    objects <- source__integration_geometry()[c(1L, 3L, 5L)]
    objects[[3L]][7:10] <- c("Surface", "Peer", "NoSun", "NoWind")
    peer <- objects[[3L]]
    peer[c(2L, 5L, 8L)] <- c("Peer", "Neighbor", "Wall")
    objects <- c(objects, list(peer, c("ElectricEquipment", "Gain"),
        c("SurfaceProperty:ConvectionCoefficients", "Wall", "Inside", "Value", 8.7),
        c("SurfaceProperty:ConvectionCoefficients", "Peer", "Inside", "Value", 8.7)))
    faces <- surface__source_faces(objects)
    sources <- list(list(name = "Gain", zone = "Room", kind = "equipment", design_power = 200,
        schedule = "Gain Schedule", mode = list(air = .2, wall = .8, floor = 0, roof = 0),
        existing_radiant = .8, existing_air = .2))
    coupled <- source__project(objects, faces, sources, partition_boundary = "energyplus")
    expect_equal(coupled$objects[[3L]][7:8], c("Surface", "Peer"))
    expect_length(coupled$audit$partitions, 0L)
    selected <- source__project(objects, faces, sources, partition_boundary = "dest_air")
    expect_equal(selected$objects[[3L]][[7L]], "OtherSideCoefficients")
    expect_length(selected$audit$partitions, 2L)
})

test_that("cache identity follows file contents and emitted model fields", {
    root <- tempfile("solar-dependencies-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE))
    paths <- file.path(root, c("weather.epw", "schedule.csv", "engine", "Energy+.idd", "libenergyplusapi.so"))
    for (path in paths) writeLines("original", path)
    values <- data.frame(id = 1L, class = "Building", index = 1L, value = "Model")
    ep <- list(to_table = function() values, external_deps = function() paths[[2L]])
    config <- list(dir = root, exe = "engine")
    initial <- solar__dependencies(ep, list(), paths[[1L]], config)
    for (path in paths) {
        writeLines("modified", path)
        changed <- solar__dependencies(ep, list(), paths[[1L]], config)
        expect_false(identical(source__fingerprint(initial), source__fingerprint(changed)))
        writeLines("original", path)
    }
    expect_identical(solar__dependencies(ep, list(), paths[[1L]], config), initial)
    values$value <- "Different geometry or controls"
    expect_false(identical(source__fingerprint(initial),
        source__fingerprint(solar__dependencies(ep, list(), paths[[1L]], config))))
})

test_that("cache checks typed dependencies, data bytes and complete series", {
    path <- tempfile("solar-cache-")
    dir.create(path)
    on.exit(unlink(path, recursive = TRUE))
    sources <- list(list(schedule = "Input Solar 1"))
    dependencies <- list(weather = "original", geometry = c(1, 2), optics = .5)
    data <- file.path(path, "solar.rds")
    saveRDS(list("Input Solar 1" = rep(0, 8760L * 12L)), data)
    receipt <- list(dependencies = dependencies, data_md5 = unname(tools::md5sum(data)))
    saveRDS(receipt, file.path(path, "manifest.rds"))
    expect_false(is.null(solar__read_cache(path, dependencies, sources)))
    for (field in names(dependencies)) {
        changed <- dependencies
        changed[[field]] <- "changed"
        expect_null(solar__read_cache(path, changed, sources))
    }
    saveRDS(list("Input Solar 1" = rep(1, 8760L * 12L)), data)
    expect_null(solar__read_cache(path, dependencies, sources))
    # Even a matching receipt must not admit a truncated or non-finite series.
    for (values in list(0, rep(NA_real_, 8760L * 12L), rep(-1, 8760L * 12L))) {
        saveRDS(list("Input Solar 1" = values), data)
        receipt$data_md5 <- unname(tools::md5sum(data))
        saveRDS(receipt, file.path(path, "manifest.rds"))
        expect_null(solar__read_cache(path, dependencies, sources))
    }
})

# Build the minimal SQL contract independently from the prepass reader. The
# last timestamp is 24:00; every other record uses EnergyPlus hour/minute fields.
solar__test_sql <- function(path) {
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    n <- 8760L * 12L
    minute <- seq_len(n) * 5L
    DBI::dbWriteTable(con, "ReportDataDictionary", data.frame(ReportDataDictionaryIndex = 1L,
        KeyValue = "WINDOW", Units = "W", Name = "Surface Window Transmitted Solar Radiation Rate",
        ReportingFrequency = "Zone Timestep"))
    DBI::dbWriteTable(con, "EnvironmentPeriods", data.frame(EnvironmentPeriodIndex = 1L, EnvironmentType = 3L))
    DBI::dbWriteTable(con, "Time", data.frame(TimeIndex = seq_len(n),
        SimulationDays = (minute - 1L) %/% 1440L + 1L,
        Hour = ((minute - 1L) %% 1440L + 1L) %/% 60L,
        Minute = minute %% 60L, Interval = 5L, EnvironmentPeriodIndex = 1L, WarmupFlag = 0L))
    DBI::dbWriteTable(con, "ReportData", data.frame(TimeIndex = seq_len(n),
        ReportDataDictionaryIndex = 1L, Value = seq_len(n) / n))
    con
}

test_that("prepass SQL must have exactly one aligned full-year record per window", {
    path <- tempfile(fileext = ".sql")
    con <- solar__test_sql(path)
    on.exit({ DBI::dbDisconnect(con); unlink(path) })
    sources <- list(list(schedule = "Solar", windows = "Window"))
    values <- solar__read_prepass(path, sources)
    expect_length(values$Solar, 105120L)
    expect_equal(tail(values$Solar, 1L), 1)
    DBI::dbExecute(con, "UPDATE Time SET Interval = 10 WHERE TimeIndex = 50")
    expect_error(solar__read_prepass(path, sources), "five-minute")
    DBI::dbExecute(con, "UPDATE Time SET Interval = 5 WHERE TimeIndex = 50")
    DBI::dbExecute(con, "DELETE FROM ReportData WHERE TimeIndex = 100")
    expect_error(solar__read_prepass(path, sources), "complete non-leap")
})
