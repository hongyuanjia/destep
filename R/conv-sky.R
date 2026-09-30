# Resolve the saved calculation switch separately from an explicit run override.
# bshell command-line switches are not stored in an exported geometry database.
sky__enabled <- function(dest, override = NULL) {
    saved <- NULL
    if ("OPTION" %in% DBI::dbListTables(dest) &&
        db_has_fields(dest, "OPTION", c("KEYWORD", "OPTION_STRING"))) {
        values <- DBI::dbGetQuery(dest, "SELECT OPTION_STRING FROM OPTION
            WHERE KEYWORD = 'CAL_SKY_RADIATION'")$OPTION_STRING
        if (length(values)) {
            if (length(values) != 1L || is.na(values) || !values %in% c("0", "1")) {
                stop("Invalid or ambiguous CAL_SKY_RADIATION option.", call. = FALSE)
            }
            saved <- values == "1"
        }
    }
    if (!is.null(override)) checkmate::assert_flag(override)
    if (is.null(saved) && is.null(override)) {
        stop("Missing CAL_SKY_RADIATION; set source_options$sky_radiation explicitly.", call. = FALSE)
    }
    list(saved = saved, effective = if (is.null(override)) saved else override,
        overridden = !is.null(override), override = override)
}

# Use only orientations independently checked with the installed DeST solver.
# The stored coefficient already includes blackness; never multiply it again.
sky__faces <- function(dest, surfaces, windows, enabled) {
    faces <- as.data.frame(data.table::rbindlist(list(
        surface__sky_faces(dest, surfaces), window__sky_faces(dest, windows)), fill = TRUE))
    if (!nrow(faces)) return(faces)
    fields <- c("OUTSIDE_ID", "TILT", "VENTILATION_COEF", "SKY_RADIA_COEF")
    for (field in fields) faces[[field]] <- suppressWarnings(as.double(faces[[field]]))
    if (anyNA(faces) || anyDuplicated(toupper(faces$NAME)) ||
        any(!is.finite(as.matrix(faces[, fields]))) ||
        any(faces$VENTILATION_COEF <= 0 | faces$SKY_RADIA_COEF < 0)) {
        stop("Invalid or incomplete per-face DeST sky inputs.", call. = FALSE)
    }
    horizontal_up <- abs(faces$TILT) < 1e-8
    vertical <- abs(faces$TILT - 90) < 1e-8
    horizontal_down <- abs(faces$TILT - 180) < 1e-8
    if (any(!(horizontal_up | vertical | horizontal_down))) {
        stop("DeST sky boundary supports vertical and horizontal faces only.", call. = FALSE)
    }
    # Exposed-floor pseudo-surfaces may store TILT = 0 despite their downward
    # physical face. The converter's explicit floor role takes precedence.
    faces$FACTOR <- ifelse(faces$TYPE == "Floor", 0,
        ifelse(vertical, .5, ifelse(horizontal_up, 1, 0)))
    faces$HSKY <- if (enabled) faces$FACTOR * faces$SKY_RADIA_COEF else 0
    faces
}

# Combine two prescribed linear exchanges without previous-step feedback.
# Surface temperature cancels from the equivalent environmental temperature.
sky__equivalent <- function(air, sky, hconv, hsky) {
    if (length(air) != length(sky) || !length(air) ||
        any(!is.finite(c(air, sky, hconv, hsky))) || length(hconv) != 1L ||
        length(hsky) != 1L || hconv <= 0 || hsky < 0) {
        stop("Invalid linear sky-boundary inputs.", call. = FALSE)
    }
    (hconv * air + hsky * sky) / (hconv + hsky)
}

# EnergyPlus accepts regular expressions for output keys. Escape every special
# character so tessellated names such as '[Part 1]' resolve as literal names.
sky__output_key <- function(name) {
    chars <- strsplit(name, "", fixed = TRUE)[[1L]]
    # Bare anchors do not trigger regex mode for an otherwise literal key in
    # EnergyPlus. Use the escaped name itself, as in its key-matching interface.
    paste0(ifelse(chars %in% c("\\", ".", "^", "$", "|", "(", ")",
        "[", "]", "{", "}", "*", "+", "?"), paste0("\\", chars), chars), collapse = "")
}

# Read the target engine's complete weather grid, including its existing
# height corrections, instead of implementing another interpolation algorithm.
sky__read_weather <- function(path, faces) {
    con <- DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    variables <- c("Site Sky Temperature", "Surface Outside Face Outdoor Air Drybulb Temperature")
    dictionary <- DBI::dbGetQuery(con, "SELECT ReportDataDictionaryIndex, KeyValue, Name, Units
        FROM ReportDataDictionary WHERE ReportingFrequency = 'Zone Timestep'")
    dictionary <- dictionary[dictionary$Name %in% variables, ]
    keys <- c("ENVIRONMENT", toupper(faces$NAME))
    if (nrow(dictionary) != length(keys) || anyDuplicated(toupper(dictionary$KeyValue)) ||
        !setequal(toupper(dictionary$KeyValue), keys) || any(dictionary$Units != "C")) {
        stop("Incomplete sky-weather output inventory.", call. = FALSE)
    }
    series <- lapply(keys, function(key) {
        id <- dictionary$ReportDataDictionaryIndex[match(key, toupper(dictionary$KeyValue))]
        values <- DBI::dbGetQuery(con, "SELECT T.SimulationDays,T.Hour,T.Minute,T.Interval,
            T.EnvironmentPeriodIndex,R.Value FROM ReportData R JOIN Time T USING(TimeIndex)
            JOIN EnvironmentPeriods E USING(EnvironmentPeriodIndex)
            WHERE R.ReportDataDictionaryIndex = ? AND E.EnvironmentType = 3
            AND COALESCE(T.WarmupFlag,0) = 0 ORDER BY R.TimeIndex", params = list(id))
        minutes <- (values$SimulationDays - 1L) * 1440L + values$Hour * 60L + values$Minute
        if (nrow(values) != 105120L || anyNA(values) || any(!is.finite(values$Value)) ||
            any(values$Interval != 5L) || length(unique(values$EnvironmentPeriodIndex)) != 1L ||
            !identical(as.integer(minutes), seq.int(5L, 525600L, 5L))) {
            stop("Sky weather requires a complete non-leap year at five-minute resolution.", call. = FALSE)
        }
        values$Value
    })
    stats::setNames(series, keys)
}

# Cache only input-weather observations; no native load or indoor state is used.
# Exact model, executable, external-file and implementation identities prevent
# stale weather schedules when geometry, weather or the local setup changes.
sky__weather <- function(ep, faces, weather, directory, verbose) {
    config <- eplusr::eplus_config(ep$version())
    if (is.null(config)) stop("A matching EnergyPlus engine is required.", call. = FALSE)
    dependencies <- solar__dependencies(ep, list(), weather, config)
    dependencies$sky <- list(faces = faces, code = lapply(c("sky__weather", "sky__read_weather",
        "sky__output_key"), function(name) paste(deparse(body(get(name,
            envir = environment(sky__weather))), width.cutoff = 500L), collapse = "\n")))
    root <- file.path(directory, paste0("sky-", source__fingerprint(dependencies)))
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
    root <- normalizePath(root, winslash = "/", mustWork = TRUE)
    manifest <- file.path(root, "manifest.rds")
    data <- file.path(root, "weather.rds")
    cached <- if (file.exists(manifest) && file.exists(data)) tryCatch({
        record <- readRDS(manifest)
        if (!identical(record$dependencies, dependencies) ||
            !identical(record$data_md5, unname(tools::md5sum(data)))) NULL else {
            value <- readRDS(data)
            if (!identical(names(value), c("ENVIRONMENT", toupper(faces$NAME))) ||
                any(lengths(value) != 105120L) ||
                any(vapply(value, function(x) !is.numeric(x) || any(!is.finite(x)), logical(1L)))) NULL else
                list(values = value, record = record)
        }
    }, error = function(e) NULL) else NULL
    if (!is.null(cached)) return(c(cached, list(directory = root, reused = TRUE)))
    folder <- tempfile("weather-", tmpdir = root)
    dir.create(folder)
    objects <- Filter(function(o) !grepl("^Output(:|Control:)", o[[1L]]), source__objects(ep))
    objects <- c(objects, list(c("Output:SQLite", "Simple"),
        c("OutputControl:Files", "No", "No", "No"),
        c("Output:Variable", "Environment", "Site Sky Temperature", "Timestep")),
        lapply(faces$NAME, function(name) c("Output:Variable", sky__output_key(name),
            "Surface Outside Face Outdoor Air Drybulb Temperature", "Timestep")))
    model <- source__model(objects, ep$version())
    path <- file.path(folder, "weather.idf")
    model$save(path, overwrite = FALSE, copy_external = TRUE)
    if (verbose) message("Reading the target sky-weather grid: ", folder)
    status <- processx::run(file.path(config$dir, config$exe),
        c("--weather", weather, "--output-directory", folder, path), wd = folder,
        error_on_status = FALSE, stdout = file.path(folder, "stdout.log"), stderr = file.path(folder, "stderr.log"))
    errors <- readLines(file.path(folder, "eplusout.err"), warn = FALSE)
    if (status$status != 0L || any(grepl("\\*\\* +(Severe|Fatal) +\\*\\*", errors)) ||
        !any(grepl("EnergyPlus Completed Successfully", errors, fixed = TRUE))) {
        stop("Sky-weather prepass failed; retained diagnostics: ", folder, call. = FALSE)
    }
    sql <- file.path(folder, "eplusout.sql")
    values <- sky__read_weather(sql, faces)
    after <- solar__dependencies(ep, list(), weather, config)
    after$sky <- dependencies$sky
    if (!identical(after, dependencies)) stop("Sky-weather inputs changed during execution.", call. = FALSE)
    saveRDS(values, data)
    record <- list(dependencies = dependencies, data_md5 = unname(tools::md5sum(data)),
        sql = sql, sql_md5 = unname(tools::md5sum(sql)))
    saveRDS(record, manifest)
    list(values = values, record = record, directory = root, reused = FALSE)
}

# Clone only each exposed layer and its construction. Opaque constructions
# need two layers to protect the room-side emissivity; glazing stores its
# front and back emissivities separately even with a single layer.
sky__project <- function(objects, faces, temperatures = NULL) {
    exposed <- Filter(function(o) (o[[1L]] == "BuildingSurface:Detailed" && o[[7L]] == "Outdoors") ||
        (o[[1L]] == "FenestrationSurface:Detailed" && o[[3L]] == "Window"), objects)
    if (!setequal(vapply(exposed, `[[`, character(1L), 2L), faces$NAME) ||
        any(vapply(objects, function(o) o[[1L]] == "SurfaceProperty:LocalEnvironment", logical(1L)))) {
        stop("Sky boundary requires a complete exterior inventory without existing local environments.", call. = FALSE)
    }
    columns <- list()
    ledger <- list()
    window_lines <- character()
    # A duplicate surface actuator would silently compete with the prescribed
    # boundary. Reject it before altering the model or running a weather pass.
    if (any(vapply(objects, function(o) o[[1L]] == "EnergyManagementSystem:Actuator" &&
        toupper(o[[3L]]) %in% toupper(faces$NAME) && toupper(o[[4L]]) == "SURFACE" &&
        toupper(o[[5L]]) %in% c("OUTDOOR AIR DRYBULB TEMPERATURE",
            "OUTDOOR AIR WETBULB TEMPERATURE"), logical(1L)))) {
        stop("Existing surface weather actuators conflict with the sky boundary.", call. = FALSE)
    }
    for (i in seq_len(nrow(faces))) {
        face <- faces[i, ]
        surface_index <- which(vapply(objects, function(o) o[[1L]] %in%
            c("BuildingSurface:Detailed", "FenestrationSurface:Detailed") && o[[2L]] == face$NAME, logical(1L)))
        if (length(surface_index) != 1L) stop("Ambiguous exterior face.", call. = FALSE)
        surface <- objects[[surface_index]]
        construction <- objects[[source__index(objects, "Construction", surface[[4L]])]]
        material <- Filter(function(o) o[[1L]] %in% c("Material", "Material:NoMass", "WindowMaterial:Glazing") &&
            o[[2L]] == construction[[3L]], objects)
        if (length(material) != 1L) stop("Unsupported exterior sky material.", call. = FALSE)
        material <- material[[1L]]
        if (length(construction) < 4L && !(surface[[1L]] == "FenestrationSurface:Detailed" &&
            material[[1L]] == "WindowMaterial:Glazing" && material[[3L]] == "SpectralAndAngle")) {
            stop("Single-layer sky constructions require an optical-table window.", call. = FALSE)
        }
        epsilon_index <- switch(material[[1L]], Material = 8L, "Material:NoMass" = 5L,
            "WindowMaterial:Glazing" = 13L)
        old_epsilon <- as.double(material[[epsilon_index]])
        film_index <- source__index(objects, "SurfaceProperty:ConvectionCoefficients", face$NAME)
        film <- objects[[film_index]]
        locations <- seq.int(3L, length(film), 5L)
        outside <- locations[film[locations] == "Outside"]
        if (length(outside) != 1L || film[[outside + 1L]] != "Value" ||
            abs(as.double(film[[outside + 2L]]) - face$VENTILATION_COEF) > 1e-8) {
            stop("Source/target exterior convection coefficient mismatch.", call. = FALSE)
        }
        prefix <- paste("DeST Sky", i)
        material[[2L]] <- paste(prefix, "Outside Material")
        # A positive value is required by the IDD. Keep its artificial longwave
        # residual below the fixed flux tolerance, including insulated floors.
        material[[epsilon_index]] <- "0.00000001"
        construction[[2L]] <- paste(prefix, "Construction")
        construction[[3L]] <- material[[2L]]
        objects[[surface_index]][[4L]] <- construction[[2L]]
        objects[[film_index]][[outside + 2L]] <- source__number(face$VENTILATION_COEF + face$HSKY)
        objects <- c(objects, list(material, construction))
        # Retain source-correction references if called on an already allocated
        # model. Only construction links change, never its incident-power table.
        for (j in seq_along(objects)) {
            if (objects[[j]][[1L]] == "SurfaceProperty:SolarIncidentInside" &&
                objects[[j]][[3L]] == face$NAME) objects[[j]][[4L]] <- construction[[2L]]
        }
        schedule <- NULL
        representation <- "source_air_no_sky"
        # Zero sky exchange still needs dry-bulb air on both temperature ports:
        # otherwise rain would switch EnergyPlus to its wet-bulb boundary.
        if (face$HSKY >= 0) {
            air <- if (is.null(temperatures)) 0 else temperatures[[toupper(face$NAME)]]
            sky <- if (is.null(temperatures)) 0 else temperatures$ENVIRONMENT
            value <- sky__equivalent(air, sky, face$VENTILATION_COEF, face$HSKY)
            match <- which(vapply(columns, identical, logical(1L), value))
            if (length(match)) schedule <- names(columns)[[match[[1L]]]] else {
                schedule <- paste("DeST Sky Equivalent", length(columns) + 1L)
                columns[[schedule]] <- value
            }
            if (surface[[1L]] == "FenestrationSurface:Detailed") {
                # The official LocalEnvironment object list excludes windows.
                # Copy the current schedule before heat-balance initialization;
                # no surface temperature is sensed and no lag is introduced.
                sensor <- paste0("DeSTSkyInput", i)
                dry <- paste0("DeSTSkyDry", i)
                wet <- paste0("DeSTSkyWet", i)
                objects <- c(objects, list(c("EnergyManagementSystem:Sensor", sensor, schedule, "Schedule Value"),
                    c("EnergyManagementSystem:Actuator", dry, face$NAME, "Surface", "Outdoor Air Drybulb Temperature"),
                    c("EnergyManagementSystem:Actuator", wet, face$NAME, "Surface", "Outdoor Air Wetbulb Temperature")))
                window_lines <- c(window_lines, paste("SET", dry, "=", sensor), paste("SET", wet, "=", sensor))
                representation <- "current_step_schedule_actuator"
            } else {
                node <- paste(prefix, "Outdoor Node")
                objects <- c(objects, list(c("OutdoorAir:Node", node, 0, schedule, schedule),
                    c("SurfaceProperty:LocalEnvironment", prefix, face$NAME, "", "", node)))
                representation <- "local_environment"
            }
        }
        ledger[[i]] <- c(as.list(face), list(original_outside_emissivity = old_epsilon,
            residual_outside_emissivity = 1e-8, construction = construction[[2L]], schedule = schedule,
            representation = representation, surface_temperature_feedback = FALSE))
    }
    # Bound the number of Erl lines per program for large window inventories.
    if (length(window_lines)) {
        chunks <- split(window_lines, ceiling(seq_along(window_lines) / 450L))
        programs <- paste0("DeSTSkyWeatherUpdate", seq_along(chunks))
        for (i in seq_along(chunks)) objects[[length(objects) + 1L]] <-
            c("EnergyManagementSystem:Program", programs[[i]], chunks[[i]])
        objects[[length(objects) + 1L]] <- c("EnergyManagementSystem:ProgramCallingManager",
            "DeST Sky Weather Manager", "BeginZoneTimestepBeforeInitHeatBalance", programs)
    }
    list(objects = objects, columns = columns, faces = ledger)
}

# Apply the opt-in sky boundary before source allocation so all downstream
# construction references are generated from the final boundary representation.
sky__apply <- function(dest, ep, surfaces, windows, options, verbose = FALSE) {
    enabled <- sky__enabled(dest, options$sky_radiation)
    faces <- sky__faces(dest, surfaces, windows, enabled$effective)
    if (!nrow(faces)) return(list(model = ep, audit = list(selection = enabled, faces = list())))
    objects <- source__objects(ep)
    sky__project(objects, faces)
    checkmate::assert_file_exists(options$weather)
    checkmate::assert_string(options$directory, min.chars = 1L)
    weather <- sky__weather(ep, faces, normalizePath(options$weather), options$directory, verbose)
    generated <- sky__project(objects, faces, weather$values)
    schedule_file <- NULL
    if (length(generated$columns)) {
        folder <- tempfile("boundary-", tmpdir = weather$directory)
        dir.create(folder)
        schedule_file <- file.path(folder, "equivalent-temperature.csv")
        data.table::fwrite(data.table::as.data.table(generated$columns), schedule_file)
        for (i in seq_along(generated$columns)) {
            generated$objects[[length(generated$objects) + 1L]] <- c("Schedule:File",
                names(generated$columns)[[i]], "", schedule_file, i, 1, 8760, "Comma", "No", 5, "No")
        }
    }
    result <- source__model(generated$objects, ep$version())
    if (!result$is_valid(level = "final")) stop("Invalid generated sky boundary.", call. = FALSE)
    list(model = result, audit = list(selection = enabled, faces = generated$faces,
        schedule_file = schedule_file, schedule_md5 = if (!is.null(schedule_file)) unname(tools::md5sum(schedule_file)),
        weather = if (!is.null(weather)) weather[setdiff(names(weather), "values")],
        interpretation = "Installed-DeST linear sky-only exterior boundary; residual default longwave retained at emissivity 1e-8"))
}
