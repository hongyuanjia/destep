# Build a conservative dependency identity for a weather-specific optical run.
# Model fields include geometry, optics, shading, schedules and run controls;
# file contents and executable/IDD contents are checked independently.
solar__dependencies <- function(ep, sources, weather, config) {
    libraries <- list.files(config$dir, pattern = "^(lib)?energyplusapi.*[.](dylib|so|dll)([.][0-9]+)*$",
        full.names = TRUE, ignore.case = TRUE)
    files <- unique(c(weather, ep$external_deps(), file.path(config$dir, config$exe),
        file.path(config$dir, "Energy+.idd"), libraries))
    if (anyNA(files) || any(!file.exists(files))) {
        stop("A solar prepass dependency is missing.", call. = FALSE)
    }
    files <- unique(normalizePath(files, winslash = "/", mustWork = TRUE))
    hashes <- unname(tools::md5sum(files))
    if (anyNA(hashes)) stop("Cannot fingerprint a solar prepass dependency.", call. = FALSE)
    # Include implementation bodies so development versions cannot reuse a
    # cache made by an older generator with the same package version number.
    functions <- c("solar__precompute", "solar__read_prepass", "solar__run_prepass",
        "solar__source_specs", "source__objects", "source__model")
    code <- lapply(functions, function(name) paste(deparse(body(get(name,
        envir = environment(solar__dependencies))), width.cutoff = 500L), collapse = "\n"))
    list(schema = 1L, model = source__objects(ep), sources = sources,
        files = stats::setNames(hashes, files), code = code)
}

# Execute an input-only optical prepass. It reports transmitted solar rather
# than DeST results, and leaves all thermal, geometric and control inputs alone.
solar__run_prepass <- function(ep, weather, directory, config, verbose = FALSE) {
    objects <- source__objects(ep)
    objects <- Filter(function(o) !o[[1L]] %in% c("Output:Variable", "Output:Meter",
        "Output:Meter:MeterFileOnly", "Output:SQLite", "Output:Table:SummaryReports"), objects)
    objects <- c(objects, list(c("Output:Variable", "*",
        "Surface Window Transmitted Solar Radiation Rate", "Timestep"),
        c("Output:SQLite", "Simple")))
    prepass <- source__model(objects, ep$version())
    path <- file.path(directory, "solar-prepass.idf")
    prepass$save(path, overwrite = FALSE, copy_external = TRUE)
    if (verbose) message("Running the weather-specific solar prepass: ", directory)
    status <- processx::run(file.path(config$dir, config$exe),
        c("--weather", weather, "--output-directory", directory, path),
        wd = directory, error_on_status = FALSE,
        stdout = file.path(directory, "stdout.log"), stderr = file.path(directory, "stderr.log"))
    error_file <- file.path(directory, "eplusout.err")
    errors <- if (file.exists(error_file)) readLines(error_file, warn = FALSE) else character()
    if (status$status != 0L || !length(errors) ||
        any(grepl("\\*\\* +(Severe|Fatal) +\\*\\*", errors)) ||
        !any(grepl("EnergyPlus Completed Successfully", errors, fixed = TRUE))) {
        stop("Solar prepass failed; retained diagnostics: ", directory, call. = FALSE)
    }
    file.path(directory, "eplusout.sql")
}

# Read a full, ordered non-leap-year series and reject duplicates, missing
# windows, altered timestep, warmup/design-day rows and nonphysical powers.
solar__read_prepass <- function(path, sources) {
    if (!file.exists(path)) stop("Missing solar prepass SQL.", call. = FALSE)
    con <- DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    inventory <- DBI::dbGetQuery(con, "SELECT ReportDataDictionaryIndex, KeyValue, Units
        FROM ReportDataDictionary WHERE Name = 'Surface Window Transmitted Solar Radiation Rate'
        AND ReportingFrequency = 'Zone Timestep'")
    keys <- toupper(unlist(lapply(sources, `[[`, "windows"), use.names = FALSE))
    if (anyDuplicated(keys) || anyDuplicated(toupper(inventory$KeyValue)) ||
        !setequal(toupper(inventory$KeyValue), keys) || any(inventory$Units != "W")) {
        stop("Solar prepass window inventory does not match the converted model.", call. = FALSE)
    }
    expected <- seq.int(5L, 8760L * 60L, 5L)
    columns <- lapply(sources, function(source) {
        power <- numeric(length(expected))
        for (window in source$windows) {
            id <- inventory$ReportDataDictionaryIndex[match(toupper(window), toupper(inventory$KeyValue))]
            values <- DBI::dbGetQuery(con, "SELECT T.SimulationDays, T.Hour, T.Minute, T.Interval,
                T.EnvironmentPeriodIndex, R.Value FROM ReportData R JOIN Time T USING(TimeIndex)
                JOIN EnvironmentPeriods E USING(EnvironmentPeriodIndex)
                WHERE R.ReportDataDictionaryIndex = ? AND E.EnvironmentType = 3 AND T.WarmupFlag = 0
                ORDER BY R.TimeIndex", params = list(id))
            minutes <- (values$SimulationDays - 1L) * 1440L + values$Hour * 60L + values$Minute
            if (nrow(values) != length(expected) || length(unique(values$EnvironmentPeriodIndex)) != 1L ||
                anyNA(values) || any(values$Interval != 5L) || !all(minutes == expected) ||
                any(!is.finite(values$Value) | values$Value < 0)) {
                stop("Solar prepass must contain one complete non-leap year at five-minute resolution.", call. = FALSE)
            }
            power <- power + values$Value
        }
        power
    })
    stats::setNames(columns, vapply(sources, `[[`, character(1L), "schedule"))
}

# Validate a cache's dependencies and its data bytes. Corrupt or stale records
# are ignored; a new run gets its own directory and never overwrites evidence.
solar__read_cache <- function(path, dependencies, sources) {
    manifest <- file.path(path, "manifest.rds")
    data <- file.path(path, "solar.rds")
    if (!file.exists(manifest) || !file.exists(data)) return(NULL)
    tryCatch({
        receipt <- readRDS(manifest)
        if (!identical(receipt$dependencies, dependencies) ||
            !identical(receipt$data_md5, unname(tools::md5sum(data)))) return(NULL)
        columns <- readRDS(data)
        names <- vapply(sources, `[[`, character(1L), "schedule")
        if (!is.list(columns) || !identical(names(columns), names) ||
            any(lengths(columns) != 8760L * 12L) ||
            any(vapply(columns, function(x) !is.numeric(x) || any(!is.finite(x) | x < 0), logical(1L)))) return(NULL)
        list(columns = columns, receipt = receipt)
    }, error = function(e) NULL)
}

# Cache only completed, verified prepasses. A fresh dependency check after the
# run catches input files edited while EnergyPlus was executing.
solar__precompute <- function(ep, sources, weather, directory, verbose = FALSE) {
    config <- eplusr::eplus_config(ep$version())
    if (is.null(config) || !file.exists(file.path(config$dir, config$exe))) {
        stop("An installed matching EnergyPlus engine is required for the solar prepass.", call. = FALSE)
    }
    dependencies <- solar__dependencies(ep, sources, weather, config)
    key <- source__fingerprint(dependencies)
    directory <- file.path(directory, key)
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(directory)) stop("Cannot create the solar cache directory.", call. = FALSE)
    directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
    cached <- solar__read_cache(directory, dependencies, sources)
    if (!is.null(cached)) {
        if (verbose) message("Reusing verified solar prepass: ", directory)
        return(list(columns = cached$columns, directory = directory, key = key,
            reused = TRUE, weather = weather, dependencies = dependencies,
            prepass = cached$receipt$prepass))
    }
    run <- tempfile("prepass-", tmpdir = directory)
    dir.create(run)
    sql <- solar__run_prepass(ep, weather, run, config, verbose)
    columns <- solar__read_prepass(sql, sources)
    if (!identical(dependencies, solar__dependencies(ep, sources, weather, config))) {
        stop("Solar prepass inputs changed during execution; result was not cached.", call. = FALSE)
    }
    data <- file.path(directory, "solar.rds")
    saveRDS(columns, data)
    receipt <- list(dependencies = dependencies, data_md5 = unname(tools::md5sum(data)),
        prepass = run, sql_md5 = unname(tools::md5sum(sql)))
    saveRDS(receipt, file.path(directory, "manifest.rds"))
    list(columns = columns, directory = directory, key = key, reused = FALSE,
        weather = weather, dependencies = dependencies, prepass = run)
}
