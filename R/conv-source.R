# Validate the explicit receiving-face inventory before calculating any source
# shares. Areas are net receiving areas, including each participating window.
source__check_faces <- function(faces) {
    required <- c("name", "zone", "area", "category", "is_window", "epsilon", "construction")
    if (!is.data.frame(faces) || !all(required %in% names(faces)) ||
        !nrow(faces) || anyNA(faces[, required]) ||
        anyDuplicated(toupper(faces$name)) || any(!nzchar(faces$name)) ||
        any(!nzchar(faces$zone)) || !is.logical(faces$is_window) ||
        any(!is.finite(faces$area) | faces$area <= 0) ||
        any(!is.finite(faces$epsilon) | faces$epsilon <= 0 | faces$epsilon > 1) ||
        any(!faces$category %in% c("wall", "floor", "roof", "furniture")) ||
        any(faces$is_window & faces$category != "wall")) {
        stop("Invalid source receiving-face inventory.", call. = FALSE)
    }
    invisible(faces)
}

# The three surface entries determine both a literal radiant total and
# category-area weights. Do not replace a total below one with unity.
source__fractions <- function(faces, mode) {
    mode <- unlist(mode, use.names = TRUE)
    keys <- c("air", "wall", "floor", "roof")
    if (!is.numeric(mode) || length(mode) != 4L ||
        !setequal(names(mode), keys) || any(!is.finite(mode) | mode < 0) ||
        sum(mode) > 1 + 1e-6) {
        stop("Source fractions must be finite, nonnegative air/wall/floor/roof values summing to at most one.", call. = FALSE)
    }
    total <- sum(mode[c("wall", "floor", "roof")])
    # Convective furniture participates in the numerical radiant pool, but
    # receives no prescribed share of any source category.
    weight <- faces$area * unname(c(mode, furniture = 0)[faces$category])
    denominator <- sum(weight)
    if (denominator <= 0 && total > 0) {
        stop("Positive radiant source has no receiving area.", call. = FALSE)
    }
    # Only the recipient weights are normalized; the source total is retained.
    stats::setNames(if (denominator > 0) total * weight / denominator else
        rep(0, nrow(faces)), faces$name)
}

# Plan independent source streams using the EnergyPlus emissivity-area pool.
# A shared radiant carrier can meet window shares only when their ratios to
# that pool agree. Opaque shares are corrected separately in source__project.
source__plan <- function(faces, sources) {
    faces <- as.data.frame(faces)
    source__check_faces(faces)
    if (!is.list(sources) || !length(sources)) {
        stop("At least one explicit source is required.", call. = FALSE)
    }
    plan <- sources
    for (i in seq_along(plan)) {
        item <- plan[[i]]
        if (!is.null(item$unsupported_reason)) stop(item$unsupported_reason, call. = FALSE)
        required <- c("name", "zone", "kind", "design_power", "schedule", "mode",
            "existing_radiant", "existing_air")
        if (!all(required %in% names(item)) ||
            !is.character(item$kind) || length(item$kind) != 1L ||
            !item$kind %in% c("solar", "equipment", "light", "people") ||
            length(item$design_power) != 1L || !is.finite(item$design_power) ||
            item$design_power < 0 || length(item$schedule) != 1L ||
            !is.character(item$schedule) || !nzchar(item$schedule) ||
            length(item$zone) != 1L || !item$zone %in% faces$zone) {
            stop("Unsupported or incomplete prescribed source.", call. = FALSE)
        }
        if (item$kind == "solar" && item$design_power != 1) {
            stop("Solar schedules must supply watts with design_power = 1.", call. = FALSE)
        }
        if (item$kind == "people" && (length(item$sensible_heat) != 1L ||
            !is.finite(item$sensible_heat) || item$sensible_heat < 0 ||
            length(item$temperature_dependent) != 1L || !is.logical(item$temperature_dependent) ||
            is.na(item$temperature_dependent))) {
            stop("Unsupported people source: sensible heat and temperature mode are required.", call. = FALSE)
        }
        prior <- c(item$existing_radiant, item$existing_air)
        if (length(prior) != 2L || any(!is.finite(prior) | prior < 0) ||
            sum(prior) > 1 + 1e-6) {
            stop("Invalid existing radiant/air source fractions.", call. = FALSE)
        }
        zone_faces <- faces[faces$zone == item$zone, ]
        item$mode <- as.list(unlist(item$mode, use.names = TRUE))
        pool <- stats::setNames(zone_faces$area * zone_faces$epsilon /
            sum(zone_faces$area * zone_faces$epsilon), zone_faces$name)
        fractions <- source__fractions(zone_faces, item$mode)
        ratios <- fractions[zone_faces$is_window] / pool[zone_faces$is_window]
        if (length(ratios) && diff(range(ratios)) >= 1e-12) {
            stop("Mixed glazing requires independent radiant carriers.", call. = FALSE)
        }
        item$index <- i
        item$fractions <- fractions
        item$pool <- pool
        item$carrier <- if (length(ratios)) unname(ratios[[1L]]) else sum(fractions)
        item$sensor <- paste0("SourceSchedule", i)
        item$power_expression <- source__term(item, 1)
        plan[[i]] <- item
    }
    plan
}

# Keep numeric constants precise enough to retain the source database values.
source__number <- function(value) sprintf("%.17g", value)

# Express known source power without reading native loads or current gains.
# For people, design_power is the design count and sensible heat is W/person.
source__term <- function(item, coefficient) {
    term <- paste(source__number(coefficient * item$design_power), "*", item$sensor)
    if (item$kind == "people") {
        term <- paste(term, "*", if (item$temperature_dependent)
            paste0("SourceSensible", item$index) else source__number(item$sensible_heat))
    }
    term
}

# Find one named positional IDF object; case-insensitive lookup follows IDF
# reference semantics and rejects ambiguous inventories before modification.
source__index <- function(objects, class, name) {
    found <- which(vapply(objects, function(o) o[[1L]] == class &&
        toupper(o[[2L]]) == toupper(name), logical(1L)))
    if (length(found) != 1L) {
        stop(sprintf("Expected one %s object named '%s'.", class, name), call. = FALSE)
    }
    found
}

# Stop duplicate automatic solar delivery while preserving the outer pane's
# absorption: with transmission zero, reflection becomes its old value plus T.
source__block_solar <- function(objects) {
    windows <- Filter(function(o) o[[1L]] == "FenestrationSurface:Detailed", objects)
    changes <- list()
    processed <- character()
    for (name in unique(vapply(windows, `[[`, character(1L), 4L))) {
        construction <- objects[[source__index(objects, "Construction", name)]]
        outer <- objects[[source__index(objects, "WindowMaterial:Glazing", construction[[3L]])]]
        if (length(construction) != 5L || outer[[3L]] != "SpectralAndAngle") {
            stop("Prescribed solar requires ordinary three-layer optical-table windows.", call. = FALSE)
        }
        tname <- outer[[20L]]
        rname <- outer[[21L]]
        # Shared optical tables must be modified once even when several
        # constructions reference the same glazing material.
        pair <- paste(tname, rname, sep = "\r")
        if (pair %in% processed) next
        if (any(c(tname, rname) %in% unlist(strsplit(processed, "\r", fixed = TRUE)))) {
            stop("Partially shared transmission/reflection tables are unsupported.", call. = FALSE)
        }
        ti <- source__index(objects, "Table:Lookup", tname)
        ri <- source__index(objects, "Table:Lookup", rname)
        transmission <- as.numeric(objects[[ti]][-(1:11)])
        reflection <- as.numeric(objects[[ri]][-(1:11)])
        if (length(transmission) != 182L || length(reflection) != 182L ||
            any(!is.finite(c(transmission, reflection))) ||
            any(transmission < 0 | reflection < 0 | transmission + reflection > 1 + 1e-12)) {
            stop("Invalid solar optical tables.", call. = FALSE)
        }
        objects[[ti]][-(1:11)] <- "0"
        objects[[ri]][-(1:11)] <- source__number(transmission + reflection)
        changes[[length(changes) + 1L]] <- list(construction = name,
            transmittance_table = tname, front_reflectance_table = rname, points = 182L,
            maximum_front_absorptance_error = max(abs((1 - transmission - reflection) -
                (1 - (transmission + reflection)))))
        processed <- c(processed, pair)
    }
    list(objects = objects, changes = changes)
}

# Give signed incident corrections unit inner absorptance. Clone the receiving
# material so existing constructions, outdoor optics and thermal fields survive.
source__receivers <- function(objects, faces, zones) {
    material_names <- construction_names <- character()
    changes <- list()
    for (i in which(!faces$is_window & faces$category != "furniture" & faces$zone %in% zones)) {
        si <- source__index(objects, "BuildingSurface:Detailed", faces$name[[i]])
        surface <- objects[[si]]
        old <- objects[[source__index(objects, "Construction", surface[[4L]])]]
        if (length(old) == 3L && !(surface[[7L]] == "Surface" && surface[[9L]] == "NoSun")) {
            stop("Unsupported exposed single-layer solar receiver.", call. = FALSE)
        }
        inner <- utils::tail(old, 1L)
        mi <- which(vapply(objects, function(o) o[[1L]] %in% c("Material", "Material:NoMass") &&
            o[[2L]] == inner, logical(1L)))
        if (length(mi) != 1L) stop("Unsupported inner receiving material.", call. = FALSE)
        material <- objects[[mi]]
        if (!inner %in% names(material_names)) {
            clone <- material
            clone[[2L]] <- paste(inner, "Prescribed Solar Inner")
            clone[[if (clone[[1L]] == "Material") 9L else 6L]] <- "1"
            material_names[[inner]] <- clone[[2L]]
            objects[[length(objects) + 1L]] <- clone
        }
        if (!old[[2L]] %in% names(construction_names)) {
            clone <- old
            clone[[2L]] <- paste(old[[2L]], "Prescribed Solar")
            clone[[length(clone)]] <- material_names[[inner]]
            construction_names[[old[[2L]]]] <- clone[[2L]]
            objects[[length(objects) + 1L]] <- clone
        }
        surface[[4L]] <- construction_names[[old[[2L]]]]
        objects[[si]] <- surface
        faces$construction[[i]] <- surface[[4L]]
        changes[[length(changes) + 1L]] <- list(surface = surface[[2L]], before = old[[2L]], after = surface[[4L]])
    }
    list(objects = objects, faces = faces, changes = changes)
}

# Generate the verified source mapping without mutating an Idf or writing files.
# The caller supplies an audited optical prepass and owns its weather/file
# binding. This internal builder is intentionally not a public conversion mode.
source__project <- function(objects, faces, sources, solar_columns = list()) {
    faces <- as.data.frame(faces)
    plan <- source__plan(faces, sources)
    classes <- vapply(objects, `[[`, character(1L), 1L)
    if (any(classes %in% c("SurfaceProperty:SolarIncidentInside", "SurfaceProperty:HeatBalanceSourceTerm",
        "WindowShadingControl")) ||
        any(startsWith(classes, "Daylighting:"))) {
        stop("Unsupported existing gains, shading, daylighting or source correction objects.", call. = FALSE)
    }
    source__check_existing(objects, faces, plan)
    if (any(vapply(objects, function(o) length(o) >= 2L &&
        (grepl("^SOURCE( ALWAYS ON$|SCHEDULE[0-9]|EQUIPMENT(RADIANT|AIR|FLUX)[0-9]| SOLAR| OPPOSITE|NEIGHBOR|REMOTE|CORRECTION|TEMPERATURE|SENSIBLE)",
            toupper(o[[2L]])) || grepl("PRESCRIBED SOLAR", toupper(o[[2L]]), fixed = TRUE)), logical(1L)))) {
        stop("Source mapping object names already exist.", call. = FALSE)
    }
    sun <- Filter(function(s) s$kind == "solar", plan)
    solar_names <- vapply(sun, `[[`, character(1L), "schedule")
    if (!setequal(names(solar_columns), solar_names) || anyDuplicated(names(solar_columns)) ||
        (length(solar_columns) && (length(unique(lengths(solar_columns))) != 1L ||
        !length(solar_columns[[1L]]) || any(!is.finite(unlist(solar_columns)) | unlist(solar_columns) < 0)))) {
        stop("Solar columns must contain complete, aligned, nonnegative source powers.", call. = FALSE)
    }
    solar_zones <- unique(vapply(sun, `[[`, character(1L), "zone"))
    # Reject unrepresented windows before blocking their automatic delivery.
    windows <- faces[faces$is_window, ]
    if (length(sun) && any(!windows$zone %in% solar_zones)) {
        stop("Every window zone must have a prescribed solar source.", call. = FALSE)
    }
    optical <- if (length(sun)) source__block_solar(objects) else list(objects = objects, changes = list())
    receiving <- source__receivers(optical$objects, faces, solar_zones)
    objects <- receiving$objects
    faces <- receiving$faces
    columns <- solar_columns
    lines <- character()
    # Append positional IDF fields locally; callers receive the new inventory.
    add <- function(...) objects[[length(objects) + 1L]] <<- as.character(c(...))
    add("Schedule:Constant", "Source Always On", "", 1)
    for (item in plan) {
        i <- item$index
        add("EnergyManagementSystem:Sensor", item$sensor, item$schedule, "Schedule Value")
        add("Output:Variable", item$schedule, "Schedule Value", "Hourly")
        if (item$kind == "people" && item$temperature_dependent) {
            temperature <- paste0("SourceTemperature", i)
            sensible <- paste0("SourceSensible", i)
            add("EnergyManagementSystem:Sensor", temperature, item$zone, "Zone Mean Air Temperature")
            # Globals remain available when a large school requires several
            # ordered programs to stay within EnergyPlus's per-program limit.
            add("EnergyManagementSystem:GlobalVariable", sensible)
            lines <- c(lines, internal_gains__people_sensible_lines(item$sensible_heat,
                temperature, sensible))
        }
        if (item$kind == "solar") {
            add("OtherEquipment", paste("Source Solar Carrier", i), "None", item$zone,
                item$schedule, "EquipmentLevel", source__number(item$carrier), "", "", 0, 1, 0)
            add("OtherEquipment", paste("Source Solar Air", i), "None", item$zone,
                item$schedule, "EquipmentLevel", source__number(item$mode$air), "", "", 0, 0, 0)
        } else {
            for (radiant in c(TRUE, FALSE)) {
                name <- paste0(if (radiant) "SourceEquipmentRadiant" else "SourceEquipmentAir", i)
                coefficient <- if (radiant) item$carrier - item$existing_radiant else
                    item$mode$air - item$existing_air
                add("OtherEquipment", name, "None", item$zone, "Source Always On", "EquipmentLevel",
                    0, "", "", 0, as.integer(radiant), 0)
                add("EnergyManagementSystem:Actuator", name, name, "OtherEquipment", "Power Level")
                lines <- c(lines, paste("SET", name, "=", source__term(item, coefficient)))
            }
        }
    }
    for (i in which(!faces$is_window & faces$category != "furniture")) {
        face <- faces[i, ]
        selected <- Filter(function(s) s$zone == face$zone, plan)
        sun <- Filter(function(s) s$kind == "solar", selected)
        equipment <- Filter(function(s) s$kind != "solar", selected)
        if (length(sun)) {
            name <- paste("Source Solar Flux", i)
            values <- numeric(length(columns[[sun[[1L]]$schedule]]))
            for (item in sun) {
                coefficient <- (item$fractions[[face$name]] - item$carrier * item$pool[[face$name]]) / face$area
                values <- values + coefficient * columns[[item$schedule]]
            }
            columns[[name]] <- values
            add("SurfaceProperty:SolarIncidentInside", paste("Source Solar", i), face$name, face$construction, name)
        }
        if (length(equipment)) {
            terms <- vapply(equipment, function(item) source__term(item,
                (item$fractions[[face$name]] - item$carrier * item$pool[[face$name]]) / face$area), character(1L))
            name <- paste0("SourceEquipmentFlux", i)
            add("Schedule:Constant", name, "", 0)
            add("SurfaceProperty:HeatBalanceSourceTerm", face$name, name)
            add("EnergyManagementSystem:Actuator", name, name, "Schedule:Constant", "Schedule Value")
            lines <- c(lines, paste("SET", name, "=", paste(terms, collapse = " + ")))
        }
    }
    # Keep the complete shared wall and add only the neighbor's prescribed
    # source to its air boundary: Teq = Tair + Qprescribed / (A * h).
    shared <- Filter(function(o) o[[1L]] == "BuildingSurface:Detailed" && o[[7L]] == "Surface", objects)
    mapping <- list()
    for (i in seq_along(shared)) {
        wall <- shared[[i]]
        peer <- objects[[source__index(objects, "BuildingSurface:Detailed", wall[[8L]])]]
        face <- faces[faces$name == peer[[2L]], ]
        film <- objects[[source__index(objects, "SurfaceProperty:ConvectionCoefficients", peer[[2L]])]]
        h <- as.numeric(film[[5L]])
        if (nrow(face) != 1L || peer[[9L]] != "NoSun" || wall[[9L]] != "NoSun" ||
            film[[3L]] != "Inside" || film[[4L]] != "Value" || !is.finite(h) || h <= 0 ||
            any(vapply(objects, function(o) o[[1L]] == "FenestrationSurface:Detailed" &&
                o[[5L]] == wall[[2L]], logical(1L)))) {
            stop("Unsupported shared-wall source boundary.", call. = FALSE)
        }
        name <- paste("Source Opposite", i)
        air <- paste0("SourceNeighbor", i)
        flux <- paste0("SourceRemoteFlux", i)
        actuator <- paste0("SourceRemoteTemp", i)
        wi <- source__index(objects, "BuildingSurface:Detailed", wall[[2L]])
        objects[[wi]][7:8] <- c("OtherSideCoefficients", name)
        add("Schedule:Constant", name, "", 20)
        add("SurfaceProperty:OtherSideCoefficients", name, source__number(h), 20, 1, 0, 0, 0, 0, name)
        add("EnergyManagementSystem:Sensor", air, peer[[5L]], "Zone Mean Air Temperature")
        add("EnergyManagementSystem:GlobalVariable", flux)
        add("EnergyManagementSystem:Actuator", actuator, name, "Schedule:Constant", "Schedule Value")
        add("EnergyManagementSystem:OutputVariable", paste("Source Remote Air", i), air, "Averaged", "ZoneTimestep", "", "C")
        add("EnergyManagementSystem:OutputVariable", paste("Source Remote Flux", i), flux, "Averaged", "ZoneTimestep", "", "W/m2")
        terms <- vapply(Filter(function(s) s$zone == face$zone, plan), function(s)
            source__term(s, s$fractions[[face$name]] / face$area), character(1L))
        lines <- c(lines, paste("SET", flux, "=", if (length(terms)) paste(terms, collapse = " + ") else "0"),
            paste("SET", actuator, "=", air, "+", flux, "/", source__number(h)))
        add("Output:Variable", "*", paste("Source Remote Air", i), "Hourly")
        add("Output:Variable", "*", paste("Source Remote Flux", i), "Hourly")
        add("Output:Variable", name, "Schedule Value", "Hourly")
        mapping[[i]] <- list(surface = wall[[2L]], opposite_surface = peer[[2L]], opposite_zone = peer[[5L]],
            h = h, area = face$area, index = i, schedule = name)
    }
    if (length(lines)) {
        chunks <- split(lines, ceiling(seq_along(lines) / 450L))
        programs <- if (length(chunks) == 1L) "SourceCorrectionUpdate" else
            paste0("SourceCorrectionUpdate", seq_along(chunks))
        for (i in seq_along(chunks)) add("EnergyManagementSystem:Program", programs[[i]], chunks[[i]])
        add("EnergyManagementSystem:ProgramCallingManager", "Source Correction Manager",
            "BeginZoneTimestepBeforeInitHeatBalance", programs)
    }
    list(objects = objects, columns = columns, audit = list(faces = faces, sources = plan,
        partitions = mapping, optical_changes = optical$changes, receiver_changes = receiving$changes))
}

# Check cross-source coverage and reject unrepresented companion gains before
# generating corrections. Object-specific furniture checks stay with furniture.
source__check_existing <- function(objects, faces, sources) {
    kinds <- c(People = "people", Lights = "light", ElectricEquipment = "equipment")
    for (class in names(kinds)) {
        actual <- vapply(Filter(function(o) o[[1L]] == class, objects), `[[`, character(1L), 2L)
        expected <- vapply(Filter(function(s) s$kind == kinds[[class]], sources),
            `[[`, character(1L), "name")
        if (!setequal(actual, expected)) stop("Unsupported existing internal gain inventory.", call. = FALSE)
    }
    others <- vapply(Filter(function(o) o[[1L]] == "OtherEquipment", objects), `[[`, character(1L), 2L)
    allowed <- unlist(lapply(sources, `[[`, "companion_objects"), use.names = FALSE)
    if (!setequal(others, allowed)) stop("Unsupported existing sensible-source companions.", call. = FALSE)
    furniture__check_source(objects, faces)
    invisible(NULL)
}
