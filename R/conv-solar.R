# Describe the simplified DeST optical stack independently of annual loads.
# The extra lossless medium represents SC; it is not an extra physical pane.
# This matches the independently checked DeST 0.2.230705 algorithm-0 model.
solar__media <- function(layers, index) {
    if (layers == 1L) {
        return(rbind(c(1.526, 0.0588), c(index, 0)))
    }
    media <- rbind(c(1.526, 0.0588), c(1, 0), c(index, 0), c(1.526, 0.0588))
    if (layers > 2L) {
        for (i in seq_len(layers - 2L)) {
            media <- rbind(media, c(1, 0), c(1.526, 0.0588))
        }
    }
    media
}

# Compute solar transmission and absorption using Snell/Fresnel/Beer relations
# and the layer recursion. These are optical input properties, not a replay
# of DeST solar position, diffuse transposition, or load integration algorithms.
solar__beam <- function(media, angle) {
    if (abs(angle) >= pi / 2 - 1e-6) {
        return(c(transmittance = 0, absorptance = 0))
    }
    indices <- c(1, media[, 1L], 1)
    extinction <- c(0, media[, 2L], 0)
    angles <- asin(sin(angle) / indices)
    count <- length(indices) - 1L
    if (abs(angle) < pi / 6) {
        reflection <- ((indices[-length(indices)] - indices[-1L]) /
            (indices[-length(indices)] + indices[-1L]))^2
    } else {
        left <- angles[-length(angles)]
        right <- angles[-1L]
        reflection <- 0.5 * ((sin(left - right) / sin(left + right))^2 +
            (tan(left - right) / tan(left + right))^2)
    }
    tau <- exp(-extinction / cos(angles))
    beta <- numeric(length(indices))
    alpha <- numeric(count)
    for (i in rev(seq_len(count))) {
        downstream <- beta[[i + 1L]] * tau[[i + 1L]]^2
        alpha[[i]] <- (1 - reflection[[i]]) /
            (1 - reflection[[i]] * downstream)
        beta[[i]] <- 1 - alpha[[i]] * (1 - downstream)
    }
    absorbed <- 0
    incoming <- 1
    for (i in seq_len(count)) {
        absorbed <- absorbed + incoming * alpha[[i]] * (1 - tau[[i + 1L]]) *
            (1 + tau[[i + 1L]] * beta[[i + 1L]])
        incoming <- incoming * alpha[[i]] * tau[[i + 1L]]
    }
    c(transmittance = prod(alpha) * prod(tau), absorptance = absorbed)
}

# Resolve SC's normal-transmittance objective with the retained bounded search.
# Saturated source values keep the source model's endpoint behavior; do not
# reinterpret the objective as SHGC or calibrate it against building loads.
solar__index <- function(sc, layers) {
    lower <- 1.526
    upper <- 20
    left <- 8.58244006874
    right <- 12.943559931260001
    objective <- function(index) {
        abs(solar__beam(solar__media(layers, index), 0)[[1L]] - 0.87 * sc)
    }
    left_error <- objective(left)
    right_error <- objective(right)
    while (abs(left - right) > 0.001) {
        if (right_error > left_error) {
            upper <- right
            right <- left
            right_error <- left_error
            left <- lower + 0.38196600999999997 * (upper - lower)
            left_error <- objective(left)
        } else {
            lower <- left
            left <- right
            left_error <- right_error
            right <- lower + 0.61803399 * (upper - lower)
            right_error <- objective(right)
        }
    }
    (left + right) / 2
}

# Add one object by positional IDD fields, including extensible table entries.
# The shared resolver uses the requested engine schema rather than fixed names.
solar__add_fields <- function(ep, class, fields) {
    names <- vapply(seq_along(fields), function(i) {
        conv__idd_field_name(ep, class, i)
    }, character(1L))
    # Combining positional fields with c() promotes numbers to strings. Restore
    # numeric literals before eplusr's type-strict object validation.
    values <- lapply(as.list(fields), function(value) {
        if (grepl("^[+-]?[0-9.]+([eE][+-]?[0-9]+)?$", value)) {
            as.double(value)
        } else {
            value
        }
    })
    names(values) <- names
    do.call(ep$add, stats::setNames(list(values), class))
    invisible(NULL)
}

# Represent a whole simplified window as an optical outer sheet, a constant-R
# gap, and a transparent inner sheet. This preserves the source glass resistance
# and its exposed emissivities while leaving EnergyPlus in charge of heat flow.
# It does not add the native solver's internal glass capacity or numerical lag.
solar__construction <- function(ep, name, sc, k, layers, emissivity = 0.84,
    inside_emissivity = emissivity
) {
    if (length(sc) != 1L || !is.finite(sc) || sc <= 0 || sc > 1 ||
        length(k) != 1L || !is.finite(k) || k <= 0 ||
        length(layers) != 1L || !is.finite(layers) ||
        !layers %in% c(2L, 3L) ||
        length(emissivity) != 1L || !is.finite(emissivity) ||
        emissivity <= 0 || emissivity >= 1 ||
        length(inside_emissivity) != 1L || !is.finite(inside_emissivity) ||
        inside_emissivity <= 0 || inside_emissivity >= 1) {
        stop("DeST solar optics requires SC in (0,1], positive K, two or three panes, and emissivity in (0,1).",
            call. = FALSE)
    }
    resistance <- 1 / k - 1 / 8.7 - 1 / 23.3
    if (resistance <= 2e-6) {
        stop("DeST solar optics requires positive glass resistance after removing nominal films.",
            call. = FALSE)
    }
    media <- solar__media(layers, solar__index(sc, layers))
    front <- vapply(0:90, function(a) solar__beam(media, a * pi / 180), numeric(2L))
    back <- vapply(0:90, function(a) solar__beam(media[nrow(media):1L, ], a * pi / 180), numeric(2L))
    prefix <- paste(name, "DeST Solar")
    solar__add_fields(ep, "Table:IndependentVariable", c(paste(prefix, "Angles"),
        "Linear", "Constant", 0, 90, "", "Dimensionless", "", "", "", 0:90))
    solar__add_fields(ep, "Table:IndependentVariable", c(paste(prefix, "Wavelengths"),
        "Linear", "Constant", 0.25, 2.5, "", "Dimensionless", "", "", "", 0.25, 2.5))
    independent <- paste(prefix, "Coordinates")
    solar__add_fields(ep, "Table:IndependentVariableList", c(independent,
        paste(prefix, "Angles"), paste(prefix, "Wavelengths")))
    tables <- list(T = front[1L, ], RF = 1 - colSums(front),
        RB = 1 - colSums(back), ClearT = rep(1, 91), ClearR = rep(0, 91))
    for (key in names(tables)) {
        # The last coordinate, wavelength, varies fastest in Table:Lookup.
        # The wavelength-independent values are solar-only; visible optics
        # cannot be inferred from this simplified solar parameterization.
        solar__add_fields(ep, "Table:Lookup", c(paste(prefix, key), independent,
            "DivisorOnly", 1, 0, 1, "Dimensionless", "", "", "",
            rep(tables[[key]], each = 2L)))
    }
    solar__add_fields(ep, "WindowMaterial:Glazing", c(paste(prefix, "Outer"),
        "SpectralAndAngle", "", 1e-6, rep("", 6L), 0,
        emissivity, 1e-6, 1, 1, "No", "", "", paste(prefix, "T"),
        paste(prefix, "RF"), paste(prefix, "RB")))
    solar__add_fields(ep, "WindowMaterial:Glazing", c(paste(prefix, "Inner"),
        "SpectralAndAngle", "", 1e-6, rep("", 6L), 0,
        1e-6, inside_emissivity, 1, 1, "No", "", "", paste(prefix, "ClearT"),
        paste(prefix, "ClearR"), paste(prefix, "ClearR")))
    solar__add_fields(ep, "WindowMaterial:Gas", c(paste(prefix, "Gap"), "Custom",
        0.001, 0.001 / (resistance - 2e-6), 0, 0, 0.000018, 0, 0,
        1006, 0, 0, 28.97, 1.4))
    new_name <- paste(prefix, "Construction")
    solar__add_fields(ep, "Construction", c(new_name, paste(prefix, "Outer"),
        paste(prefix, "Gap"), paste(prefix, "Inner")))
    data.frame(CONSTRUCTION = new_name, SC = sc, K = k, LAYERS = layers,
        GLASS_RESISTANCE = resistance, NORMAL_T = front[1L, 1L],
        NORMAL_A = front[2L, 1L], SOURCE_DIFFUSE_T = front[1L, 61L],
        SOURCE_DIFFUSE_A = front[2L, 61L], EXPOSED_EMISSIVITY = emissivity,
        INSIDE_EMISSIVITY = inside_emissivity)
}

# Replace only verified aggregate two/three-pane exterior-window constructions.
# Retain the legacy conversion as the default until whole-model regression is
# complete. This opt-in path never silently claims daylight or distribution
# equivalence; those are separate semantics and retain their existing warnings.
solar__apply <- function(dest, ep, source_distribution = "energyplus", windows = NULL) {
    if (!db_has_rows(dest, "WINDOW")) return(invisible(data.frame()))
    if (as.numeric_version(ep$version()) < as.numeric_version("23.1")) {
        stop("window_optics = 'dest_solar' currently requires EnergyPlus 23.1 or later.", call. = FALSE)
    }
    if (any(grepl("^Daylighting:", ep$class_name()))) {
        stop("DeST solar-only optical tables cannot be used with daylighting objects.", call. = FALSE)
    }
    if (!db_has_fields(dest, "WINDOW_TYPE_DATA", "LAYER_NUM")) {
        stop("DeST solar optics requires WINDOW_TYPE_DATA.LAYER_NUM.", call. = FALSE)
    }
    source <- const__window_type_performance(dest)
    layers <- DBI::dbGetQuery(dest, "SELECT ID AS TYPE_ID, LAYER_NUM FROM WINDOW_TYPE_DATA")
    source <- merge(source, layers, by = "TYPE_ID", all.x = TRUE)
    # Read-only Access exports and SQLite drivers can return integer-valued
    # fields as text. Normalize before the same strict pane-count validation.
    source[, LAYER_NUM := suppressWarnings(as.double(LAYER_NUM))]
    if (any(!source$TYPE_DATA_VALID) || anyNA(source$LAYER_NUM) ||
        any(!source$LAYER_NUM %in% c(2L, 3L))) {
        stop("DeST solar optics supports valid aggregate two/three-pane windows only.", call. = FALSE)
    }
    emissivity <- window__emissivity_table(dest, windows)
    source <- merge(source, emissivity, by.x = "WINDOW_ID", by.y = "ID", all.x = TRUE)
    window <- data.table::as.data.table(ep$to_table(class = "FenestrationSurface:Detailed", wide = TRUE))
    selected <- window[`Surface Type` == "Window"]
    boundary <- selected$`Outside Boundary Condition Object`
    construction_names <- c(source$TYPE_CONSTRUCTION_NAME,
        paste(source$TYPE_CONSTRUCTION_NAME, "[Reverse]"))
    if (any(!is.na(boundary) & nzchar(boundary)) ||
        any(!selected$`Construction Name` %in% construction_names)) {
        stop("DeST solar optics currently supports exterior aggregate windows only.", call. = FALSE)
    }
    if (anyNA(source[, .(NAME, OUTSIDE_EMISSIVITY, INSIDE_EMISSIVITY)]) ||
        anyDuplicated(source$NAME) || !setequal(source$NAME, selected$Name)) {
        stop("Cannot resolve the source emissivity of every converted window piece.", call. = FALSE)
    }
    # Optical type alone cannot identify a construction: individual windows
    # can have different inside/outside blackness, including clipped pieces.
    source[, EMISSIVITY_KEY := sprintf("%s/%.17g/%.17g", TYPE_ID,
        OUTSIDE_EMISSIVITY, INSIDE_EMISSIVITY)]
    variants <- unique(source, by = "EMISSIVITY_KEY")
    audits <- lapply(seq_len(nrow(variants)), function(i) {
        row <- variants[i]
        name <- row$TYPE_NAME
        if (sum(variants$TYPE_ID == row$TYPE_ID) > 1L) {
            name <- sprintf("%s [DeST e-out%.17g-in%.17g]", name,
                row$OUTSIDE_EMISSIVITY, row$INSIDE_EMISSIVITY)
        }
        audit <- solar__construction(ep, name, row$SC, row$K, row$LAYER_NUM,
            row$OUTSIDE_EMISSIVITY, row$INSIDE_EMISSIVITY)
        for (name in source[EMISSIVITY_KEY == row$EMISSIVITY_KEY, NAME]) {
            ep$object(name)$set(construction_name = audit$CONSTRUCTION)
        }
        audit$SOURCE_TYPE_ID <- row$TYPE_ID
        audit$SOURCE_VISIBLE_TRANSMITTANCE <- row$LIGHT_TRANS_RATIO
        audit
    })
    warning(paste("DeST solar optical tables preserve the simplified solar inputs;",
        "visible/daylighting optics are not represented. EnergyPlus retains its",
        if (source_distribution == "dest") "own diffuse integration and heat-balance solver."
        else "own angular fit, diffuse integration, heat balance and solar distribution."), call. = FALSE)
    invisible(data.table::rbindlist(audits))
}

# Retain the source distribution of each emitted window piece. Grouping is by
# zone and exact source values, so different window modes are never averaged.
solar__source_specs <- function(dest, windows, faces) {
    if (is.null(windows) || !nrow(windows)) return(list())
    required <- c("DIST_MODE_ID", "DIST_AIR", "DIST_AROUND", "DIST_FLOOR", "DIST_ROOF")
    if (!db_has_fields(dest, "WINDOW", c("ID", "SUN_TRANS_DIST_MODE")) ||
        !db_has_fields(dest, "DIST_MODE", required)) {
        stop("Missing window solar-distribution fields.", call. = FALSE)
    }
    modes <- DBI::dbGetQuery(dest, "SELECT W.ID, D.DIST_AIR, D.DIST_AROUND,
        D.DIST_FLOOR, D.DIST_ROOF FROM WINDOW W LEFT JOIN DIST_MODE D
        ON W.SUN_TRANS_DIST_MODE = D.DIST_MODE_ID")
    pieces <- unique(as.data.frame(windows)[, c("ID", "NAME")])
    streams <- list()
    keys <- character()
    for (i in seq_len(nrow(pieces))) {
        row <- modes[modes$ID == pieces$ID[[i]], ]
        face <- faces[faces$name == pieces$NAME[[i]] & faces$is_window, ]
        if (nrow(row) != 1L || anyNA(row) || nrow(face) != 1L) {
            stop("Cannot resolve a converted window's source solar distribution.", call. = FALSE)
        }
        mode <- stats::setNames(as.list(as.double(row[1L, -1L])), c("air", "wall", "floor", "roof"))
        # Validate literal fractions even when this window receives no sunlight.
        source__fractions(faces[faces$zone == face$zone, ], mode)
        key <- source__fingerprint(list(zone = face$zone, mode = mode))
        index <- match(key, keys)
        if (is.na(index)) {
            index <- length(streams) + 1L
            keys <- c(keys, key)
            name <- paste("Input Solar", index)
            streams[[index]] <- list(name = name, zone = face$zone, kind = "solar",
                design_power = 1, schedule = name, mode = mode, existing_radiant = 0,
                existing_air = 0, windows = character())
        }
        streams[[index]]$windows <- c(streams[[index]]$windows, face$name)
    }
    if (!setequal(unlist(lapply(streams, `[[`, "windows")), faces$name[faces$is_window])) {
        stop("Incomplete source-window inventory.", call. = FALSE)
    }
    streams
}
