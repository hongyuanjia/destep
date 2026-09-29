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
solar__construction <- function(ep, name, sc, k, layers, emissivity = 0.84) {
    if (length(sc) != 1L || !is.finite(sc) || sc <= 0 || sc > 1 ||
        length(k) != 1L || !is.finite(k) || k <= 0 ||
        length(layers) != 1L || !is.finite(layers) ||
        !layers %in% c(2L, 3L) ||
        length(emissivity) != 1L || !is.finite(emissivity) ||
        emissivity <= 0 || emissivity >= 1) {
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
        1e-6, emissivity, 1, 1, "No", "", "", paste(prefix, "ClearT"),
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
        SOURCE_DIFFUSE_A = front[2L, 61L], EXPOSED_EMISSIVITY = emissivity)
}

# Replace only verified aggregate two/three-pane exterior-window constructions.
# Retain the legacy conversion as the default until whole-model regression is
# complete. This opt-in path never silently claims daylight or distribution
# equivalence; those are separate semantics and retain their existing warnings.
solar__apply <- function(dest, ep) {
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
    source <- unique(source, by = "TYPE_ID")
    window <- data.table::as.data.table(ep$to_table(class = "FenestrationSurface:Detailed", wide = TRUE))
    selected <- window[`Surface Type` == "Window"]
    boundary <- selected$`Outside Boundary Condition Object`
    construction_names <- c(source$TYPE_CONSTRUCTION_NAME,
        paste(source$TYPE_CONSTRUCTION_NAME, "[Reverse]"))
    if (any(!is.na(boundary) & nzchar(boundary)) ||
        any(!selected$`Construction Name` %in% construction_names)) {
        stop("DeST solar optics currently supports exterior aggregate windows only.", call. = FALSE)
    }
    audits <- lapply(seq_len(nrow(source)), function(i) {
        row <- source[i]
        audit <- solar__construction(ep, row$TYPE_NAME, row$SC, row$K, row$LAYER_NUM)
        # A source SIDE1/SIDE2 reversal changes the geometric construction
        # suffix even for exterior windows. Absorption remains on the physical
        # outdoor sheet for both suffixes; interzone windows are rejected above.
        old_names <- c(row$TYPE_CONSTRUCTION_NAME,
            paste(row$TYPE_CONSTRUCTION_NAME, "[Reverse]"))
        for (name in selected[`Construction Name` %in% old_names, Name]) {
            ep$object(name)$set(construction_name = audit$CONSTRUCTION)
        }
        audit$SOURCE_TYPE_ID <- row$TYPE_ID
        audit$SOURCE_VISIBLE_TRANSMITTANCE <- row$LIGHT_TRANS_RATIO
        audit
    })
    warning(paste("DeST solar optical tables preserve the simplified solar inputs;",
        "visible/daylighting optics are not represented. EnergyPlus retains its",
        "own angular fit, diffuse integration, heat balance and solar distribution."), call. = FALSE)
    invisible(data.table::rbindlist(audits))
}
