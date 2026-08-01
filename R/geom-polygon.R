# Return the EnergyPlus geometry tolerances used by one conversion. Keeping the
# compatibility profile versioned prevents surface and subsurface algorithms
# from silently drifting to different numerical assumptions.
eplus_geom__profile <- function(version = "23.1") {
    version <- numeric_version(as.character(version))
    if (length(version) != 1L || is.na(version)) {
        stop("An EnergyPlus geometry profile needs exactly one valid version.")
    }
    reference_version <- numeric_version("23.1")
    validated <- isTRUE(version == reference_version)
    if (!validated) {
        warning(sprintf(
            paste(
                "EnergyPlus %s uses the geometry compatibility profile",
                "validated against EnergyPlus %s."
            ),
            as.character(version), as.character(reference_version)
        ), call. = FALSE)
    }
    list(
        version = version,
        reference_version = reference_version,
        validated = validated,
        coordinate_distance = 0.01,
        closure_vertex_distance = 0.0127,
        plane_distance = 1e-8,
        planarity_distance = 1e-6,
        boundary_inset_distance = 1e-8,
        area = 1e-6,
        zero_distance = 1e-12,
        normal_magnitude = 1e-12,
        angle = 1e-6,
        intersection = 1e-10,
        triangulation_max_states = 10000L
    )
}

# Extract one ordered polygon's XYZ matrix from either a data frame or an
# already numeric matrix. All shared geometry helpers use this one input check.
geom__coordinates <- function(value) {
    if (is.matrix(value)) {
        if (ncol(value) != 3L) {
            stop("A polygon coordinate matrix must have exactly three columns.")
        }
        return(matrix(as.double(value), ncol = 3L))
    }

    coordinate_columns <- c("POINT_X", "POINT_Y", "POINT_Z")
    if (!all(coordinate_columns %in% names(value))) {
        stop("A polygon must contain POINT_X, POINT_Y, and POINT_Z columns.")
    }
    as.matrix(as.data.frame(value)[coordinate_columns])
}

# Calculate the unnormalised Newell vector for an ordered 3-D polygon. Returning
# the raw vector lets callers derive area without inheriting normal-error policy.
geom__newell_vector <- function(value) {
    coordinate <- geom__coordinates(value)
    if (nrow(coordinate) < 3L) return(c(0.0, 0.0, 0.0))

    following <- seq_len(nrow(coordinate)) %% nrow(coordinate) + 1L
    c(
        sum((coordinate[, 2L] - coordinate[following, 2L]) *
            (coordinate[, 3L] + coordinate[following, 3L])),
        sum((coordinate[, 3L] - coordinate[following, 3L]) *
            (coordinate[, 1L] + coordinate[following, 1L])),
        sum((coordinate[, 1L] - coordinate[following, 1L]) *
            (coordinate[, 2L] + coordinate[following, 2L]))
    )
}

# Calculate the signed shoelace area of one ordered two-dimensional polygon.
geom__signed_area_2d <- function(value) {
    value <- as.matrix(value)
    if (nrow(value) < 3L || ncol(value) != 2L) return(0.0)
    following <- seq_len(nrow(value)) %% nrow(value) + 1L
    sum(
        value[, 1L] * value[following, 2L] -
            value[following, 1L] * value[, 2L]
    ) / 2.0
}

# Return the scalar two-dimensional cross product for the turn A -> B -> C.
geom__cross_2d <- function(a, b, c) {
    (b[[1L]] - a[[1L]]) * (c[[2L]] - a[[2L]]) -
        (b[[2L]] - a[[2L]]) * (c[[1L]] - a[[1L]])
}

# Derive the reusable geometric frame of one polygon: raw and unit normals,
# area, stable projection, signed projected area, and maximum planarity error.
geom__polygon_frame <- function(value, normal_tolerance = 1e-12) {
    coordinate <- geom__coordinates(value)
    newell <- geom__newell_vector(coordinate)
    magnitude <- sqrt(sum(newell ^ 2))
    valid <- nrow(coordinate) >= 3L && is.finite(magnitude) &&
        magnitude > normal_tolerance
    if (!valid) {
        return(list(
            valid = FALSE, coordinates = coordinate, newell = newell,
            magnitude = magnitude, area = magnitude / 2.0,
            normal = rep(NA_real_, 3L), projection = integer(),
            xy = matrix(numeric(), nrow = nrow(coordinate), ncol = 2L),
            signed_area = 0.0, planarity_error = Inf
        ))
    }

    normal <- newell / magnitude
    projection <- setdiff(1:3, which.max(abs(normal)))
    xy <- coordinate[, projection, drop = FALSE]
    origin <- coordinate[1L, ]
    planarity_error <- max(abs(as.vector(
        sweep(coordinate, 2L, origin, "-") %*% normal
    )))
    list(
        valid = TRUE, coordinates = coordinate, newell = newell,
        magnitude = magnitude, area = magnitude / 2.0, normal = normal,
        projection = projection, xy = xy,
        signed_area = geom__signed_area_2d(xy),
        planarity_error = planarity_error
    )
}

# Return a polygon unit normal while retaining the converter's explicit failure
# on degenerate geometry. Area-only and simplification callers use the raw frame.
geom__unit_normal <- function(value, normal_tolerance = 1e-12) {
    frame <- geom__polygon_frame(value, normal_tolerance)
    if (!frame$valid) {
        name <- if (!is.matrix(value) && "NAME" %in% names(value)) {
            value$NAME[[1L]]
        } else {
            "<unknown>"
        }
        stop(sprintf("DeST surface '%s' has a degenerate polygon.", name))
    }
    frame$normal
}

# Return the orientation-independent area of one ordered 3-D polygon.
geom__polygon_area <- function(value) {
    sqrt(sum(geom__newell_vector(value) ^ 2)) / 2.0
}

# Create a deterministic undirected key for each edge endpoint pair.
geom__edge_key <- function(start, end) {
    ifelse(
        start < end,
        paste(start, end, sep = "/"),
        paste(end, start, sep = "/")
    )
}

# Return the standard coordinate columns copied between geometry and metadata.
geom__coordinate_columns <- function() {
    c("POINT_NO", "POINT_X", "POINT_Y", "POINT_Z")
}

# Canonicalise polygon winding and start vertex in its stable two-dimensional
# projection. Opposite room-side copies then produce identical partitions.
geom__canonicalize_polygon <- function(value, normal_tolerance = 1e-12) {
    value <- data.table::copy(value)
    frame <- geom__polygon_frame(value, normal_tolerance)
    if (!frame$valid) return(value)

    if (frame$signed_area < 0.0) value <- value[nrow(value):1L]
    key <- sprintf(
        "%.12f|%.12f|%.12f",
        value$POINT_X, value$POINT_Y, value$POINT_Z
    )
    first <- order(key)[[1L]]
    order <- c(seq.int(first, nrow(value)), seq_len(first - 1L))
    value <- value[order]
    data.table::set(value, NULL, "POINT_NO", seq_len(nrow(value)) - 1L)
    value
}

# Return whether a planar four-vertex polygon is a rectangle within the shared
# EnergyPlus angular and distance tolerances.
geom__polygon_is_rectangle <- function(
    value, angle_tolerance = 1e-6, distance_tolerance = 0.01,
    planarity_tolerance = 1e-6
) {
    if (nrow(value) != 4L) return(FALSE)
    frame <- geom__polygon_frame(value)
    if (!frame$valid || frame$planarity_error > planarity_tolerance) return(FALSE)

    xy <- frame$xy
    following <- seq_len(nrow(xy)) %% nrow(xy) + 1L
    next_following <- following[following]
    edge <- xy[following, , drop = FALSE] - xy
    next_edge <- xy[next_following, , drop = FALSE] - xy[following, , drop = FALSE]
    edge_length <- sqrt(rowSums(edge ^ 2))
    next_length <- sqrt(rowSums(next_edge ^ 2))
    if (any(edge_length < distance_tolerance) ||
        any(next_length < distance_tolerance)) return(FALSE)

    right_angle <- abs(rowSums(edge * next_edge)) <=
        sin(angle_tolerance) * edge_length * next_length
    all(right_angle)
}

# Return whether all non-collinear turns of a planar polygon share one direction.
# The stable projection makes the predicate valid for any DeST surface tilt.
geom__polygon_is_convex <- function(value, angle_tolerance = 1e-6) {
    frame <- geom__polygon_frame(value)
    if (!frame$valid) geom__unit_normal(value)

    xy <- frame$xy
    previous <- c(nrow(xy), seq_len(nrow(xy) - 1L))
    following <- c(seq.int(2L, nrow(xy)), 1L)
    incoming <- xy - xy[previous, , drop = FALSE]
    outgoing <- xy[following, , drop = FALSE] - xy
    turn <- vapply(seq_len(nrow(xy)), function(index) {
        geom__cross_2d(
            xy[previous[[index]], ], xy[index, ], xy[following[[index]], ]
        )
    }, numeric(1L))
    scale <- sqrt(rowSums(incoming ^ 2)) * sqrt(rowSums(outgoing ^ 2))
    turn <- turn[abs(turn) > sin(angle_tolerance) * scale]
    length(turn) > 0L && (all(turn > 0.0) || all(turn < 0.0))
}

# Serialize one ordered polygon into EnergyPlus's repeated X/Y/Z vertex values.
geom__eplus_vertex_values <- function(value) {
    value <- value[order(value$POINT_NO)]
    coordinate <- geom__coordinates(value)
    as.list(as.double(t(coordinate)))
}

# Return the drawing-space south direction used to interpret DeST surface
# azimuths. Legacy databases without ENVIRONMENT use DeST's standard direction.
geom__south_direction <- function(dest) {
    if (!"ENVIRONMENT" %in% DBI::dbListTables(dest) ||
        !db__has_fields(dest, "ENVIRONMENT", "SOUTH_DIRECTION")) {
        return(270.0)
    }

    direction <- DBI::dbGetQuery(
        dest,
        "SELECT DISTINCT SOUTH_DIRECTION FROM ENVIRONMENT WHERE SOUTH_DIRECTION IS NOT NULL"
    )$SOUTH_DIRECTION
    if (length(direction) == 0L) return(270.0)
    if (length(direction) > 1L) {
        # One EnergyPlus Building object cannot represent several drawing axes.
        stop("Multiple DeST south directions cannot be represented in one EnergyPlus model.")
    }
    as.double(direction[[1L]]) %% 360.0
}

# Translate the DeST south-vector angle to EnergyPlus's clockwise rotation from
# true north to the model's positive Y axis.
geom__north_axis <- function(dest) {
    (geom__south_direction(dest) + 90.0) %% 360.0
}

# Convert DeST azimuth and tilt metadata into one drawing-coordinate unit normal.
geom__expected_surface_normal <- function(azimuth, tilt, south_direction) {
    if (length(azimuth) != 1L || length(tilt) != 1L ||
        is.na(azimuth) || is.na(tilt)) {
        stop("A DeST surface must have one azimuth and tilt to determine its orientation.")
    }
    # DeST uses sentinel azimuths for downward and upward horizontal faces.
    if (azimuth == 999.0) return(c(0.0, 0.0, -1.0))
    if (azimuth == -999.0) return(c(0.0, 0.0, 1.0))

    direction <- (south_direction - 180.0 - azimuth) * pi / 180.0
    inclination <- tilt * pi / 180.0
    c(
        cos(direction) * sin(inclination),
        sin(direction) * sin(inclination),
        cos(inclination)
    )
}

# Reverse a polygon only when its geometric normal opposes the DeST outward
# direction, while rejecting coordinates inconsistent with direction metadata.
geom__orient_surface_polygon <- function(
    surface, south_direction, profile = eplus_geom__profile()
) {
    surface <- data.table::copy(surface)
    expected <- geom__expected_surface_normal(
        surface$AZIMUTH[[1L]], surface$TILT[[1L]], south_direction
    )
    alignment <- sum(
        geom__unit_normal(surface, profile$normal_magnitude) * expected
    )
    if (abs(alignment) <= profile$plane_distance) {
        stop(sprintf(
            "DeST surface '%s' polygon is inconsistent with its azimuth and tilt.",
            surface$NAME[[1L]]
        ))
    }
    if (alignment < 0.0) surface <- surface[nrow(surface):1L]
    data.table::set(surface, NULL, "POINT_NO", seq_len(nrow(surface)) - 1L)
    surface
}
