# Read active DeST window-shading parameters with their library dimensions.
# Rows with SC == 0 are inactive even when stale SHADING records remain linked.
shading__source_table <- function(dest) {
    required <- list(
        WINDOW = c("ID", "NAME", "SC", "SHADINGID"),
        SHADING = c("ID", "TAO", "ROU", "LIB_SHADING_ID"),
        LIB_SHADING = c(
            "ID", "B0", "B1", "B2", "DEG", "DIST", "W", "N",
            "HF", "HL", "WL", "DEGL", "HR", "WR", "DEGR"
        )
    )
    if (!all(names(required) %in% DBI::dbListTables(dest)) ||
            !all(vapply(names(required), function(table) {
                db_has_fields(dest, table, required[[table]])
            }, logical(1L)))) {
        return(data.table::data.table())
    }

    shading <- data.table::as.data.table(DBI::dbGetQuery(dest, "
        SELECT
            W.ID AS WINDOW_ID,
            W.NAME AS WINDOW_NAME,
            W.SC,
            W.SHADINGID,
            S.TAO,
            S.ROU,
            S.LIB_SHADING_ID,
            L.B0,
            L.B1,
            L.B2,
            L.DEG,
            L.DIST,
            L.W,
            L.N,
            L.HF,
            L.HL,
            L.WL,
            L.DEGL,
            L.HR,
            L.WR,
            L.DEGR
        FROM WINDOW W
        LEFT JOIN SHADING S ON W.SHADINGID = S.ID
        LEFT JOIN LIB_SHADING L ON S.LIB_SHADING_ID = L.ID
        WHERE W.SC > 0
        ORDER BY W.ID
    "))
    if (nrow(shading) == 0L) return(shading)

    duplicate <- shading[, .N, by = "WINDOW_ID"][N != 1L, WINDOW_ID]
    if (length(duplicate) > 0L) {
        stop("A DeST window must resolve to exactly one active shading definition.")
    }
    numeric_fields <- setdiff(names(shading), c("WINDOW_NAME"))
    dt_force_numeric(shading, numeric_fields)
    if (anyNA(shading)) {
        stop("An active DeST window shading definition is incomplete.")
    }
    shading
}

# Enforce the limited DeST shading family covered by the current validation
# evidence. Unsupported shapes fail explicitly so conversion cannot silently
# invent them.
shading__validate_parameters <- function(shading, tolerance = 1e-7) {
    if (nrow(shading) == 0L) return(invisible(shading))
    unsupported_angle <- apply(
        abs(as.matrix(shading[, .(DEG, DEGL, DEGR)])) > tolerance,
        1L,
        any
    )
    non_opaque <- abs(shading$TAO) > tolerance
    nonzero_header_fin <- abs(shading$HF) > tolerance
    asymmetric <- abs(shading$B1 - shading$B2) > tolerance |
        abs(shading$HL - shading$HR) > tolerance |
        abs(shading$WL - shading$WR) > tolerance
    dimensions <- c("B0", "B1", "B2", "DIST", "W", "N", "HF",
        "HL", "WL", "HR", "WR")
    negative <- apply(as.matrix(shading[, ..dimensions]) < -tolerance, 1L, any)
    inconsistent_fin <- xor(
        shading$HL > tolerance,
        shading$WL > tolerance
    ) | xor(shading$HR > tolerance, shading$WR > tolerance)
    empty <- shading$W <= tolerance &
        shading$WL <= tolerance & shading$WR <= tolerance
    unsupported <- unsupported_angle | non_opaque | nonzero_header_fin |
        asymmetric | negative | inconsistent_fin | empty
    if (any(unsupported)) {
        stop(sprintf(
            paste(
                "Unsupported DeST window shading geometry for window(s): %s.",
                "Only opaque, symmetric, zero-rotation overhangs and side fins",
                "on one axis-aligned window are currently validated."
            ),
            paste(shading$WINDOW_NAME[unsupported], collapse = ", ")
        ), call. = FALSE)
    }
    invisible(shading)
}

# Package one four-vertex shading polygon as EnergyPlus extensible fields.
shading__object_values <- function(name, base_surface, vertices) {
    value <- list(
        name = name,
        base_surface_name = base_surface,
        transmittance_schedule_name = "",
        number_of_vertices = 4L
    )
    for (vertex in seq_len(4L)) {
        for (axis in seq_len(3L)) {
            value[[paste0(
                "vertex_", vertex, "_", c("x", "y", "z")[[axis]],
                "_coordinate"
            )]] <- vertices[vertex, axis]
        }
    }
    value
}

# Build the validated EnergyPlus overhang and symmetric side-fin polygons for
# one final, unsplit exterior window.
shading__window_objects <- function(window, parameter, profile) {
    tolerance <- profile$intersection
    coordinates <- as.matrix(window[, .(POINT_X, POINT_Y, POINT_Z)])
    spreads <- apply(coordinates[, 1:2, drop = FALSE], 2L, function(x) {
        diff(range(x))
    })
    plane_axis <- which(spreads <= tolerance)
    frame <- geom__polygon_frame(window, profile$normal_magnitude)
    vertical <- frame$valid && abs(frame$normal[[3L]]) <= profile$angle
    if (length(plane_axis) != 1L || !vertical) {
        stop(sprintf(
            "DeST shading for window '%s' requires one axis-aligned vertical polygon.",
            parameter$WINDOW_NAME
        ), call. = FALSE)
    }

    span_axis <- setdiff(1:2, plane_axis)
    outward <- sign(frame$normal[[plane_axis]])
    if (outward == 0.0) {
        stop("Could not determine the outward direction of a shaded DeST window.")
    }
    plane <- coordinates[1L, plane_axis]
    span <- range(coordinates[, span_axis])
    top <- max(coordinates[, 3L]) + parameter$B0
    objects <- list()
    provenance <- list()

    # The overhang spans the window plus equal side extensions and projects W.
    if (parameter$W > tolerance) {
        low <- span[[1L]] - parameter$B1
        high <- span[[2L]] + parameter$B2
        wall_low <- wall_high <- outside_low <- outside_high <- c(0, 0, top)
        wall_low[[plane_axis]] <- wall_high[[plane_axis]] <- plane
        outside_low[[plane_axis]] <- outside_high[[plane_axis]] <-
            plane + outward * parameter$W
        wall_low[[span_axis]] <- outside_low[[span_axis]] <- low
        wall_high[[span_axis]] <- outside_high[[span_axis]] <- high
        vertices <- rbind(outside_low, wall_low, wall_high, outside_high)
        name <- paste(window$NAME[[1L]], "DeST Overhang")
        objects[[length(objects) + 1L]] <- shading__object_values(
            name, window$SURFACE_NAME[[1L]], vertices
        )
        provenance[[length(provenance) + 1L]] <- data.table::data.table(
            WINDOW_ID = parameter$WINDOW_ID,
            WINDOW_NAME = window$NAME[[1L]],
            SHADING_NAME = name,
            KIND = "overhang"
        )
    }

    # Equal DeST left/right fin dimensions are emitted on both span boundaries.
    for (side in c("low", "high")) {
        depth <- if (side == "low") parameter$WL else parameter$WR
        height <- if (side == "low") parameter$HL else parameter$HR
        if (depth <= tolerance || height <= tolerance) next
        span_value <- if (side == "low") span[[1L]] else span[[2L]]
        bottom <- top - height
        wall_top <- wall_bottom <- outside_top <- outside_bottom <- c(0, 0, 0)
        wall_top[[plane_axis]] <- wall_bottom[[plane_axis]] <- plane
        outside_top[[plane_axis]] <- outside_bottom[[plane_axis]] <-
            plane + outward * depth
        wall_top[[span_axis]] <- wall_bottom[[span_axis]] <- span_value
        outside_top[[span_axis]] <- outside_bottom[[span_axis]] <- span_value
        wall_top[[3L]] <- outside_top[[3L]] <- top
        wall_bottom[[3L]] <- outside_bottom[[3L]] <- bottom
        vertices <- rbind(outside_top, outside_bottom, wall_bottom, wall_top)
        label <- if (side == "low") "Low Fin" else "High Fin"
        name <- paste(window$NAME[[1L]], "DeST", label)
        objects[[length(objects) + 1L]] <- shading__object_values(
            name, window$SURFACE_NAME[[1L]], vertices
        )
        provenance[[length(provenance) + 1L]] <- data.table::data.table(
            WINDOW_ID = parameter$WINDOW_ID,
            WINDOW_NAME = window$NAME[[1L]],
            SHADING_NAME = name,
            KIND = paste0(side, "_fin")
        )
    }
    list(objects = objects, provenance = provenance)
}

# Convert the validated active WINDOW -> SHADING -> LIB_SHADING chain into
# opaque zone-attached EnergyPlus polygons after final window clipping.
shading__convert <- function(
    dest, ep, window,
    geometry_profile = eplus_geom__profile(ep$version())
) {
    shading <- shading__source_table(dest)
    if (nrow(shading) == 0L) return(NULL)
    shading__validate_parameters(shading)
    if (is.null(window) || nrow(window) == 0L) {
        stop("Active DeST window shading could not resolve a converted window.")
    }

    mapped <- window[shading, on = c("ID" = "WINDOW_ID"), nomatch = 0L,
        allow.cartesian = TRUE]
    resolved <- unique(mapped$ID)
    missing <- shading[!WINDOW_ID %in% resolved, WINDOW_NAME]
    if (length(missing) > 0L) {
        stop(sprintf(
            "Active DeST shading could not resolve exterior window(s): %s.",
            paste(missing, collapse = ", ")
        ), call. = FALSE)
    }
    invalid_window <- unique(mapped[
        INTERZONE | PART_COUNT != 1L,
        WINDOW_NAME
    ])
    if (length(invalid_window) > 0L) {
        stop(sprintf(
            paste(
                "Unsupported DeST shading host for window(s): %s.",
                "Only one-piece exterior windows are currently validated."
            ),
            paste(invalid_window, collapse = ", ")
        ), call. = FALSE)
    }

    built <- mapped[, {
        parameter <- shading[WINDOW_ID == ID[[1L]]][1L]
        list(result = list(shading__window_objects(.SD, parameter,
            geometry_profile)))
    }, by = "OUTPUT_PART_ID"]$result
    values <- unlist(lapply(built, `[[`, "objects"), recursive = FALSE)
    provenance <- data.table::rbindlist(
        unlist(lapply(built, `[[`, "provenance"), recursive = FALSE),
        fill = TRUE
    )
    assert_unique_name(provenance$SHADING_NAME, "window shading")
    out <- conv__add_objects(dest, ep, "Shading:Zone:Detailed", values)
    attr(out, "table") <- provenance
    out
}
