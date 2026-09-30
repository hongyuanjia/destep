# Resolve preset defaults without changing any explicitly selected behavior.
# Source-object translation remains in the corresponding owning converters.
conv__mode_options <- function(
    mode,
    people_heat = NULL,
    window_optics = NULL,
    source_distribution = NULL,
    surface_convection = "dest",
    exterior_boundary = NULL,
    source_options = NULL
) {
    mode <- match.arg(mode, c("objects", "dest"))
    defaults <- if (mode == "dest") {
        list(
            people_heat = "temperature_dependent",
            window_optics = "dest_solar",
            source_distribution = "dest",
            exterior_boundary = "dest_sky"
        )
    } else {
        list(
            people_heat = "constant",
            window_optics = "simple_glazing",
            source_distribution = "energyplus",
            exterior_boundary = "energyplus"
        )
    }
    people_heat <- match.arg(
        if (is.null(people_heat)) defaults$people_heat else people_heat,
        c("constant", "temperature_dependent")
    )
    window_optics <- match.arg(
        if (is.null(window_optics)) defaults$window_optics else window_optics,
        c("simple_glazing", "dest_solar")
    )
    source_distribution <- match.arg(
        if (is.null(source_distribution)) {
            defaults$source_distribution
        } else {
            source_distribution
        },
        c("energyplus", "dest")
    )
    surface_convection <- match.arg(surface_convection, c("dest", "energyplus"))

    # Retain the existing nested boundary option as a compatible explicit
    # choice. Two different explicit selections must not override each other.
    if (!is.null(source_options)) {
        checkmate::assert_list(source_options, names = "unique")
    }
    nested <- source_options$exterior_boundary
    if (!is.null(nested)) {
        nested <- match.arg(nested, c("energyplus", "dest_sky"))
    }
    if (!is.null(exterior_boundary)) {
        exterior_boundary <- match.arg(
            exterior_boundary,
            c("energyplus", "dest_sky")
        )
        if (!is.null(nested) && exterior_boundary != nested) {
            stop(
                "Conflicting exterior_boundary and source_options$exterior_boundary.",
                call. = FALSE
            )
        }
    } else {
        exterior_boundary <- if (is.null(nested)) {
            defaults$exterior_boundary
        } else {
            nested
        }
    }
    if (exterior_boundary == "dest_sky" && surface_convection != "dest") {
        stop(
            "exterior_boundary = 'dest_sky' requires surface_convection = 'dest'.",
            call. = FALSE
        )
    }
    if (
        identical(source_options$partition_boundary, "dest_air") &&
            (source_distribution != "dest" || surface_convection != "dest")
    ) {
        stop(
            "partition_boundary = 'dest_air' requires source_distribution = 'dest' and surface_convection = 'dest'.",
            call. = FALSE
        )
    }
    active <- source_distribution == "dest" || exterior_boundary == "dest_sky"
    if (!active && !is.null(source_options)) {
        stop(
            "'source_options' requires source_distribution = 'dest' or exterior_boundary = 'dest_sky'.",
            call. = FALSE
        )
    }
    if (active) {
        if (is.null(source_options)) {
            source_options <- list()
        }
        source_options$exterior_boundary <- exterior_boundary
    }
    list(
        mode = mode,
        people_heat = people_heat,
        window_optics = window_optics,
        source_distribution = source_distribution,
        surface_convection = surface_convection,
        exterior_boundary = exterior_boundary,
        source_options = source_options
    )
}

# Inventory actual emitted EMS programs, separating input-preserving moisture
# translation from optional DeST behavior. Unknown programs stay unclassified.
conv__mode_audit <- function(ep, options) {
    # Some supported eplusr versions reject a class filter when no instance
    # exists. Select from the complete table so genuinely EMS-free models work.
    table <- ep$to_table()
    programs <- as.character(table$value[
        table$class == "EnergyManagementSystem:Program" & table$index == 1L
    ])
    purpose <- rep("unclassified", length(programs))
    requirement <- rep("unclassified", length(programs))
    moisture <- startsWith(programs, "DeST_Moisture_")
    people <- startsWith(programs, "DeST_People_T_")
    source <- startsWith(programs, "SourceCorrectionUpdate")
    sky <- startsWith(programs, "DeSTSkyWeatherUpdate")
    purpose[moisture] <- "equipment_moisture"
    purpose[people] <- "temperature_dependent_people"
    purpose[source] <- "prescribed_source_distribution"
    purpose[sky] <- "dest_sky_boundary"
    requirement[moisture] <- "source_input"
    requirement[people | source | sky] <- "optional_alignment"
    effective <- options[setdiff(names(options), c("mode", "source_options"))]
    effective$partition_boundary <- if (
        is.null(options$source_options$partition_boundary)
    ) {
        "energyplus"
    } else {
        options$source_options$partition_boundary
    }
    list(
        preset = options$mode,
        effective = effective,
        ems = data.frame(
            program = programs,
            purpose = purpose,
            requirement = requirement
        ),
        equipment_moisture_policy = "preserve_source_input_in_both_modes"
    )
}

# Keep the chosen behavior visible in a saved IDF as well as the R attributes.
# A preset label alone cannot describe explicit per-feature overrides.
conv__mode_comments <- function(audit) {
    settings <- paste(
        names(audit$effective),
        unlist(audit$effective),
        sep = "=",
        collapse = "; "
    )
    uses <- if (nrow(audit$ems)) {
        paste(
            unique(paste(audit$ems$purpose, audit$ems$requirement, sep = ":")),
            collapse = "; "
        )
    } else {
        "none generated"
    }
    c(
        paste0("destep conversion preset: ", audit$preset),
        paste0("destep effective options: ", settings),
        paste0("destep EMS purposes: ", uses)
    )
}
