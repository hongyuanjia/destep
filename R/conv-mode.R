#' Configure DeST-to-EnergyPlus conversion
#'
#' Build a reusable options object for [to_eplus()]. A preset supplies defaults;
#' explicitly named settings override those defaults. The constructor checks
#' option values and dependencies; conversion checks model, weather and target
#' version requirements. Neither preset promises full DeST solver equivalence.
#'
#' @param preset \[string\] `"objects"` (default) selects constant people heat,
#'       simple glazing, EnergyPlus source allocation and exterior radiation.
#'       `"dest"` selects temperature-dependent people heat, DeST solar windows,
#'       DeST source allocation and DeST sky boundaries. Both retain source
#'       surface convection and physical inputs. Necessary EMS, including that
#'       for equipment moisture, remains available in both presets.
#' @param ... Must be empty. Supply custom settings by their full names.
#' @param schedule_format \[string\] `"compact"` (default) writes date-based
#'       Schedule:Compact objects, losslessly merging consecutive identical
#'       days and equal hourly values. `"file"` writes all 8760 hourly values
#'       to a CSV referenced by Schedule:File objects. Neither uses weekdays
#'       to select source values. Both preserve the required unit conversions.
#' @param schedule_directory \[string or NULL\] Persistent output directory,
#'       required with `schedule_format = "file"`. Each conversion writes a
#'       unique CSV and returns absolute references; keep this file with the IDF.
#'       This directory is independent of weather-dependent prepass files.
#' @param run_period \[integer vector\] Inclusive, one-based simulation start
#'       and end days, from 1 to 365. Defaults to `c(1L, 365L)` because a saved
#'       DeST simulation range has not been established in the supported schema.
#'       Supply the days used for the source simulation when known. Dates are
#'       mapped to a non-leap calendar with daylight saving disabled. Schedules
#'       always cover the full year, including for a partial run period.
#'       Conversion guarantees the initial correspondence only; later user
#'       edits to the IDF, files or calendar are not monitored.
#' @param hvac \[string\] HVAC representation. `"ideal_loads"`, the default,
#'       preserves the established load-only conversion. `"physical"` groups
#'       conditioned rooms by their referenced `AC_SYS` and generates each
#'       supported air loop independently. A one-room `AC_SYS_TYPE = 0` system
#'       uses a dedicated constant-volume graph. Type-0 systems with multiple
#'       rooms use constant-volume fans and fix every terminal minimum flow to
#'       its maximum flow. Type-1 systems with one or more rooms retain
#'       variable-volume fans and source terminal bounds. Multiple referenced
#'       systems may be generated in one model when each has exactly one `AHU`
#'       and satisfies the same documented constraints. All paths accept verified
#'       `FRESH_AIR_TYPE` values 1, 5, or 6. Outdoor-air type 1 uses the DeST
#'       minimum as a fixed flow and retains the maximum as a source capacity
#'       boundary. Types 5 and 6 map the source minimum and maximum flows to
#'       differential dry-bulb and differential enthalpy economizers. The type-1
#'       path proportionally reconciles terminal minimum flows when their sum
#'       falls below the system minimum outdoor-air flow by no more than 0.01%,
#'       with a warning; larger conflicts stop conversion. Multizone paths require
#'       one shared availability schedule per system and map matching
#'       `AC_SYS.SUPPLY_T_MIN/MAX` schedules to the cooling-coil setpoint and use
#'       their minimum value for cooling sizing; distinct minimum and maximum
#'       trajectories remain unsupported. Physical paths require
#'       `ver = "9.0.1"`, an installed matching EnergyPlus version, and explicit
#'       `hvac_options` for parameters absent from DeST. All HVAC representations
#'       reject models containing `AC_SYS_TYPE` values other than 0 and 1 instead
#'       of silently omitting unsupported systems.
#'
#' @param hvac_options \[list or NULL\] Named equipment parameters required by
#'       the selected `hvac = "physical"` paths. Common fan fields are
#'       `supply_fan_total_efficiency`, `supply_fan_delta_pressure_pa`,
#'       `supply_fan_motor_efficiency`, `supply_fan_motor_in_air_fraction`, the
#'       corresponding four `return_fan_*` fields,
#'       `zone_exhaust_fan_total_efficiency`, and
#'       `zone_exhaust_fan_pressure_rise_pa`. Common coil and plant fields are
#'       `chilled_water_design_setpoint_c`, `condenser_water_design_setpoint_c`,
#'       `chiller_type`, `chiller_nominal_cop`, and `tower_type`. A model with
#'       any single-zone type-0 path also requires five
#'       `return_fan_power_coefficient_*` fields,
#'       `cooling_coil_design_setpoint_c`,
#'       `heating_coil_design_setpoint_c`,
#'       `heating_coil_rated_air_water_convection_ratio`,
#'       `hot_water_design_setpoint_c`, `boiler_type`, `boiler_efficiency`, and
#'       `boiler_fuel_type`. Type-1 paths also require the five fan power
#'       coefficients; multizone type-0 paths use constant-volume fans and do not.
#'       All terminal-reheat paths require `cooling_coil_type = "ChilledWater"`,
#'       `preheat_coil_type = "Electric"`, `preheat_coil_design_setpoint_c`,
#'       `reheat_coil_type = "Electric"`, and a named numeric
#'       `zone_outdoor_air_flow_m3_s` vector. Its names must be exactly the DeST
#'       `ROOM.ID` values served by terminal-reheat paths. Allocations are checked
#'       per `AC_SYS`, and each system sum must equal that source system's minimum
#'       outdoor-air flow. One flat option list currently applies the same
#'       equipment and shared plant assumptions to all generated systems. Only
#'       fields required by the selected paths need to be supplied. These values
#'       are never inferred from reference models.
#'
#' @param people_heat \[string or NULL\] People sensible-heat mode. `NULL`
#'       uses the preset. `"constant"`
#'       preserves the existing conversion and corresponds to DeST bshell's
#'       `--const_occupant` option. Use `"temperature_dependent"` for the
#'       default behavior verified with DeST 0.2.230705: per-person sensible
#'       heat is `max(0, input + 5.536 * (26 - previous temperature))` W.
#'       This mode requires EnergyPlus 9.1 or newer and uses the preceding
#'       EnergyPlus zone-step air temperature. Record both engines' time steps
#'       when comparing results. The source database does not store the DeST
#'       execution option. Moisture input is unchanged; radiant recipient
#'       fractions and full-building numerical equivalence remain limitations.
#'
#' @param window_optics \[string or NULL\] Aggregate-window optical representation.
#'       `NULL` uses the preset.
#'       `"simple_glazing"` preserves the existing default. The opt-in
#'       `"dest_solar"` mode uses SC and pane count to derive solar angle tables
#'       for the simplified model verified with DeST 0.2.230705. It supports
#'       exterior one-to-three-pane aggregate windows and EnergyPlus 23.1 or newer.
#'       SC specifies a normal-transmittance objective of `0.87 * SC`, not SHGC.
#'       Glass resistance and each window face's source blackness are preserved.
#'       Single-pane solar absorption acts at mid-glass; multi-pane absorption
#'       acts at the outside of the aggregate glass resistance.
#'       EnergyPlus retains its own diffuse integration and heat-balance solver;
#'       native glass storage, sky exchange and room solar distribution are not
#'       added by this option. The tables are solar-only: visible/daylighting
#'       optics are not represented, and existing daylighting objects are rejected.
#'
#' @param source_distribution \[string or NULL\] `NULL` uses the preset.
#'       `"energyplus"` retains the default
#'       surface allocation. `"dest"` preserves the literal DeST air, wall,
#'       floor and roof fractions, including sums below one. This opt-in mode
#'       currently requires EnergyPlus 26.1 and `hvac = "ideal_loads"`.
#'       With windows it also requires `window_optics = "dest_solar"` and runs
#'       a weather-specific solar prepass. Unsupported moisture combinations,
#'       doors, daylighting and dynamic shading fail explicitly. Exterior
#'       radiation is selected independently through `exterior_boundary` or the
#'       preset; native time-integration algorithms are not reproduced.
#'
#' @param surface_convection \[string\] `"dest"` (default in both presets)
#'       retains fixed source coefficients on walls, floors, roofs, windows and
#'       doors using ordinary EnergyPlus surface-property objects. `"energyplus"`
#'       omits these overrides so EnergyPlus selects its own coefficients.
#'       Furniture's internal-mass exchange definition is retained separately.
#'       DeST sky and neighbor-air boundaries require `"dest"`.
#'
#' @param exterior_boundary \[string or NULL\] `"energyplus"` or `"dest_sky"`.
#'       `NULL` uses the preset. Sky conversion can be enabled independently
#'       of DeST source distribution. It requires EnergyPlus 26.1, ideal loads,
#'       source convection, `weather`, `directory`, and `window_optics =
#'       "dest_solar"` when windows are present. The linear sky-only boundary
#'       verified with DeST 0.2.230705 supports vertical walls/windows and
#'       horizontal roofs/exposed floors. Single-layer constructions and
#'       existing local environments are unsupported. A weather prepass preserves
#'       target height corrections and dry-bulb convection during rain. Window
#'       EMS copies current weather-table values without surface-temperature
#'       feedback. Outdoor emissivity is set to `1e-8` to suppress target
#'       longwave exchange, leaving a small numerical residual. The returned
#'       model's `exterior_boundary` attribute records coefficients and tables.
#'
#' @param partition_boundary \[string\] `"energyplus"` retains coupled interzone
#'       surfaces. `"dest_air"` selects the neighbor-air plus prescribed radiation
#'       approximation verified with DeST 0.2.230705. It requires DeST source
#'       distribution and surface convection; it is not general solver equivalence.
#' @param sky_radiation \[logical or NULL\] Override the saved
#'       `OPTION.CAL_SKY_RADIATION` switch for this run. `NULL` reads the source
#'       switch. Missing switches require an explicit override. Only available
#'       with `exterior_boundary = "dest_sky"`.
#' @param weather \[string or NULL\] Existing EPW path. Required by DeST sky
#'       conversion (even when sky radiation is disabled) and by DeST source
#'       distribution with windows.
#' @param directory \[string or NULL\] Persistent directory for generated
#'       weather/solar time tables and prepass records, required together with
#'       `weather`. Solar tables cover a non-leap year at five-minute resolution.
#'       Cache reuse checks model, weather, external files, engine and generated
#'       data. Reconvert after changing weather, geometry, optics, schedules or
#'       timestep; editing or running an `Idf` does not refresh these tables.
#'       Keep the directory or copy external files when saving the model.
#' @param terrain \[string or NULL\] EnergyPlus terrain: `"Country"`, `"Suburbs"`,
#'       `"City"`, `"Ocean"` or `"Urban"`. `NULL` retains the converter/IDD default.
#' @param solar_distribution \[string or NULL\] EnergyPlus solar method:
#'       `"MinimalShadowing"`, `"FullExterior"`, `"FullInteriorAndExterior"`,
#'       `"FullExteriorWithReflections"` or
#'       `"FullInteriorAndExteriorWithReflections"`. `NULL` retains the default.
#'       Choose a method suitable for the geometry; interior beam distribution
#'       has enclosure/convexity requirements. A `WithReflections` method enables
#'       exterior reflections from preserved opaque `SHADING.ROU` values; no
#'       visible or specular reflectance is inferred. This is separate from
#'       `source_distribution`, which controls DeST heat allocation.
#' @param shadow_update_days \[integer or NULL\] Positive periodic shading
#'       update interval in days. EnergyPlus warns above 31. `NULL` retains the
#'       default. Target settings apply before prepasses, enter cache identities,
#'       and are recorded in `attr(model, "conversion")$simulation` and IDF comments.
#'
#' @return An object of class `destep_options`, accepted by `to_eplus(options = )`.
#' @seealso [to_eplus()]
#' @examples
#' destep_opts()
#' destep_opts("dest", exterior_boundary = "energyplus")
#' # File schedules remain annual even when simulating days 59 through 61.
#' destep_opts(schedule_format = "file", schedule_directory = "schedules",
#'     run_period = c(59L, 61L))
#' opts <- destep_opts(
#'     "objects",
#'     window_optics = "dest_solar",
#'     terrain = "Country",
#'     shadow_update_days = 1L
#' )
#' # to_eplus(dest, "23.1", options = opts)
#' @export
destep_opts <- function(
    preset = "objects",
    ...,
    schedule_format = "compact",
    schedule_directory = NULL,
    run_period = c(1L, 365L),
    hvac = "ideal_loads",
    hvac_options = NULL,
    people_heat = NULL,
    window_optics = NULL,
    source_distribution = NULL,
    surface_convection = "dest",
    exterior_boundary = NULL,
    partition_boundary = "energyplus",
    sky_radiation = NULL,
    weather = NULL,
    directory = NULL,
    terrain = NULL,
    solar_distribution = NULL,
    shadow_update_days = NULL
) {
    if (length(list(...))) {
        stop(
            "Unknown or unnamed options. Use full argument names in destep_opts().",
            call. = FALSE
        )
    }
    checkmate::assert_choice(preset, c("objects", "dest"))
    checkmate::assert_choice(schedule_format, c("compact", "file"))
    checkmate::assert_string(
        schedule_directory,
        min.chars = 1L,
        null.ok = schedule_format == "compact"
    )
    if (schedule_format == "compact" && !is.null(schedule_directory)) {
        stop(
            "schedule_directory requires schedule_format = 'file'.",
            call. = FALSE
        )
    }
    checkmate::assert_integerish(
        run_period,
        len = 2L,
        lower = 1L,
        upper = 365L,
        any.missing = FALSE
    )
    if (run_period[[1L]] > run_period[[2L]]) {
        stop("run_period start day must not exceed its end day.", call. = FALSE)
    }
    run_period <- as.integer(run_period)
    defaults <- if (preset == "dest") {
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
    # Resolve preset defaults once so reuse never depends on missing arguments.
    if (is.null(people_heat)) {
        people_heat <- defaults$people_heat
    }
    if (is.null(window_optics)) {
        window_optics <- defaults$window_optics
    }
    if (is.null(source_distribution)) {
        source_distribution <- defaults$source_distribution
    }
    if (is.null(exterior_boundary)) {
        exterior_boundary <- defaults$exterior_boundary
    }
    checkmate::assert_choice(hvac, c("ideal_loads", "physical"))
    checkmate::assert_choice(
        people_heat,
        c("constant", "temperature_dependent")
    )
    checkmate::assert_choice(window_optics, c("simple_glazing", "dest_solar"))
    checkmate::assert_choice(source_distribution, c("energyplus", "dest"))
    checkmate::assert_choice(surface_convection, c("dest", "energyplus"))
    checkmate::assert_choice(exterior_boundary, c("energyplus", "dest_sky"))
    checkmate::assert_choice(partition_boundary, c("energyplus", "dest_air"))
    if (hvac == "physical") {
        checkmate::assert_list(
            hvac_options,
            names = "unique",
            .var.name = "hvac_options"
        )
    } else if (!is.null(hvac_options)) {
        stop(
            "'hvac_options' can only be supplied when hvac = 'physical'.",
            call. = FALSE
        )
    }
    if (exterior_boundary == "dest_sky" && surface_convection != "dest") {
        stop(
            "exterior_boundary = 'dest_sky' requires surface_convection = 'dest'.",
            call. = FALSE
        )
    }
    if (
        partition_boundary == "dest_air" &&
            (source_distribution != "dest" || surface_convection != "dest")
    ) {
        stop(
            "partition_boundary = 'dest_air' requires source_distribution = 'dest' and surface_convection = 'dest'.",
            call. = FALSE
        )
    }
    if (!is.null(sky_radiation)) {
        checkmate::assert_flag(sky_radiation)
        if (exterior_boundary != "dest_sky") {
            stop(
                "sky_radiation requires exterior_boundary = 'dest_sky'.",
                call. = FALSE
            )
        }
    }
    active <- source_distribution == "dest" || exterior_boundary == "dest_sky"
    if (active && hvac != "ideal_loads") {
        stop(
            "DeST source distribution and sky boundaries require hvac = 'ideal_loads'.",
            call. = FALSE
        )
    }
    if (!active && (!is.null(weather) || !is.null(directory))) {
        stop(
            "weather and directory require source_distribution = 'dest' or exterior_boundary = 'dest_sky'.",
            call. = FALSE
        )
    }
    # Paths can be configured before files exist; the conversion validates files
    # when a selected model actually needs a weather-dependent prepass.
    checkmate::assert_string(weather, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_string(directory, min.chars = 1L, null.ok = TRUE)
    simulation__options(Filter(
        Negate(is.null),
        list(
            terrain = terrain,
            solar_distribution = solar_distribution,
            shadow_update_days = shadow_update_days
        )
    ))
    structure(
        list(
            preset = preset,
            schedule_format = schedule_format,
            schedule_directory = schedule_directory,
            run_period = run_period,
            hvac = hvac,
            hvac_options = hvac_options,
            people_heat = people_heat,
            window_optics = window_optics,
            source_distribution = source_distribution,
            surface_convection = surface_convection,
            exterior_boundary = exterior_boundary,
            partition_boundary = partition_boundary,
            sky_radiation = sky_radiation,
            weather = weather,
            directory = directory,
            terrain = terrain,
            solar_distribution = solar_distribution,
            shadow_update_days = shadow_update_days
        ),
        class = "destep_options"
    )
}

# Revalidate reusable objects at the conversion boundary, including objects edited
# after construction. Arbitrary lists and partial/unknown fields are not accepted.
conv__resolve_options <- function(options) {
    if (is.character(options)) {
        return(destep_opts(options))
    }
    if (!inherits(options, "destep_options") || !is.list(options)) {
        stop(
            "'options' must be a preset string or an object created by destep_opts().",
            call. = FALSE
        )
    }
    fields <- setdiff(names(formals(destep_opts)), "...")
    if (
        is.null(names(options)) ||
            anyDuplicated(names(options)) ||
            !setequal(names(options), fields)
    ) {
        stop(
            "Invalid destep_options fields; recreate the object with destep_opts().",
            call. = FALSE
        )
    }
    do.call(destep_opts, unclass(options))
}

# Adapt the single public configuration to the owning converters' internal inputs.
conv__conversion_options <- function(options) {
    source <- NULL
    if (
        options$source_distribution == "dest" ||
            options$exterior_boundary == "dest_sky"
    ) {
        source <- Filter(
            Negate(is.null),
            unclass(options[c(
                "weather",
                "directory",
                "partition_boundary",
                "exterior_boundary",
                "sky_radiation"
            )])
        )
    }
    c(
        list(mode = options$preset),
        unclass(options[c(
            "people_heat",
            "window_optics",
            "source_distribution",
            "surface_convection",
            "exterior_boundary"
        )]),
        list(source_options = source)
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
