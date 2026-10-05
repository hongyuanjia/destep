#' Configure DeST-to-EnergyPlus conversion
#'
#' Build a reusable options object for [to_eplus()]. A preset supplies defaults;
#' explicitly named settings override those defaults. The constructor checks
#' option values and dependencies; conversion checks model and target version
#' requirements. Aggregate windows use a documented SimpleGlazing
#' approximation and do not promise full DeST solver equivalence.
#'
#' @param preset \[string\] `"objects"` selects source input conversion.
#'       Necessary moisture EMS remains available.
#' @param ... Must be empty. Supply custom settings by their full names.
#' @param schedule_format \[string\] `"compact"` (default) writes date-based
#'       Schedule:Compact objects, losslessly merging consecutive identical
#'       days and equal hourly values. `"file"` writes all 8760 hourly values
#'       to a CSV referenced by Schedule:File objects. Neither uses weekdays
#'       to select source values. Both preserve the required unit conversions.
#' @param schedule_directory \[string or NULL\] Persistent output directory,
#'       required with `schedule_format = "file"`. Each conversion writes a
#'       unique CSV and returns absolute references; keep this file with the IDF.
#'       This directory is independent of the source weather file.
#' @param run_period \[integer vector\] Inclusive, one-based simulation start
#'       and end days, from 1 to 365. Defaults to `c(1L, 365L)` because a saved
#'       DeST simulation range has not been established in the supported schema.
#'       Supply the days used for the source simulation when known. Dates are
#'       mapped to a non-leap calendar with daylight saving disabled. Schedules
#'       always cover the full year, including for a partial run period.
#'       Conversion guarantees the initial correspondence only; later user
#'       edits to the IDF, files or calendar are not monitored.
#' @param hvac \[string\] HVAC representation. `"auto"` (default) reads effective
#'       room and system references: unconditioned rooms receive no air system,
#'       source load-only controls use IdealLoads, and supported actual systems
#'       select physical conversion. Incomplete or mixed source definitions stop
#'       rather than silently dropping equipment. Explicit `"ideal_loads"`
#'       produces a load-analysis copy even when the source contains a real
#'       system; its conversion audit records that simplification. `"physical"` groups
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
#'       trajectories remain unsupported. Physical paths generate baseline
#'       objects with the baseline version's local ExpandObjects installation,
#'       then use [eplusr::transition()] for higher requested targets. Necessary
#'       classes, emitted fields and the resulting object graph are validated.
#'       Baseline syntax is 9.0.1, raised to 9.1 for effective moisture sources;
#'       targets older than 9.0.1 are not maintained. Tested
#'       baseline versions and upgraded targets are evidence, not a whitelist.
#'       Supported models with a selected main water coil and no selected plant
#'       use source AHU two/four-pipe water temperature schedules without
#'       required `hvac_options`. Independent heating and cooling water stages
#'       are a target equivalent; their autosized performance is approximate.
#'       Selected plants and conflicting AHU water-loop ownership currently stop
#'       until their mappings are supported. Automatic and
#'       physical conversion reject effective `AC_SYS_TYPE` values other than 0
#'       and 1. Unreferenced system records do not define converted equipment.
#'       Populated `LIB_*` tables are catalogues; they do not establish that a
#'       source model selected a chiller, boiler, or cooling tower.
#'       Nonzero people or equipment moisture requires the target calling point
#'       `BeginZoneTimestepBeforeInitHeatBalance`, introduced in 9.1.0. Joint
#'       physical-system and moisture runs have been checked on 9.1.0; higher
#'       versions are obtained by transition. 9.0.1 lacks that calling point
#'       and rejects nonzero sources.
#'
#' @param hvac_options \[list or NULL\] Optional target equipment overrides.
#'       Missing fan inputs use EnergyPlus 9.0.1 template defaults: total
#'       efficiency 0.7, motor efficiency 0.9, motor-in-air fraction 1;
#'       CAV supply/return pressure 600/300 Pa, VAV pressure 1000/500 Pa, and
#'       native inlet-vane power coefficients. These are target assumptions,
#'       not a conversion of DeST AHU fan-power fields. Accepted names are
#'       `supply_fan_total_efficiency`, `supply_fan_delta_pressure_pa`,
#'       `supply_fan_motor_efficiency`, `supply_fan_motor_in_air_fraction`,
#'       the corresponding four `return_fan_*` fields, five
#'       `return_fan_power_coefficient_1` through `_5` fields,
#'       `zone_exhaust_fan_total_efficiency`, `zone_exhaust_fan_pressure_rise_pa`,
#'       and `heating_coil_rated_air_water_convection_ratio` (native default 0.5).
#'       Flow-balancing exhaust and water-loop pumps default to zero pressure
#'       head, adding no assumed electricity. Equivalent coil water design
#'       temperatures of 7/60 C are sizing assumptions; full source schedules
#'       determine operational supply temperatures. Main coil technology,
#'       water schedules and plant choices cannot be overridden with this list.
#'       Two-pipe equivalent stages use native coil/loop availability derived
#'       from source temperatures: water must be strictly below the minimum
#'       or above the maximum supply-air bound at each hour. Intermediate or
#'       equal temperatures remain unsupported; shared loops require identical
#'       stage availability. No calendar-based season rule is inferred.
#'       Single-zone constant source supply-air bounds constrain native feedback
#'       managers; varying bounds currently stop conversion.
#'       Zone outdoor-air design allocation preserves source room minima and
#'       system total. `zone_outdoor_air_flow_m3_s` is an optional named ROOM.ID
#'       override, subject to the same total and terminal flow bounds.
#'       Effective values and origins are recorded per system in
#'       `attr(model, "conversion")$hvac$effective_options` and IDF comments.
#'       Room terminal reheat retains `ROOM.SET_TERMINAL_MAX` in total W and
#'       explicit ROOM `ROOM_REHEATER_TYPE`; positive capacity with an absent
#'       type uses a disclosed converter electric default. Single-zone CAV and
#'       multizone paths place electric reheat at the room terminal, with the
#'       source capacity ceiling and native target zone-demand control.
#'       Hot-water terminals
#'       and central AHU reheat remain unsupported. No fictitious electric
#'       preheat, chiller, boiler or cooling tower is generated for a source
#'       that only specifies its water boundary. The existing physical path
#'       supports single-zone electric steam humidification with source room-RH
#'       schedules and target autosizing. Source humidifiers with zero room and
#'       supply minimum RH are recorded as inactive. Single-zone and multizone CAV room-RH
#'       upper bounds use native temperature/humidity control on the existing
#'       cooling coil, independently of humidifier presence. Source temperature
#'       targets remain, but native humidity control can supersede them; unmet
#'       temperature/RH requirements are possible. DeST AHU optimization is not
#'       reproduced and no reheat is added for humidity control. Single-zone
#'       electric humidification also retains a source supply-RH lower bound:
#'       necessary EMS converts it using current supply-fan outlet temperature
#'       and barometric pressure, taking the larger of the room and supply
#'       minimum humidity-ratio targets without changing equipment capacity.
#'       Supply-RH upper bounds remain unsupported.
#'       An absent source humidifier stays absent; positive
#'       room lower RH then produces a warning and an audit record, without
#'       inventing equipment. Active external-steam or spray humidifiers
#'       remain unsupported. Multizone CAV/VAV electric
#'       humidification preserves a humidistat per source room and uses native
#'       critical-zone control on the shared air loop. Its target numerical
#'       humidity-ratio guards are 1e-9 and 1 kg/kg, not DeST input limits;
#'       demands outside this interval are not represented. Native low-load
#'       behavior, capacity and saturation can leave room targets unmet.
#'       Multizone CAV room upper RH uses the native critical-zone maximum humidity
#'       manager on the existing cooling coil with the same numerical guards.
#'       VAV room lower RH uses the same native minimum manager and preserves
#'       source terminal flow bounds. Active VAV dehumidification remains
#'       unsupported because native humidity override probes failed to converge.
#'       Multizone supply-RH bounds remain unsupported.
#'       Single-zone sensible heat recovery retains equal seasonal maximum
#'       coefficients when minima are zero and no source duct network or
#'       nonzero recovery pressure loss is specified. A native plate exchanger
#'       uses constant effectiveness, zero auxiliary power, and native economizer
#'       lockout. No HX outlet temperature limit or frost protection is inferred;
#'       these target assumptions and unmapped pressure loss are audited.
#'       Total-heat, unequal-seasonal, and multizone recovery remain unsupported.
#'       Unsupported inputs are diagnosed rather than silently disabled.
#'
#' @param surface_convection \[string\] `"dest"` (default)
#'       retains fixed source coefficients on walls, floors, roofs, windows and
#'       doors using ordinary EnergyPlus surface-property objects. `"energyplus"`
#'       omits these overrides so EnergyPlus selects its own coefficients.
#'       Furniture's internal-mass exchange definition is retained separately.
#'
#' @param terrain \[string or NULL\] EnergyPlus terrain: `"Country"`, `"Suburbs"`,
#'       `"City"`, `"Ocean"` or `"Urban"`. `NULL` retains the converter/IDD default.
#' @param solar_distribution \[string or NULL\] EnergyPlus solar method:
#'       `"MinimalShadowing"`, `"FullExterior"`, `"FullInteriorAndExterior"`,
#'       `"FullExteriorWithReflections"` or
#'       `"FullInteriorAndExteriorWithReflections"`. `NULL` retains the default.
#'       Choose a method suitable for the geometry; interior beam distribution
#'       has enclosure/convexity requirements. A `WithReflections` method enables
#'       exterior reflections from preserved opaque `SHADING.ROU` values; no
#'       visible or specular reflectance is inferred.
#' @param shadow_update_days \[integer or NULL\] Positive periodic shading
#'       update interval in days. EnergyPlus warns above 31. `NULL` retains the
#'       default. Selections are recorded in
#'       `attr(model, "conversion")$simulation` and IDF comments.
#'
#' @return An object of class `destep_options`, accepted by `to_eplus(options = )`.
#' @seealso [to_eplus()]
#' @section People inputs and interzone surfaces:
#' Conversion retains nominal people sensible heat and `O_DAMP_PER_PERSON`;
#' DeST occupant temperature feedback is not reproduced. Nonzero people moisture
#' requires EnergyPlus 9.1 or newer. An unmetered OtherEquipment companion uses
#' target vapor enthalpy to express nominal g/h/person as latent watts. Rapid
#' temperature changes can leave a one-zone-step mass residual. People carries
#' sensible heat only: its activity is not a comfort-model metabolic input and
#' its latent-gain reports exclude the independent moisture source.
#' Interzone surfaces retain their paired surface references and constructions;
#' no neighbor-air equivalent-temperature boundary is generated.
#'
#' @examples
#' destep_opts()
#' # File schedules remain annual even when simulating days 59 through 61.
#' destep_opts(schedule_format = "file", schedule_directory = "schedules",
#'     run_period = c(59L, 61L))
#' opts <- destep_opts(
#'     "objects",
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
    hvac = "auto",
    hvac_options = NULL,
    surface_convection = "dest",
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
    checkmate::assert_choice(preset, "objects")
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
    checkmate::assert_choice(hvac, c("auto", "ideal_loads", "physical"))
    checkmate::assert_choice(surface_convection, c("dest", "energyplus"))
    if (hvac == "ideal_loads" && !is.null(hvac_options)) {
        stop(
            "'hvac_options' can only be supplied when hvac = 'auto' or 'physical'.",
            call. = FALSE
        )
    }
    if (!is.null(hvac_options)) {
        checkmate::assert_list(
            hvac_options,
            names = "unique",
            .var.name = "hvac_options"
        )
    }

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
            surface_convection = surface_convection,
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
    c(
        list(mode = options$preset),
        unclass(options["surface_convection"])
    )
}

# Inventory necessary moisture EMS programs. Unknown programs stay unclassified.
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
    people_moisture <- startsWith(programs, "DeST_People_Moisture_")
    purpose[moisture] <- "equipment_moisture"
    purpose[people_moisture] <- "people_moisture"
    requirement[moisture | people_moisture] <- "source_input"
    # Supply-RH coordinate conversion is source-input EMS owned by HVAC.
    # Keep unrelated caller programs unclassified rather than inferring intent.
    supply_humidity <- startsWith(programs, "DeST_Supply_RH_")
    purpose[supply_humidity] <- "supply_humidity"
    requirement[supply_humidity] <- "source_input"
    effective <- options[setdiff(names(options), "mode")]
    list(
        preset = options$mode,
        effective = effective,
        ems = data.frame(
            program = programs,
            purpose = purpose,
            requirement = requirement
        ),
        equipment_moisture_policy = "preserve_source_input"
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
    hvac <- if (!is.null(audit$hvac)) {
        paste0(
            "destep HVAC: requested=",
            audit$hvac$requested,
            "; resolved=",
            audit$hvac$resolved,
            "; source=",
            audit$hvac$source$state
        )
    }
    equipment <- if (!is.null(audit$hvac)) {
        paste0(
            "destep model-local plant records: ",
            hvac__plant_record_summary(audit$hvac$source),
            "; LIB_* rows are catalogue entries"
        )
    }
    c(
        paste0("destep conversion preset: ", audit$preset),
        paste0("destep effective options: ", settings),
        hvac,
        equipment,
        paste0("destep EMS purposes: ", uses)
    )
}
