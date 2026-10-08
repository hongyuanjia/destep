# TODO: functions to export LIB_* tables from DeST models

MAP_ID_NAME <- list(
    AC_SYS = c(id = "AC_SYS_ID", name = "NAME", prefix = "AC Sys"),
    AHU = c(id = "AHU_ID", name = "NAME", prefix = "AHU"),
    AIR_SUPPLY_PORT = c(id = "ID", name = "NAME", prefix = "Air Supply Port"),
    BOILER = c(id = "BOILER_ID", name = "NAME", prefix = "Boiler"),
    BUILDING = c(id = "BUILDING_ID", name = "NAME", prefix = "Building"),
    CHILLER = c(id = "CHILLER_ID", name = "NAME", prefix = "Chiller"),
    COOLINGTOWER = c(
        id = "COOLINGTOWER_ID",
        name = "NAME",
        prefix = "Cooling Tower"
    ),
    DEFAULT_COEF = c(
        id = "DEFAULT_COEF_ID",
        name = "COEF_NAME",
        prefix = "Coef"
    ),
    DIST_MODE = c(id = "DIST_MODE_ID", name = "NAME", prefix = "Dist Mode"),
    DOOR = c(id = "ID", name = "NAME", prefix = "Door"),
    DUCT = c(id = "ID", name = "NAME", prefix = "Duct"),
    DUCTNET = c(id = "ID", name = "NAME", prefix = "Duct Net"),
    DUCT_JOINT = c(id = "ID", name = "NAME", prefix = "Duct Joint"),
    DUCT_TERMINAL = c(id = "ID", name = "NAME", prefix = "Duct Terminal"),
    ENERGY_DEVICE = c(id = "ID", name = "NAME", prefix = "Energy Device"),
    ENERGY_HOTWATER = c(id = "ID", name = "NAME", prefix = "Energy Hot Water"),
    ENERGY_LIFT_ESCALATOR = c(
        id = "ID",
        name = "NAME",
        prefix = "Energy Lift Escalator"
    ),
    ENERGY_PUMP_FAN = c(id = "ID", name = "NAME", prefix = "Energy Pump Fan"),
    ENERGY_PUMP_GROUP = c(
        id = "ID",
        name = "NAME",
        prefix = "Energy Pump Group"
    ),
    ENVIRONMENT = c(
        id = "ENVIRONMENT_ID",
        name = "NAME",
        prefix = "Environment"
    ),
    EQUIPMENT_GAINS = c(
        id = "GAIN_ID",
        name = "NAME",
        prefix = "Equipment Gains"
    ),
    EQUIPMENT_TEMP = c(id = "ID", name = "NAME", prefix = "Equipment Temp"),
    FAN_COIL = c(id = "ID", name = "NAME", prefix = "Fan Coil"),
    GROUND = c(id = "GROUND_ID", name = "NAME", prefix = "Ground"),
    HACNET_BRANCH = c(id = "BRANCH_ID", name = "NAME", prefix = "Branch"),
    HACNET_NODE = c(id = "NODE_ID", name = "NAME", prefix = "Node"),
    HACNET_PUMP = c(id = "PUMP_ID", name = "NAME", prefix = "Pump"),
    HACNET_SUBNET = c(id = "SUBNET_ID", name = "NAME", prefix = "Subnet"),
    HACNET_TERMINAL = c(id = "TERMINAL_ID", name = "NAME", prefix = "Terminal"),
    HACNET_VALVE = c(id = "VALVE_ID", name = "NAME", prefix = "Valve"),
    HEATEXCHANGER = c(
        id = "HEATEXCHANGER_ID",
        name = "NAME",
        prefix = "Heat Exchanger"
    ),
    HEATING_PIPE = c(
        id = "HEATING_PIPE_ID",
        name = "NAME",
        prefix = "Heating Pipe"
    ),
    HEATING_SYSTEM = c(
        id = "HEATING_SYSTEM_ID",
        name = "NAME",
        prefix = "Heating System"
    ),
    LIB_BOILER = c(id = "LIB_BOILER_ID", name = "NAME", prefix = "Lib Boiler"),
    LIB_CHILLER = c(
        id = "LIB_CHILLER_ID",
        name = "NAME",
        prefix = "Lib Chiller"
    ),
    LIB_COOLINGTOWER = c(
        id = "LIB_COOLINGTOWER_ID",
        name = "NAME",
        prefix = "Lib Cooling Tower"
    ),
    LIB_CURVE = c(id = "LIB_CURVE_ID", name = "NAME", prefix = "Lib Curve"),
    LIB_HEATEXCHANGER = c(
        id = "LIB_HEATEXCHANGER_ID",
        name = "NAME",
        prefix = "Lib Heat Exchanger"
    ),
    LIB_PHASE_CHANGE_MAT = c(
        id = "LIB_PHASE_CHANGE_MAT_ID",
        name = "NAME",
        prefix = "Lib Phase Change Mat"
    ),
    LIB_PRODUCT = c(id = "PRODUCT_ID", name = "NAME", prefix = "Lib Product"),
    LIB_PUMP = c(id = "LIB_PUMP_ID", name = "NAME", prefix = "Lib Pump"),
    LIB_ROOFUNIT_DEVICE = c(
        id = "ID",
        name = "NAME",
        prefix = "Lib Roof Unit Device"
    ),
    LIB_SHADING = c(id = "ID", name = "NAME", prefix = "Lib Shading"),
    LIB_SOLAR_ENERGY_COLLECTOR = c(
        id = "LIB_SOLAR_ENERGY_COLLECTOR_ID",
        name = "NAME",
        prefix = "Lib Solar Energy Collector"
    ),
    LIB_VRV_SOURCE = c(id = "ID", name = "NAME", prefix = "Lib VRV Source"),
    LIB_VRV_TERMINAL = c(id = "ID", name = "NAME", prefix = "Lib VRV Terminal"),
    LIB_WIND_RATIO_MODEL = c(
        id = "ID",
        name = "NAME",
        prefix = "Lib Wind Ratio Model"
    ),
    LIB_WIND_RATIO_TYPE = c(
        id = "ID",
        name = "NAME",
        prefix = "Lib Wind Ratio Type"
    ),
    LIGHT_GAINS = c(id = "GAIN_ID", name = "NAME", prefix = "Light"),
    OCCUPANT_GAINS = c(id = "GAIN_ID", name = "NAME", prefix = "Occupant"),
    OUTSIDE = c(id = "OUTSIDE_ID", name = "NAME", prefix = "Outside"),
    PUMP = c(id = "PUMP_ID", name = "NAME", prefix = "Pump"),
    ROOM = c(id = "ID", name = "NAME", prefix = "Room"),
    ROOM_GROUP = c(id = "ROOM_GROUP_ID", name = "NAME", prefix = "Room Group"),
    SCHEDULE_YEAR = c(
        id = "SCHEDULE_ID",
        name = "NAME",
        prefix = "Schedule Year"
    ),
    SHADING = c(id = "ID", name = "NAME", prefix = "Shading"),
    SKY = c(id = "SKY_ID", name = "NAME", prefix = "Sky"),
    STOREY = c(id = "ID", name = "NAME", prefix = "Storey"),
    SURFACE = c(id = "SURFACE_ID", name = "NAME", prefix = "Surface"),
    SYS_AIRFLOOR = c(id = "STRUCT_ID", name = "CNAME", prefix = "Sys Airfloor"),
    SYS_APP_MATERIAL = c(
        id = "APP_MATERIAL_ID",
        name = "CNAME",
        prefix = "Sys App Material"
    ),
    SYS_CITY = c(id = "CITY_ID", name = "CNAME", prefix = "Sys City"),
    SYS_CURTAIN = c(id = "CURTAIN_ID", name = "CNAME", prefix = "Sys Curtain"),
    SYS_DOOR = c(id = "DOOR_ID", name = "CNAME", prefix = "Sys Door"),
    SYS_GROUNDFLOOR = c(
        id = "STRUCT_ID",
        name = "CNAME",
        prefix = "Sys Groundfloor"
    ),
    SYS_GROUPS = c(id = "TYPE", name = "CNAME", prefix = "Sys Groups"),
    SYS_INWALL = c(id = "STRUCT_ID", name = "CNAME", prefix = "Sys Inwall"),
    SYS_MATERIAL = c(
        id = "MATERIAL_ID",
        name = "CNAME",
        prefix = "Sys Material"
    ),
    SYS_MIDDLEFLOOR = c(
        id = "STRUCT_ID",
        name = "CNAME",
        prefix = "Sys Middlefloor"
    ),
    SYS_OUTWALL = c(id = "STRUCT_ID", name = "CNAME", prefix = "Sys Outwall"),
    SYS_ROOF = c(id = "STRUCT_ID", name = "CNAME", prefix = "Sys Roof"),
    SYS_SHADING = c(id = "SHIELD_ID", name = "CNAME", prefix = "Sys Shading"),
    SYS_WINDOW = c(id = "WINDOW_ID", name = "CNAME", prefix = "Sys Window"),
    VRV_SOURCE = c(id = "ID", name = "NAME", prefix = "VRV Source"),
    VRV_TERMINAL = c(id = "ID", name = "NAME", prefix = "VRV Terminal"),
    WATER_SYS = c(id = "WATER_SYS_ID", name = "NAME", prefix = "Water Sys"),
    WATER_SYSTEM = c(
        id = "WATER_SYSTEM_ID",
        name = "NAME",
        prefix = "Water System"
    ),
    WINDOW = c(id = "ID", name = "NAME", prefix = "Window"),
    WINDOW_TYPE_DATA = c(id = "ID", name = "NAME", prefix = "Window Type Data")
)

#' Convert a DeST model to EnergyPlus model
#'
#' @param dest A \[string or DBIConnection\] path to a DeST model file or a
#'        DBIConnection object.
#'
#' @param ver \[string\] A character string specifying the EnergyPlus version.
#'        It can be `"latest"`, which is the default, to indicate using the
#'        latest EnergyPlus version supported by the
#'        \{[eplusr](https://cran.r-project.org/package=eplusr)\} package.
#'        Objects are generated using the project's EnergyPlus 9.0.1 baseline.
#'        Earlier targets are not maintained. Effective moisture raises the generation
#'        baseline to 9.1 and requires a target of at least 9.1. Higher targets
#'        are produced with [eplusr::transition()], not separate object writers.
#'        Physical HVAC requires the generation version's local ExpandObjects;
#'        target and intermediate IDDs are resolved by eplusr for transition.
#'        The generation/target versions are recorded in the conversion audit.
#'        Geometry compatibility has been validated against EnergyPlus 23.1;
#'        other versions currently reuse that profile with an explicit warning.
#'
#' @param copy \[logical\] Whether to copy the input DeST database to a
#'        temporary SQLite database. Note that if `FALSE`, the input database
#'        will be modified during the conversion. Default is `TRUE`.
#'
#' @param verbose \[logical\] Whether to show verbose messages. Default is
#'       `FALSE`.
#'
#' @param options \[string or destep_options\] Conversion configuration.
#'       Use `"objects"` (default) or [destep_opts()] to configure source
#'       inputs, time tables and HVAC representation.
#'
#' @details Outdoor ventilation retains the source minimum ACH time table.
#'       A saved `OPTION.VARIANT_VENT = 0` disables the range supplement.
#'       When the saved switch is absent, conversion retains its legacy
#'       documented outdoor-temperature-band rule and warns about the assumed
#'       enabled setting. The `ventilation` attribute records this selection.
#'       A room group with `IS_AC_ROOM = 0` retains minimum ventilation only;
#'       unused maximum and temperature-range references are not consumed.
#'       the AC availability time table does not gate the range increment.
#'       The rule preserves the remaining range inputs
#'       but does not reproduce DeST's internal ventilation control algorithm.
#'
#' Window-side surfaces stored with `OF_ROOM = -1` are restored from the
#' same side of their explicit `WINDOW.OF_ENCLOSURE` host. Restoration
#' requires unique references, an existing room/outdoor/ground owner and
#' no conflicting binding on the other side. Ambiguous or incomplete
#' relationships stop conversion. Only `OF_ROOM` and `TYPE` are restored;
#' geometry and thermal properties are retained. The default `copy = TRUE`
#' leaves the input database unchanged. A
#' `destep_restored_window_bindings` warning carries the affected records
#' in its `bindings` field. This restores redundant input relationships,
#' not missing geometry or DeST solver behavior.
#'
#' Storey multipliers are mapped to ZoneGroup independently of source
#' surface boundaries. Outdoor, ground and interzone relationships are
#' retained. Interzone pairs with unequal multipliers produce a warning
#' and are listed in `conversion$surface_boundaries`: weighted outputs
#' must not be interpreted as a physically balanced whole-building model.
#' Successful conversion does not establish DeST thermal equivalence.
#' Missing enclosure-side or middle-plane references produce a
#' `destep_invalid_surface_references` error whose `references` table identifies
#' the source enclosure, field and saved reference.
#'
#' @return \[eplusr::Idf\] The converted EnergyPlus model. The
#'       `conversion` attribute and Version comments record the selected
#'       options, resolved HVAC representation, source component record counts,
#'       and necessary EMS programs.
#'       Its `window_bindings` table records the window, side, source surface,
#'       host enclosure/surface and original/restored ownership and type for
#'       each restored face. Restoration is also recorded in saved IDF comments.
#'       Its `windows` table records the
#'       source K/SC, nominal SHGC, face blackness and unresolved optical
#'       properties for each window. The same aggregate assumptions appear in
#'       the saved IDF glazing comments. This does not establish whole-building
#'       equivalence.
#'
#' @seealso [to_epw()], [destep_opts()] for supported inputs and HVAC limitations.
#' @section Conversion scope:
#' Source inputs are mapped to EnergyPlus objects with documented equivalent
#' representations where needed. Conversion does not reproduce DeST solver
#' algorithms or guarantee matching annual loads. Unsupported selected HVAC
#' equipment stops automatic/physical conversion with a diagnostic; it is not
#' replaced silently by IdealLoads. See [destep_opts()] for supported subsets.
#' Internal gains retain the source air and total radiant fractions. Separate
#' surrounding-surface, floor and roof fractions are not mapped to EnergyPlus;
#' effective sensible sources using these fractions produce a
#' `destep_unsupported_gain_distribution` warning. The warning's `distributions`
#' field records the affected gain types and source modes. Receiving-surface
#' allocation and the resulting transient loads are not guaranteed equivalent.
#' Heating setpoints above cooling setpoints are diagnosed only during hours
#' when the effective room-type AC availability schedule is greater than zero.
#' A `destep_thermostat_conflict` warning identifies the heating, cooling and
#' availability schedule IDs, rooms and conflicting hours. The same table is
#' retained in `conversion$schedules$temperature_conflicts`. Original schedules
#' are preserved, including inactive inverted values; this diagnostic does not
#' guarantee that EnergyPlus accepts the controls during a simulation.
#' Terrain, solar distribution and shading-update settings retain EnergyPlus
#' defaults. Edit the returned [eplusr::Idf] to change target simulation settings.
#'
#' @examples
#' \dontrun{
#' to_idf(dest, "23.1", options = "objects")
#' opts <- destep_opts("objects", run_period = c(1L, 31L))
#' to_idf(dest, "23.1", options = opts)
#' }
#'
#' @export
# TODO: How about STOREY_GROUP?
to_idf <- function(
    dest,
    ver = "latest",
    copy = TRUE,
    verbose = FALSE,
    options = "objects"
) {
    # Resolve one configuration before reading or copying the source database.
    options <- conv__resolve_options(options)
    conversion <- conv__conversion_options(options)
    requested_hvac <- options$hvac
    hvac_options <- options$hvac_options
    surface_convection <- conversion$surface_convection

    if (is_string(dest) && file.exists(dest)) {
        dest <- read_dest(dest, verbose = verbose)
        on.exit(DBI::dbDisconnect(dest), add = TRUE)
    } else if (!inherits(dest, "DBIConnection")) {
        stop(
            "'dest' should be a path to a DeST model file or a DBIConnection object."
        )
    }

    if (!is_flag(copy)) {
        stop("'copy' should be a single logical value of 'TRUE' or 'FALSE'")
    }
    if (!is_flag(verbose)) {
        stop("'verbose' should be a single logical value of 'TRUE' or 'FALSE'")
    }

    # Resolve HVAC from effective room ownership before copying or generating
    # objects. Only an explicit load-analysis request may omit a source system.
    hvac_inventory <- hvac__source_inventory(dest)
    if (requested_hvac != "ideal_loads") {
        hvac__assert_supported_system_types(dest, hvac_inventory)
    }
    hvac <- hvac__resolve_representation(requested_hvac, hvac_inventory)
    if (hvac != "physical" && !is.null(hvac_options)) {
        abort(
            "hvac_options were supplied but the source has no physical HVAC path.",
            class = "destep_unused_hvac_equipment_options"
        )
    }

    # copy the DeST database to a temporary SQLite database since we need to
    # update the database
    if (!copy) {
        tmpdb <- dest
    } else {
        path_tmpdb <- tempfile("destep-tmp-", fileext = ".sql")
        tmpdb <- DBI::dbConnect(RSQLite::SQLite(), path_tmpdb)
        RSQLite::sqliteCopyDatabase(dest, tmpdb)
        on.exit(
            {
                DBI::dbDisconnect(tmpdb)
                unlink(path_tmpdb)
            },
            add = TRUE
        )
    }

    # Resolve the requested output version, then generate common old syntax.
    # eplusr owns the upward release transitions after all objects are built.
    target_version <- eplusr::empty_idf(ver)$version()
    generation_version <- conv__generation_version(tmpdb, target_version)
    if (verbose) {
        ep <- eplusr::with_verbose(eplusr::empty_idf(generation_version))
    } else {
        ep <- eplusr::empty_idf(generation_version)
    }

    # add GlobalGeometryRules
    ep$add(
        "GlobalGeometryRules" := list(
            starting_vertex_position = "UpperLeftCorner",
            vertex_entry_direction = "Counterclockwise",
            # DeST POINT coordinates share one building-wide drawing origin. Keep
            # every EnergyPlus Zone origin at zero and use relative coordinates so
            # Building North Axis can rotate that drawing to true north.
            coordinate_system = "Relative",
            daylighting_reference_point_coordinate_system = "Relative"
        )
    )

    # Use a five-minute solver step so IdealLoads humidity control converges
    # within the converted hourly DeST control bounds. This is explicit for
    # reproducibility instead of relying on EnergyPlus's four-step default.
    ep$add(
        "Timestep" := list(
            number_of_timesteps_per_hour = 12L
        )
    )
    # Use calendar dates so both schedule formats retain source hour positions.
    schedule__run_period(ep, options$run_period)

    # Restore only explicit, uniquely hosted window-side ownership before names
    # or surface geometry are derived. The input connection is copied by default.
    window_bindings <- window__restore_bindings(tmpdb)

    # update object names and make sure all names are unique
    conv__update_names(tmpdb)

    # update Version comments
    ver <- conv__version_comment(tmpdb, ep)
    ep$Version$comment(un_list(ver$object$comment))

    # Surface part geometry must be available when an opening crosses a topology
    # split, because each clipped piece references exactly one host part.
    geometry_profile <- eplus_geom__profile(target_version)
    surface <- surface__convert(tmpdb, ep, geometry_profile, surface_convection)
    window <- window__convert(
        tmpdb,
        ep,
        attr(surface, "table"),
        geometry_profile,
        surface_convection = surface_convection
    )
    door <- door__convert(
        tmpdb,
        ep,
        attr(surface, "table"),
        geometry_profile,
        surface_convection = surface_convection
    )
    shading <- shading__convert(
        tmpdb,
        ep,
        attr(window, "table"),
        geometry_profile
    )

    # TODO: is it possible to have multiple locations in tmpdb?
    conv <- list(
        location = location__convert(tmpdb, ep),
        ground_reflectance = ground_reflectance__convert(tmpdb, ep),
        ground_temperature = ground_temperature__convert(tmpdb, ep),
        building = building__convert(tmpdb, ep),
        zone = zone__convert(tmpdb, ep),
        furniture = furniture__convert(tmpdb, ep),
        surface = surface,
        window = window,
        door = door,
        shading = shading,
        const = const__convert(
            tmpdb,
            ep,
            attr(surface, "table"),
            attr(door, "table")
        ),
        schedule = schedule__convert(
            tmpdb,
            ep,
            options$schedule_format,
            options$schedule_directory,
            # Linked water/RH properties store references in DATA_LONG rather
            # than schedule-named columns, so the generic scan cannot see them.
            extra_ids = if (hvac == "physical") {
                handlers <- DBI::dbReadTable(tmpdb, "AHU")
                ids <- handlers$AHU_ID[
                    handlers$OF_AC_SYS %in%
                        hvac_inventory$systems$AC_SYS_ID
                ]
                properties <- hvac__read_ahu_properties(tmpdb, ids)
                unique(c(
                    properties$schedule_id[!is.na(properties$schedule_id)],
                    hvac__supply_rh_schedule_ids(
                        tmpdb,
                        hvac_inventory$systems$AC_SYS_ID
                    )
                ))
            } else {
                integer()
            }
        ),
        thermostat = if (hvac == "ideal_loads") {
            thermostat__convert(tmpdb, ep)
        },
        outdoor_air = outdoor_air__convert(tmpdb, ep),
        ideal_loads = if (hvac == "ideal_loads") {
            ideal_loads__convert(tmpdb, ep)
        },
        ventilation = ventilation__convert(tmpdb, ep)
    )

    if (internal_gains__has_room_type_data(tmpdb)) {
        conv$internal_gains <- internal_gains__convert(tmpdb, ep)
    }
    conv <- Filter(Negate(is.null), conv)

    # update rleid
    num_obj <- 0L
    for (cv in conv) {
        data.table::set(cv$object, NULL, "rleid", cv$object$rleid + num_obj)
        data.table::set(cv$value, NULL, "rleid", cv$value$rleid + num_obj)
        num_obj <- max(cv$object$rleid)
    }

    obj <- data.table::rbindlist(lapply(conv, .subset2, "object"))
    val <- data.table::rbindlist(lapply(conv, .subset2, "value"))

    add <- eplusr::add_idf_object(
        eplusr::get_priv_env(ep)$idd_env(),
        eplusr::get_priv_env(ep)$idf_env(),
        obj,
        val,
        default = TRUE,
        unique = FALSE,
        empty = TRUE,
        level = "draft"
    )

    if (length(add$changed)) {
        # log
        eplusr::get_priv_env(ep)$log_new_order(add$changed)
        eplusr::get_priv_env(ep)$log_unsaved()
        eplusr::get_priv_env(ep)$log_new_uuid()
        eplusr::get_priv_env(ep)$update_idf_env(add)
    }

    # Source K/SC gives an aggregate window approximation. Preserve its input
    # limitations in the conversion audit without adding solver-specific optics.
    window_diagnostics <- attr(conv$const, "windows")
    if (!is.null(window_diagnostics) && nrow(window_diagnostics) > 0L) {
        warning(
            sprintf(
                paste(
                    "DeST window types %s use nominal SimpleGlazing K/SC",
                    "approximation; source two-face BLACKNESS and detailed",
                    "optics are not expressed. See attr(idf, 'conversion')$windows",
                    "and IDF glazing comments."
                ),
                paste(unique(window_diagnostics$TYPE_ID), collapse = ", ")
            ),
            call. = FALSE
        )
    }

    if (hvac == "physical") {
        ep <- hvac__convert(tmpdb, ep, hvac_options)
    } else {
        # EnergyPlus 9.0.1 rejects non-ASCII object names even when the IDF
        # passes schema validation. Rename objects and references only after
        # the complete selected HVAC graph has been assembled.
        conv__normalize_object_names(ep)
    }

    # Keep the selected saved/fallback ventilation state visible.
    if (!is.null(conv$ventilation)) {
        attr(ep, "ventilation") <- attr(conv$ventilation, "table")
    }
    # Audit the actual returned schema. Transition can rename default choices
    # (for example the ShadowCalculation method) and add or remove objects.
    ep <- conv__transition(ep, target_version, verbose)
    # Record necessary moisture EMS from the emitted model.
    audit <- conv__mode_audit(ep, conversion)
    audit$versions <- list(
        generation = as.character(generation_version),
        target = as.character(target_version),
        transition = if (generation_version == target_version) {
            "none"
        } else {
            "eplusr"
        }
    )
    audit$options <- options
    audit$window_bindings <- window_bindings
    audit$surface_boundaries <- data.table::copy(attr(
        conv$surface,
        "boundary_diagnostics"
    ))
    audit$hvac <- list(
        requested = requested_hvac,
        resolved = hvac,
        source = hvac_inventory,
        terminals = attr(ep, "hvac_terminals"),
        water = attr(ep, "hvac_water"),
        air_treatment = attr(ep, "hvac_air_treatment"),
        effective_options = attr(ep, "hvac_effective_options")
    )
    audit$windows <- if (is.null(window_diagnostics)) {
        data.table::data.table()
    } else {
        data.table::copy(window_diagnostics)
    }
    audit$schedules <- list(
        format = options$schedule_format,
        files = attr(conv$schedule, "files"),
        temperature_conflicts = data.table::copy(attr(
            conv$schedule,
            "temperature_conflicts"
        )),
        run_period = options$run_period,
        calendar = "365 days; no daylight saving; date-based values"
    )
    audit$simulation <- simulation__audit(ep)
    attr(ep, "conversion") <- audit
    ep$Version$comment(
        c(
            un_list(ver$object$comment),
            sprintf(
                "destep IDF syntax: generated with %s; requested %s; transition=%s",
                audit$versions$generation,
                audit$versions$target,
                audit$versions$transition
            ),
            conv__mode_comments(audit),
            window__binding_comments(audit$window_bindings),
            hvac__air_treatment_comments(audit$hvac$air_treatment),
            hvac__terminal_comments(audit$hvac$terminals),
            hvac__water_comments(
                audit$hvac$water,
                audit$hvac$effective_options
            ),
            simulation__comments(audit$simulation)
        ),
        append = NULL
    )
    ep
}

# Give non-ASCII objects stable names accepted by the EnergyPlus 9.0.1 parser.
conv__normalize_object_names <- function(ep) {
    if (as.numeric_version(ep$version()) > as.numeric_version("9.0.1")) {
        return(invisible(ep))
    }

    objects <- unique(ep$to_table()[!is.na(name), .(id, name)])
    objects <- objects[grepl("[^\\x01-\\x7f]", name, perl = TRUE)]
    if (nrow(objects) == 0L) {
        return(invisible(ep))
    }

    # Object ids are deterministic within one converted model, remain short,
    # and avoid collisions between distinct source names after normalization.
    replacements <- sprintf("DeST Object %d", objects$id)
    arguments <- as.list(stats::setNames(objects$id, replacements))
    invisible(do.call(ep$rename, arguments))
    invisible(ep)
}

conv__comment <- function(dest, ep, class = NULL, object = NULL, comment) {
    obj <- eplusr::get_idf_object(
        eplusr::get_priv_env(ep)$idd_env(),
        eplusr::get_priv_env(ep)$idf_env(),
        class,
        object
    )
    val <- eplusr::get_idf_value(
        eplusr::get_priv_env(ep)$idd_env(),
        eplusr::get_priv_env(ep)$idf_env(),
        class,
        object
    )

    if (length(comment) != nrow(obj)) {
        stop(sprintf(
            "The length of 'comment' (%i) did not match the number of objects (%i).",
            length(comment),
            nrow(obj)
        ))
    }

    # make sure there are no line breaks in the comment
    if (any(grepl("\n", comment, fixed = TRUE))) {
        stop("Comments should not contain line breaks.")
    }

    if (is.character(comment)) {
        comment <- as.list(comment)
    }

    if (length(comment) == 1L) {
        comment <- list(comment)
    }
    data.table::set(obj, NULL, "comment", comment)

    list(object = obj, value = val)
}

conv__add <- function(dest, ep, ..., .env = parent.frame()) {
    .env <- force(.env)
    eplusr::expand_idf_dots_value(
        eplusr::get_priv_env(ep)$idd_env(),
        eplusr::get_priv_env(ep)$idf_env(),
        ...,
        .type = "class",
        .complete = TRUE,
        .default = TRUE,
        .scalar = FALSE,
        .pair = TRUE,
        .ref_assign = TRUE,
        .unique = FALSE,
        .empty = TRUE,
        .env = .env
    )
}

# Expand a list of value records into objects of one EnergyPlus class. This is
# the shared boundary for converters that previously rebuilt the same NSE call.
conv__add_objects <- function(dest, ep, class, values) {
    if (!is_string(class)) {
        stop("'class' should be a single character string.", call. = FALSE)
    }
    if (!is.list(values)) {
        stop(
            "'values' should be a list of EnergyPlus value records.",
            call. = FALSE
        )
    }
    if (length(values) == 0L) {
        return(NULL)
    }

    # expand_idf_dots_value() accepts repeated class names as ordinary named
    # arguments, which avoids evaluating dynamically constructed `:=` calls.
    objects <- stats::setNames(values, rep(class, length(values)))
    do.call(conv__add, c(list(dest, ep), objects))
}

# Combine partial EnergyPlus expansion results and rebase their object ids so
# independently generated sections can be appended without collisions.
conv__combine_outputs <- function(outputs, table = NULL) {
    outputs <- Filter(Negate(is.null), outputs)
    if (length(outputs) == 0L) {
        return(NULL)
    }

    num_obj <- 0L
    for (i in seq_along(outputs)) {
        data.table::set(
            outputs[[i]]$object,
            NULL,
            "rleid",
            outputs[[i]]$object$rleid + num_obj
        )
        data.table::set(
            outputs[[i]]$value,
            NULL,
            "rleid",
            outputs[[i]]$value$rleid + num_obj
        )
        num_obj <- max(outputs[[i]]$object$rleid)
    }

    out <- list(
        object = data.table::rbindlist(lapply(outputs, .subset2, "object")),
        value = data.table::rbindlist(lapply(outputs, .subset2, "value"))
    )

    if (is.null(table)) {
        table <- data.table::rbindlist(
            lapply(names(outputs), function(name) {
                tbl <- attr(outputs[[name]], "table")
                if (is.null(tbl)) {
                    return(NULL)
                }
                data.table::set(
                    data.table::copy(tbl),
                    NULL,
                    "SOURCE_TABLE",
                    name
                )
            }),
            fill = TRUE
        )
    }
    # Preserve the source snapshot for diagnostics and downstream converters.
    attr(out, "table") <- table
    out
}

conv__load <- function(dest, ep, ..., .env = parent.frame()) {
    .env <- force(.env)
    eplusr::expand_idf_dots_literal(
        eplusr::get_priv_env(ep)$idd_env(),
        eplusr::get_priv_env(ep)$idf_env(),
        ...,
        .default = TRUE,
        .exact = FALSE
    )
}

conv__field <- function(dest, ep, class, num_fields) {
    fields <- utils::getFromNamespace("get_idd_field", "eplusr")(
        eplusr::get_priv_env(ep)$idd_env(),
        class = rep(class, length(num_fields)),
        field = num_fields,
        complete = TRUE
    )
    fields <- collapse::ss(fields, j = c("rleid", "class_name", "field_index"))
    data.table::setnames(fields, c("id", "class", "index"))
    fields
}

conv__idd_field_name <- function(ep, class, field) {
    fields <- utils::getFromNamespace("get_idd_field", "eplusr")(
        eplusr::get_priv_env(ep)$idd_env(),
        class = class,
        field = field,
        complete = FALSE
    )

    fields$field_name[[1L]]
}

# Resolve requested DeST name-bearing tables and enforce the dependency order
# required by room and surface prefixes.
conv__resolve_name_tables <- function(dest, tables) {
    if (is.null(tables)) {
        # It is possible that some supported tables are absent from a model.
        tables <- MAP_ID_NAME[names(MAP_ID_NAME) %in% DBI::dbListTables(dest)]
    } else {
        if (!is_character(tables)) {
            stop(sprintf(
                "'tables' should be NULL or a character vector but found '%s'",
                class(tables)[1L]
            ))
        }

        tables <- unique(tables)
        matched <- match(tables, names(MAP_ID_NAME), 0L)
        if (any(matched == 0L)) {
            warning(sprintf(
                "Ignore table(s) that do not have name or currently not supported: %s",
                paste(tables[matched == 0L], collapse = ", ")
            ))
        }
        tables <- MAP_ID_NAME[matched]
    }

    if (length(tables) == 0L) {
        return(tables)
    }

    dependencies <- intersect(
        c("OUTSIDE", "GROUND", "ROOM", "SURFACE"),
        names(tables)
    )
    c(tables[dependencies], tables[setdiff(names(tables), dependencies)])
}

# Fill missing DeST names with table-specific defaults while preserving the
# special storey numbering convention for above- and below-ground levels.
conv__fill_missing_names <- function(dest, table, input) {
    missing <- DBI::dbGetQuery(
        dest,
        sprintf(
            "SELECT COUNT(*) AS N FROM `%s` WHERE `%s` = '.' OR `%s` IS NULL",
            table,
            input["name"],
            input["name"]
        )
    )$N
    if (missing == 0L) {
        return(invisible(NULL))
    }

    if (table == "STOREY") {
        DBI::dbExecute(
            dest,
            sprintf(
                "
            UPDATE `%s`
            SET `%s` = CASE
                WHEN `%s` IS NULL OR `%s` = '.'
                THEN
                    '%s ' || CASE
                    WHEN NO >= 0 THEN CAST(NO + 1 AS TEXT)
                    ELSE 'B' || CAST(NO AS TEXT)
                    END
                ELSE `%s`
                END
            ",
                table,
                input["name"],
                input["name"],
                input["name"],
                input["prefix"],
                input["name"]
            )
        )
    } else {
        DBI::dbExecute(
            dest,
            sprintf(
                "UPDATE `%s` SET `%s` = CASE WHEN `%s` IS NULL OR `%s` = '.' THEN '%s' ELSE `%s` END",
                table,
                input["name"],
                input["name"],
                input["name"],
                input["prefix"],
                input["name"]
            )
        )
    }

    invisible(NULL)
}

# Prefix room names with their owning building when DeST contains more than one
# building and bare room names would otherwise collide across the model.
conv__prefix_room_names <- function(dest, input) {
    DBI::dbExecute(
        dest,
        sprintf(
            "
        WITH TMP AS (
            SELECT ROOM.`%s`, BUILDING.`%s` FROM ROOM
            LEFT JOIN STOREY ON ROOM.OF_STOREY = STOREY.`%s`
            LEFT JOIN BUILDING ON STOREY.OF_BUILDING = BUILDING.`%s`
        )
        UPDATE ROOM
        SET `%s` = (
            SELECT TMP.`%s` FROM TMP WHERE TMP.`%s` = ROOM.`%s`
        ) || ' ' || `%s`
        ",
            input["id"],
            MAP_ID_NAME$BUILDING["name"],
            MAP_ID_NAME$STOREY["id"],
            MAP_ID_NAME$BUILDING["id"],
            input["name"],
            MAP_ID_NAME$BUILDING["name"],
            input["id"],
            input["id"],
            input["name"]
        )
    )
}

# Prefix storey names with their owning building in multi-building models.
conv__prefix_storey_names <- function(dest, input) {
    DBI::dbExecute(
        dest,
        sprintf(
            "
        UPDATE STOREY
        SET `%s` = (
            SELECT NAME FROM BUILDING
            WHERE STOREY.OF_BUILDING = BUILDING.`%s`
        ) || ' ' || `%s`
        ",
            input["name"],
            MAP_ID_NAME$BUILDING["id"],
            input["name"]
        )
    )
}

# Derive surface names from the adjacent room or boundary plus the DeST
# enclosure kind, then fall back to the generic surface prefix.
conv__prefix_surface_names <- function(dest, table, input) {
    DBI::dbExecute(
        dest,
        sprintf(
            "
        WITH TMP AS (
            SELECT
                `%s`,
                COALESCE(
                    ROOM.`%s`, OUTSIDE.`%s`, GROUND.`%s`, SHADING.`%s`
                ) AS ROOM_NAME,
                CASE
                    WHEN E.KIND = 1 OR E.KIND = 2 THEN 'Wall'
                    WHEN E.KIND = 3 OR E.KIND = 6 THEN 'Roof'
                    WHEN E.KIND = 4 THEN 'Floor'
                    WHEN E.KIND = 5 THEN 'Ceiling'
                END AS SURFACE_KIND
            FROM SURFACE S
            LEFT JOIN (
                SELECT SIDE1 AS SIDE, KIND FROM MAIN_ENCLOSURE
                UNION
                SELECT SIDE2 AS SIDE, KIND FROM MAIN_ENCLOSURE
            ) E
            ON S.SURFACE_ID = E.SIDE
            LEFT JOIN ROOM
            ON S.OF_ROOM = ROOM.`%s`
            LEFT JOIN OUTSIDE
            ON S.TYPE = 1 AND S.OF_ROOM = OUTSIDE.`%s`
            LEFT JOIN GROUND
            ON S.TYPE = 2 AND S.OF_ROOM = GROUND.`%s`
            LEFT JOIN SHADING
            ON S.TYPE = 3 AND S.OF_ROOM = SHADING.`%s`
        )
        UPDATE SURFACE
        SET `%s` = (
            SELECT TMP.ROOM_NAME || ' ' || TMP.SURFACE_KIND
            FROM TMP
            WHERE SURFACE.`%s` = TMP.`%s`
        )
        ",
            input["id"],
            MAP_ID_NAME$ROOM["name"],
            MAP_ID_NAME$OUTSIDE["name"],
            MAP_ID_NAME$GROUND["name"],
            MAP_ID_NAME$SHADING["name"],
            MAP_ID_NAME$ROOM["id"],
            MAP_ID_NAME$OUTSIDE["id"],
            MAP_ID_NAME$GROUND["id"],
            MAP_ID_NAME$SHADING["id"],
            input["name"],
            input["id"],
            input["id"]
        )
    )

    DBI::dbExecute(
        dest,
        sprintf(
            "UPDATE `%s` SET `%s` = CASE WHEN `%s` IS NULL OR `%s` = '.' THEN '%s' ELSE `%s` END",
            table,
            input["name"],
            input["name"],
            input["name"],
            input["prefix"],
            input["name"]
        )
    )
}

# Add the enclosure kind to construction-library names because one DeST
# construction identifier can occur in several EnergyPlus construction scopes.
conv__prefix_construction_names <- function(dest, table, input) {
    prefixes <- c(
        SYS_OUTWALL = "ExtWall",
        SYS_INWALL = "IntWall",
        SYS_ROOF = "Roof",
        SYS_GROUNDFLOOR = "GroundFloor",
        SYS_MIDDLEFLOOR = "Ceiling",
        SYS_AIRFLOOR = "Airfloor"
    )
    prefix <- unname(prefixes[table])
    if (length(prefix) == 0L || is.na(prefix)) {
        return(invisible(NULL))
    }

    DBI::dbExecute(
        dest,
        sprintf(
            "UPDATE `%s` SET `%s` = '%s - ' || `%s`",
            table,
            input["name"],
            prefix,
            input["name"]
        )
    )
    invisible(NULL)
}

# Apply table-specific contextual prefixes after empty names have been filled.
conv__prefix_contextual_names <- function(dest, table, input) {
    if (
        table == "ROOM" &&
            DBI::dbGetQuery(dest, "SELECT COUNT(*) AS N FROM BUILDING")$N > 1L
    ) {
        conv__prefix_room_names(dest, input)
    } else if (
        table == "STOREY" &&
            DBI::dbGetQuery(dest, "SELECT COUNT(*) AS N FROM BUILDING")$N > 1L
    ) {
        conv__prefix_storey_names(dest, input)
    } else if (
        table == "SURFACE" &&
            DBI::dbGetQuery(dest, "SELECT COUNT(*) AS N FROM ROOM")$N > 1L
    ) {
        conv__prefix_surface_names(dest, table, input)
    } else {
        conv__prefix_construction_names(dest, table, input)
    }
    invisible(NULL)
}

# Add stable numeric suffixes to duplicate names using each table's primary key
# as the deterministic ordering column.
conv__deduplicate_names <- function(dest, table, input) {
    DBI::dbExecute(
        dest,
        sprintf(
            "-- create a temporary table to store the name suffix for duplicated names
        WITH TMP AS (
            SELECT
                `%s`,
                CASE WHEN SUFFIX = 1 THEN '' ELSE ' ' || CAST(SUFFIX - 1 AS TEXT) END AS SUFFIX
            FROM
            (
                SELECT
                    `%s`,
                    ROW_NUMBER() OVER (PARTITION BY %s ORDER BY %s) AS SUFFIX
                FROM `%s`
            )
        )

        -- add suffix to the names
        UPDATE `%s`
        SET `%s` = `%s` || (SELECT SUFFIX FROM TMP WHERE `%s`.`%s` = TMP.`%s`)",
            input["id"],
            input["id"],
            input["name"],
            input["id"],
            table,
            table,
            input["name"],
            input["name"],
            table,
            input["id"],
            input["id"]
        )
    )
}

#' Update NAME column in DeST tables
#'
#' @details
#' In EnergyPlus, object names have to be unique in the scope of their belonging
#' class. However, in DeST, 'NAME' column is likely empty or '.' for most table.
#' This function can be called before actual conversion starts to update the
#' values in 'NAME' column and make sure the follow EnergyPlus requirements.
#'
#' The updating process consists of two steps:
#'
#' 1. Fill empty name column with a prefix if the name is empty or '.'. The
#'    prefix is based on the table name.
#' 1. Add suffix to the names if there are duplicated names.
#'
#' @param dest \[DBIConnection\] A SQLite database connection to the DeST model.
#'
#' @param tables \[character\] Vector of table names to update. If `NULL`, which
#'       is the default, all tables will be updated.
#'
#' @return \[DBIConnection\] The same database connection object.
#'
#' @keywords internal
conv__update_names <- function(dest, tables = NULL) {
    tables <- conv__resolve_name_tables(dest, tables)
    if (length(tables) == 0L) {
        message("No matched table name found. Skip.")
        return(dest)
    }

    for (i in seq_along(tables)) {
        table <- names(tables)[[i]]
        input <- tables[[i]]

        DBI::dbWithTransaction(dest, {
            # in case of unhandled errors, rollback the transaction
            on.exit(
                if (RSQLite::sqliteIsTransacting(dest)) DBI::dbRollback(dest),
                add = TRUE
            )

            # skip empty table
            if (!db_has_rows(dest, table)) {
                DBI::dbBreak()
            }

            conv__fill_missing_names(dest, table, input)

            conv__prefix_contextual_names(dest, table, input)

            conv__deduplicate_names(dest, table, input)
        })
    }

    dest
}

# add comment about the DeST version to be converted
conv__version_comment <- function(dest, ep) {
    ver <- DBI::dbGetQuery(dest, "SELECT MAJOR, MINOR FROM VERSION_CONTROL")
    conv__comment(
        dest,
        ep,
        "Version",
        comment = sprintf("Converted from DeST v%i.%i", ver$MAJOR, ver$MINOR)
    )
}
