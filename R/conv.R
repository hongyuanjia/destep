# TODO: functions to export LIB_* tables from DeST models

MAP_ID_NAME <- list(
    AC_SYS                     = c(id = "AC_SYS_ID",                     name = "NAME",      prefix = "AC Sys"),
    AHU                        = c(id = "AHU_ID",                        name = "NAME",      prefix = "AHU"),
    AIR_SUPPLY_PORT            = c(id = "ID",                            name = "NAME",      prefix = "Air Supply Port"),
    BOILER                     = c(id = "BOILER_ID",                     name = "NAME",      prefix = "Boiler"),
    BUILDING                   = c(id = "BUILDING_ID",                   name = "NAME",      prefix = "Building"),
    CHILLER                    = c(id = "CHILLER_ID",                    name = "NAME",      prefix = "Chiller"),
    COOLINGTOWER               = c(id = "COOLINGTOWER_ID",               name = "NAME",      prefix = "Cooling Tower"),
    DEFAULT_COEF               = c(id = "DEFAULT_COEF_ID",               name = "COEF_NAME", prefix = "Coef"),
    DIST_MODE                  = c(id = "DIST_MODE_ID",                  name = "NAME",      prefix = "Dist Mode"),
    DOOR                       = c(id = "ID",                            name = "NAME",      prefix = "Door"),
    DUCT                       = c(id = "ID",                            name = "NAME",      prefix = "Duct"),
    DUCTNET                    = c(id = "ID",                            name = "NAME",      prefix = "Duct Net"),
    DUCT_JOINT                 = c(id = "ID",                            name = "NAME",      prefix = "Duct Joint"),
    DUCT_TERMINAL              = c(id = "ID",                            name = "NAME",      prefix = "Duct Terminal"),
    ENERGY_DEVICE              = c(id = "ID",                            name = "NAME",      prefix = "Energy Device"),
    ENERGY_HOTWATER            = c(id = "ID",                            name = "NAME",      prefix = "Energy Hot Water"),
    ENERGY_LIFT_ESCALATOR      = c(id = "ID",                            name = "NAME",      prefix = "Energy Lift Escalator"),
    ENERGY_PUMP_FAN            = c(id = "ID",                            name = "NAME",      prefix = "Energy Pump Fan"),
    ENERGY_PUMP_GROUP          = c(id = "ID",                            name = "NAME",      prefix = "Energy Pump Group"),
    ENVIRONMENT                = c(id = "ENVIRONMENT_ID",                name = "NAME",      prefix = "Environment"),
    EQUIPMENT_GAINS            = c(id = "GAIN_ID",                       name = "NAME",      prefix = "Equipment Gains"),
    EQUIPMENT_TEMP             = c(id = "ID",                            name = "NAME",      prefix = "Equipment Temp"),
    FAN_COIL                   = c(id = "ID",                            name = "NAME",      prefix = "Fan Coil"),
    GROUND                     = c(id = "GROUND_ID",                     name = "NAME",      prefix = "Ground"),
    HACNET_BRANCH              = c(id = "BRANCH_ID",                     name = "NAME",      prefix = "Branch"),
    HACNET_NODE                = c(id = "NODE_ID",                       name = "NAME",      prefix = "Node"),
    HACNET_PUMP                = c(id = "PUMP_ID",                       name = "NAME",      prefix = "Pump"),
    HACNET_SUBNET              = c(id = "SUBNET_ID",                     name = "NAME",      prefix = "Subnet"),
    HACNET_TERMINAL            = c(id = "TERMINAL_ID",                   name = "NAME",      prefix = "Terminal"),
    HACNET_VALVE               = c(id = "VALVE_ID",                      name = "NAME",      prefix = "Valve"),
    HEATEXCHANGER              = c(id = "HEATEXCHANGER_ID",              name = "NAME",      prefix = "Heat Exchanger"),
    HEATING_PIPE               = c(id = "HEATING_PIPE_ID",               name = "NAME",      prefix = "Heating Pipe"),
    HEATING_SYSTEM             = c(id = "HEATING_SYSTEM_ID",             name = "NAME",      prefix = "Heating System"),
    LIB_BOILER                 = c(id = "LIB_BOILER_ID",                 name = "NAME",      prefix = "Lib Boiler"),
    LIB_CHILLER                = c(id = "LIB_CHILLER_ID",                name = "NAME",      prefix = "Lib Chiller"),
    LIB_COOLINGTOWER           = c(id = "LIB_COOLINGTOWER_ID",           name = "NAME",      prefix = "Lib Cooling Tower"),
    LIB_CURVE                  = c(id = "LIB_CURVE_ID",                  name = "NAME",      prefix = "Lib Curve"),
    LIB_HEATEXCHANGER          = c(id = "LIB_HEATEXCHANGER_ID",          name = "NAME",      prefix = "Lib Heat Exchanger"),
    LIB_PHASE_CHANGE_MAT       = c(id = "LIB_PHASE_CHANGE_MAT_ID",       name = "NAME",      prefix = "Lib Phase Change Mat"),
    LIB_PRODUCT                = c(id = "PRODUCT_ID",                    name = "NAME",      prefix = "Lib Product"),
    LIB_PUMP                   = c(id = "LIB_PUMP_ID",                   name = "NAME",      prefix = "Lib Pump"),
    LIB_ROOFUNIT_DEVICE        = c(id = "ID",                            name = "NAME",      prefix = "Lib Roof Unit Device"),
    LIB_SHADING                = c(id = "ID",                            name = "NAME",      prefix = "Lib Shading"),
    LIB_SOLAR_ENERGY_COLLECTOR = c(id = "LIB_SOLAR_ENERGY_COLLECTOR_ID", name = "NAME",      prefix = "Lib Solar Energy Collector"),
    LIB_VRV_SOURCE             = c(id = "ID",                            name = "NAME",      prefix = "Lib VRV Source"),
    LIB_VRV_TERMINAL           = c(id = "ID",                            name = "NAME",      prefix = "Lib VRV Terminal"),
    LIB_WIND_RATIO_MODEL       = c(id = "ID",                            name = "NAME",      prefix = "Lib Wind Ratio Model"),
    LIB_WIND_RATIO_TYPE        = c(id = "ID",                            name = "NAME",      prefix = "Lib Wind Ratio Type"),
    LIGHT_GAINS                = c(id = "GAIN_ID",                       name = "NAME",      prefix = "Light"),
    OCCUPANT_GAINS             = c(id = "GAIN_ID",                       name = "NAME",      prefix = "Occupant"),
    OUTSIDE                    = c(id = "OUTSIDE_ID",                    name = "NAME",      prefix = "Outside"),
    PUMP                       = c(id = "PUMP_ID",                       name = "NAME",      prefix = "Pump"),
    ROOM                       = c(id = "ID",                            name = "NAME",      prefix = "Room"),
    ROOM_GROUP                 = c(id = "ROOM_GROUP_ID",                 name = "NAME",      prefix = "Room Group"),
    SCHEDULE_YEAR              = c(id = "SCHEDULE_ID",                   name = "NAME",      prefix = "Schedule Year"),
    SHADING                    = c(id = "ID",                            name = "NAME",      prefix = "Shading"),
    SKY                        = c(id = "SKY_ID",                        name = "NAME",      prefix = "Sky"),
    STOREY                     = c(id = "ID",                            name = "NAME",      prefix = "Storey"),
    SURFACE                    = c(id = "SURFACE_ID",                    name = "NAME",      prefix = "Surface"),
    SYS_AIRFLOOR               = c(id = "STRUCT_ID",                     name = "CNAME",     prefix = "Sys Airfloor"),
    SYS_APP_MATERIAL           = c(id = "APP_MATERIAL_ID",               name = "CNAME",     prefix = "Sys App Material"),
    SYS_CITY                   = c(id = "CITY_ID",                       name = "CNAME",     prefix = "Sys City"),
    SYS_CURTAIN                = c(id = "CURTAIN_ID",                    name = "CNAME",     prefix = "Sys Curtain"),
    SYS_DOOR                   = c(id = "DOOR_ID",                       name = "CNAME",     prefix = "Sys Door"),
    SYS_GROUNDFLOOR            = c(id = "STRUCT_ID",                     name = "CNAME",     prefix = "Sys Groundfloor"),
    SYS_GROUPS                 = c(id = "TYPE",                          name = "CNAME",     prefix = "Sys Groups"),
    SYS_INWALL                 = c(id = "STRUCT_ID",                     name = "CNAME",     prefix = "Sys Inwall"),
    SYS_MATERIAL               = c(id = "MATERIAL_ID",                   name = "CNAME",     prefix = "Sys Material"),
    SYS_MIDDLEFLOOR            = c(id = "STRUCT_ID",                     name = "CNAME",     prefix = "Sys Middlefloor"),
    SYS_OUTWALL                = c(id = "STRUCT_ID",                     name = "CNAME",     prefix = "Sys Outwall"),
    SYS_ROOF                   = c(id = "STRUCT_ID",                     name = "CNAME",     prefix = "Sys Roof"),
    SYS_SHADING                = c(id = "SHIELD_ID",                     name = "CNAME",     prefix = "Sys Shading"),
    SYS_WINDOW                 = c(id = "WINDOW_ID",                     name = "CNAME",     prefix = "Sys Window"),
    VRV_SOURCE                 = c(id = "ID",                            name = "NAME",      prefix = "VRV Source"),
    VRV_TERMINAL               = c(id = "ID",                            name = "NAME",      prefix = "VRV Terminal"),
    WATER_SYS                  = c(id = "WATER_SYS_ID",                  name = "NAME",      prefix = "Water Sys"),
    WATER_SYSTEM               = c(id = "WATER_SYSTEM_ID",               name = "NAME",      prefix = "Water System"),
    WINDOW                     = c(id = "ID",                            name = "NAME",      prefix = "Window"),
    WINDOW_TYPE_DATA           = c(id = "ID",                            name = "NAME",      prefix = "Window Type Data")
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
#' @return \[eplusr::Idf\] The converted EnergyPlus model.
#'
#' @export
# TODO: How about STOREY_GROUP?
to_eplus <- function(dest, ver = "latest", copy = TRUE, verbose = FALSE) {
    if (is_string(dest) && file.exists(dest)) {
        dest <- read_dest(dest, verbose = verbose)
        on.exit(DBI::dbDisconnect(dest), add = TRUE)
    } else if (!inherits(dest, "DBIConnection")) {
        stop("'dest' should be a path to a DeST model file or a DBIConnection object.")
    }

    if (!is_flag(copy)) {
        stop("'copy' should be a single logical value of 'TRUE' or 'FALSE'")
    }
    if (!is_flag(verbose)) {
        stop("'verbose' should be a single logical value of 'TRUE' or 'FALSE'")
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

    # create an empty EnergyPlus model
    if (verbose) {
        ep <- eplusr::with_verbose(eplusr::empty_idf(ver))
    } else {
        ep <- eplusr::empty_idf(ver)
    }

    # add GlobalGeometryRules
    ep$add("GlobalGeometryRules" := list(
        starting_vertex_position                      = "UpperLeftCorner",
        vertex_entry_direction                        = "Counterclockwise",
        # DeST POINT coordinates share one building-wide drawing origin. Keep
        # every EnergyPlus Zone origin at zero and use relative coordinates so
        # Building North Axis can rotate that drawing to true north.
        coordinate_system                             = "Relative",
        daylighting_reference_point_coordinate_system = "Relative"
    ))

    # DeST stores model inputs but no EnergyPlus simulation period.  Add a
    # calendar-year period so each converted model can run against an EPW file.
    ep$add("RunPeriod" := list(
        name                                   = "Annual",
        begin_month                            = 1L,
        begin_day_of_month                     = 1L,
        end_month                              = 12L,
        end_day_of_month                       = 31L,
        # DeST SCHEDULE_YEAR stores the first seven profiles as Monday through
        # Sunday. Fix the simulation calendar to that same convention instead
        # of inheriting a weather-file weekday that can shift every schedule.
        day_of_week_for_start_day              = "Monday",
        use_weather_file_holidays_and_special_days = "Yes",
        use_weather_file_daylight_saving_period = "Yes",
        apply_weekend_holiday_rule             = "No",
        use_weather_file_rain_indicators       = "Yes",
        use_weather_file_snow_indicators       = "Yes"
    ))

    # update object names and make sure all names are unique
    conv__update_names(tmpdb)

    # update Version comments
    ver <- conv__version_comment(tmpdb, ep)
    ep$Version$comment(un_list(ver$object$comment))

    # Surface part geometry must be available when a window crosses a topology
    # split, because each clipped window piece references exactly one host part.
    geometry_profile <- eplus_geom__profile(ep$version())
    surface <- surface__convert(tmpdb, ep, geometry_profile)
    window <- window__convert(
        tmpdb, ep, attr(surface, "table"), geometry_profile
    )

    # TODO: is it possible to have multiple locations in tmpdb?
    conv <- list(
        location = location__convert(tmpdb, ep),
        ground_temperature = ground_temperature__convert(tmpdb, ep),
        building = building__convert(tmpdb, ep),
        zone     = zone__convert(tmpdb, ep),
        surface  = surface,
        window   = window,
        const    = const__convert(tmpdb, ep),
        schedule = schedule__convert(tmpdb, ep),
        thermostat = thermostat__convert(tmpdb, ep),
        outdoor_air = outdoor_air__convert(tmpdb, ep),
        ideal_loads = ideal_loads__convert(tmpdb, ep),
        ventilation = ventilation__convert(tmpdb, ep)
    )

    if (any(vapply(
        c("OCCUPANT_GAINS", "LIGHT_GAINS", "EQUIPMENT_GAINS"),
        db_has_rows, logical(1L), dest = tmpdb
    ))) {
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
        obj, val,
        default = TRUE, unique = FALSE, empty = TRUE,
        level = "draft"
    )

    if (length(add$changed)) {
        # log
        eplusr::get_priv_env(ep)$log_new_order(add$changed)
        eplusr::get_priv_env(ep)$log_unsaved()
        eplusr::get_priv_env(ep)$log_new_uuid()
        eplusr::get_priv_env(ep)$update_idf_env(add)
    }

    ep
}

conv__comment <- function(dest, ep, class = NULL, object = NULL, comment) {
    obj <- eplusr::get_idf_object(
        eplusr::get_priv_env(ep)$idd_env(),
        eplusr::get_priv_env(ep)$idf_env(),
        class, object
    )
    val <- eplusr::get_idf_value(
        eplusr::get_priv_env(ep)$idd_env(),
        eplusr::get_priv_env(ep)$idf_env(),
        class, object
    )

    if (length(comment) != nrow(obj)) {
        stop(sprintf(
            "The length of 'comment' (%i) did not match the number of objects (%i).",
            length(comment), nrow(obj)
        ))
    }

    # make sure there are no line breaks in the comment
    if (any(grepl("\n", comment, fixed = TRUE))) {
        stop("Comments should not contain line breaks.")
    }

    if (is.character(comment)) comment <- as.list(comment)

    if (length(comment) == 1L) comment <- list(comment)
    data.table::set(obj, NULL, "comment", comment)

    list(object = obj, value = val)
}

conv__add <- function(dest, ep, ..., .env = parent.frame()) {
    .env <- force(.env)
    eplusr::expand_idf_dots_value(
        eplusr::get_priv_env(ep)$idd_env(),
        eplusr::get_priv_env(ep)$idf_env(),
        ...,
        .type = "class", .complete = TRUE, .default = TRUE,
        .scalar = FALSE, .pair = TRUE, .ref_assign = TRUE,
        .unique = FALSE, .empty = TRUE, .env = .env
    )
}

# Expand a list of value records into objects of one EnergyPlus class. This is
# the shared boundary for converters that previously rebuilt the same NSE call.
conv__add_objects <- function(dest, ep, class, values) {
    if (!is_string(class)) {
        stop("'class' should be a single character string.", call. = FALSE)
    }
    if (!is.list(values)) {
        stop("'values' should be a list of EnergyPlus value records.", call. = FALSE)
    }
    if (length(values) == 0L) return(NULL)

    # expand_idf_dots_value() accepts repeated class names as ordinary named
    # arguments, which avoids evaluating dynamically constructed `:=` calls.
    objects <- stats::setNames(values, rep(class, length(values)))
    do.call(conv__add, c(list(dest, ep), objects))
}

# Combine partial EnergyPlus expansion results and rebase their object ids so
# independently generated sections can be appended without collisions.
conv__combine_outputs <- function(outputs, table = NULL) {
    outputs <- Filter(Negate(is.null), outputs)
    if (length(outputs) == 0L) return(NULL)

    num_obj <- 0L
    for (i in seq_along(outputs)) {
        data.table::set(outputs[[i]]$object, NULL, "rleid", outputs[[i]]$object$rleid + num_obj)
        data.table::set(outputs[[i]]$value, NULL, "rleid", outputs[[i]]$value$rleid + num_obj)
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
                if (is.null(tbl)) return(NULL)
                data.table::set(data.table::copy(tbl), NULL, "SOURCE_TABLE", name)
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

    if (length(tables) == 0L) return(tables)

    dependencies <- intersect(
        c("OUTSIDE", "GROUND", "ROOM", "SURFACE"), names(tables)
    )
    c(tables[dependencies], tables[setdiff(names(tables), dependencies)])
}

# Fill missing DeST names with table-specific defaults while preserving the
# special storey numbering convention for above- and below-ground levels.
conv__fill_missing_names <- function(dest, table, input) {
    missing <- DBI::dbGetQuery(dest, sprintf(
        "SELECT COUNT(*) AS N FROM `%s` WHERE `%s` = '.' OR `%s` IS NULL",
        table, input["name"], input["name"]
    ))$N
    if (missing == 0L) return(invisible(NULL))

    if (table == "STOREY") {
        DBI::dbExecute(dest, sprintf(
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
            table, input["name"], input["name"], input["name"],
            input["prefix"], input["name"]
        ))
    } else {
        DBI::dbExecute(dest, sprintf(
            "UPDATE `%s` SET `%s` = CASE WHEN `%s` IS NULL OR `%s` = '.' THEN '%s' ELSE `%s` END",
            table, input["name"], input["name"], input["name"],
            input["prefix"], input["name"]
        ))
    }

    invisible(NULL)
}

# Prefix room names with their owning building when DeST contains more than one
# building and bare room names would otherwise collide across the model.
conv__prefix_room_names <- function(dest, input) {
    DBI::dbExecute(dest, sprintf(
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
        input["id"], MAP_ID_NAME$BUILDING["name"],
        MAP_ID_NAME$STOREY["id"], MAP_ID_NAME$BUILDING["id"],
        input["name"], MAP_ID_NAME$BUILDING["name"],
        input["id"], input["id"], input["name"]
    ))
}

# Prefix storey names with their owning building in multi-building models.
conv__prefix_storey_names <- function(dest, input) {
    DBI::dbExecute(dest, sprintf(
        "
        UPDATE STOREY
        SET `%s` = (
            SELECT NAME FROM BUILDING
            WHERE STOREY.OF_BUILDING = BUILDING.`%s`
        ) || ' ' || `%s`
        ",
        input["name"], MAP_ID_NAME$BUILDING["id"], input["name"]
    ))
}

# Derive surface names from the adjacent room or boundary plus the DeST
# enclosure kind, then fall back to the generic surface prefix.
conv__prefix_surface_names <- function(dest, table, input) {
    DBI::dbExecute(dest, sprintf(
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
        MAP_ID_NAME$ROOM["name"], MAP_ID_NAME$OUTSIDE["name"],
        MAP_ID_NAME$GROUND["name"], MAP_ID_NAME$SHADING["name"],
        MAP_ID_NAME$ROOM["id"], MAP_ID_NAME$OUTSIDE["id"],
        MAP_ID_NAME$GROUND["id"], MAP_ID_NAME$SHADING["id"],
        input["name"], input["id"], input["id"]
    ))

    DBI::dbExecute(dest, sprintf(
        "UPDATE `%s` SET `%s` = CASE WHEN `%s` IS NULL OR `%s` = '.' THEN '%s' ELSE `%s` END",
        table, input["name"], input["name"], input["name"],
        input["prefix"], input["name"]
    ))
}

# Add the enclosure kind to construction-library names because one DeST
# construction identifier can occur in several EnergyPlus construction scopes.
conv__prefix_construction_names <- function(dest, table, input) {
    prefixes <- c(
        SYS_OUTWALL = "ExtWall", SYS_INWALL = "IntWall",
        SYS_ROOF = "Roof", SYS_GROUNDFLOOR = "GroundFloor",
        SYS_MIDDLEFLOOR = "Ceiling", SYS_AIRFLOOR = "Airfloor"
    )
    prefix <- unname(prefixes[table])
    if (length(prefix) == 0L || is.na(prefix)) return(invisible(NULL))

    DBI::dbExecute(dest, sprintf(
        "UPDATE `%s` SET `%s` = '%s - ' || `%s`",
        table, input["name"], prefix, input["name"]
    ))
    invisible(NULL)
}

# Apply table-specific contextual prefixes after empty names have been filled.
conv__prefix_contextual_names <- function(dest, table, input) {
    if (table == "ROOM" &&
        DBI::dbGetQuery(dest, "SELECT COUNT(*) AS N FROM BUILDING")$N > 1L) {
        conv__prefix_room_names(dest, input)
    } else if (table == "STOREY" &&
        DBI::dbGetQuery(dest, "SELECT COUNT(*) AS N FROM BUILDING")$N > 1L) {
        conv__prefix_storey_names(dest, input)
    } else if (table == "SURFACE" &&
        DBI::dbGetQuery(dest, "SELECT COUNT(*) AS N FROM ROOM")$N > 1L) {
        conv__prefix_surface_names(dest, table, input)
    } else {
        conv__prefix_construction_names(dest, table, input)
    }
    invisible(NULL)
}

# Add stable numeric suffixes to duplicate names using each table's primary key
# as the deterministic ordering column.
conv__deduplicate_names <- function(dest, table, input) {
    DBI::dbExecute(dest, sprintf(
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
        input["id"], input["id"], input["name"], input["id"], table,
        table, input["name"], input["name"], table, input["id"], input["id"]
    ))
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
            if (!db_has_rows(dest, table)) DBI::dbBreak()

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
    conv__comment(dest, ep, "Version",
        comment = sprintf("Converted from DeST v%i.%i", ver$MAJOR, ver$MINOR)
    )
}
