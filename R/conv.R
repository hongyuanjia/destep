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

ENUM_SCH_DAYTYPE <- c(
    Sunday = 1L, Monday = 2L, Tuesday = 3L, Wednesday = 4L, Thursday = 5L, Friday = 6L, Saturday = 7L,
    Holiday = 8L, SummerDesignDay = 9L, WinterDesignDay = 10L,
    CustomDay1 = 11L, CustomDay2 = 12L,
    Weekday = 13L, Weekend = 14L, AllDay = 15L, AllOtherDay = 16L
)
ENUM_SCH_DAYTYPE_NORMAL <- ENUM_SCH_DAYTYPE[ENUM_SCH_DAYTYPE <= ENUM_SCH_DAYTYPE["CustomDay2"]]
ENUM_SCH_DAYTYPE_SPECIAL <- ENUM_SCH_DAYTYPE[ENUM_SCH_DAYTYPE > ENUM_SCH_DAYTYPE["CustomDay2"]]
ENUM_SCH_DAYTYPE_WEEKEND <- c(ENUM_SCH_DAYTYPE["Saturday"], ENUM_SCH_DAYTYPE["Sunday"])
ENUM_SCH_DAYTYPE_WEEKDAY <- ENUM_SCH_DAYTYPE[ENUM_SCH_DAYTYPE["Monday"]:ENUM_SCH_DAYTYPE["Friday"]]

#' Convert a DeST model to EnergyPlus model
#'
#' @param dest A \[string or DBIConnection\] path to a DeST model file or a
#'        DBIConnection object.
#'
#' @param ver \[string\] A character string specifying the EnergyPlus version.
#'        It can be `"latest"`, which is the default, to indicate using the
#'        latest EnergyPlus version supported by the
#'        \{[eplusr](https://cran.r-project.org/package=eplusr)\} package.
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
# TODO: How about ROOM_GROUP? and STOREY_GROUP?
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
        on.exit({ DBI::dbDisconnect(tmpdb); unlink(path_tmpdb) }, add = TRUE)
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
        coordinate_system                             = "Relative",
        daylighting_reference_point_coordinate_system = "Relative"
    ))

    # update object names and make sure all names are unique
    destep_update_name(tmpdb)

    # update Version comments
    # TODO: currently eplusr does not support updating comments for Version
    # object
    # TODO: change to use `IdfObject$comment()`
    ver <- destep_comment_version(tmpdb, ep)
    idf_env_obj <- eplusr::get_priv_env(ep)$idf_env()$object
    data.table::set(idf_env_obj,
        which(eplusr::get_priv_env(ep)$idf_env()$obj == "Version"),
        "comment", list(ver$value$comment)
    )

    # TODO: is it possible to have multiple locations in tmpdb?
    conv <- list(
        location = destep_conv_location(tmpdb, ep),
        building = destep_conv_building(tmpdb, ep),
        zone     = destep_conv_zone(tmpdb, ep),
        surface  = destep_conv_surface(tmpdb, ep),
        # window  = destep_conv_window(tmpdb, ep)
        const    = destep_conv_const(tmpdb, ep),
        schedule = destep_conv_schedule(tmpdb, ep)
    )

    # update rleid
    num_obj <- 0L
    for (cv in conv) {
        data.table::set(cv$object, NULL, "rleid", cv$object$rleid + num_obj)
        data.table::set(cv$value,  NULL, "rleid", cv$value$rleid + num_obj)
        num_obj <- max(cv$object$rleid)
    }

    obj <- data.table::rbindlist(lapply(conv, .subset2, "object"))
    val <- data.table::rbindlist(lapply(conv, .subset2, "value"))

    add <- eplusr::add_idf_object(
        eplusr::get_priv_env(ep)$idd_env(),
        eplusr::get_priv_env(ep)$idf_env(),
        obj, val, default = TRUE, unique = FALSE, empty = TRUE,
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

destep_comment <- function(dest, ep, class = NULL, object = NULL, comment) {
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

destep_add <- function(dest, ep, ..., .env = parent.frame()) {
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

destep_load <- function(dest, ep, ..., .env = parent.frame()) {
    .env <- force(.env)
    eplusr::expand_idf_dots_literal(
        eplusr::get_priv_env(ep)$idd_env(),
        eplusr::get_priv_env(ep)$idf_env(),
        ...,
        .default = TRUE,
        .exact = FALSE
    )
}

destep_field <- function(dest, ep, class, num_fields) {
    fields <- getFromNamespace("get_idd_field", "eplusr")(
        eplusr::get_priv_env(ep)$idd_env(),
        class = rep(class, length(num_fields)),
        field = num_fields,
        complete = TRUE
    )
    fields <- collapse::ss(fields, j = c("rleid", "class_name", "field_index"))
    data.table::setnames(fields, c("id", "class", "index"))
    fields
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
destep_update_name <- function(dest, tables = NULL) {
    if (is.null(tables)) {
        # it is possible that some tables are not in the database
        tables <- MAP_ID_NAME[names(MAP_ID_NAME) %in% DBI::dbListTables(dest)]
    } else {
        if (!is_character(tables)) {
            stop(sprintf(
                "'tables' should be NULL or a character vector but found '%s'",
                class(tables)[1L]
            ))
        }

        tables <- unique(tables)
        m <- match(tables, names(MAP_ID_NAME), 0L)
        if (any(m == 0L)) {
            warning(sprintf(
                "Ignore table(s) that do not have name or currently not supported: %s",
                paste(tables[m == 0L], collapse = ", ")
            ))
        }
        tables <- MAP_ID_NAME[m]

        if (length(tables) == 0L) {
            message("No matched table name found. Skip.")
            return(dest)
        }
    }

    # first fill empty name column
    # make sure tables are handled in the following order:
    # 1. OUTSIDE | GROUND -> they are used in SURFACE$OF_ROOM
    # 2. ROOM
    # 3. SURFACE
    tbls <- intersect(c("OUTSIDE", "GROUND", "ROOM", "SURFACE"), names(tables))
    tables[match(tbls, names(tables), 0L)] <- tables[tbls]
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
            if (DBI::dbGetQuery(dest, sprintf("SELECT COUNT(*) AS N FROM `%s`", table))$N == 0L) {
                DBI::dbBreak()
            }

            # TODO: should it be quicker if we directly update the table in the
            # database instead of checking first?
            n <- DBI::dbGetQuery(dest, sprintf(
                "SELECT COUNT(*) AS N FROM `%s` WHERE `%s` = '.' OR `%s` IS NULL",
                table, input["name"], input["name"]
            ))$N

            # fill empty name column first
            if (n > 0L) {
                # name storey based on storey number
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
                        table, input["name"], input["name"], input["name"], input["prefix"], input["name"]
                    ))
                } else {
                    DBI::dbExecute(dest, sprintf(
                        "UPDATE `%s` SET `%s` = CASE WHEN `%s` IS NULL OR `%s` = '.' THEN '%s' ELSE `%s` END",
                        table, input["name"], input["name"], input["name"], input["prefix"], input["name"]
                    ))
                }
            }

            # if more than one building exist, prefix each zone name with the
            # corresponding building name
            if (table == "ROOM" && DBI::dbGetQuery(dest, "SELECT COUNT(*) AS N FROM BUILDING")$N > 1L) {
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
                    MAP_ID_NAME$STOREY["id"],
                    MAP_ID_NAME$BUILDING["id"],
                    input["name"],
                    MAP_ID_NAME$BUILDING["name"], input["id"], input["id"],
                    input["name"]
                ))
            # if more than one building exist, prefix each storey name with the
            # corresponding building name
            } else if (table == "STOREY" && DBI::dbGetQuery(dest, "SELECT COUNT(*) AS N FROM BUILDING")$N > 1L) {
                DBI::dbExecute(dest, sprintf(
                    "
                    UPDATE STOREY
                    SET `%s` = (
                        SELECT NAME FROM BUILDING
                        WHERE STOREY.OF_BUILDING = BUILDING.`%s`
                    ) || ' ' || `%s`
                    ",
                    input["name"], input["name"], MAP_ID_NAME$BUILDING["id"]
                ))
            # prefix each surface name with the corresponding room name
            } else if (table == "SURFACE" && DBI::dbGetQuery(dest, "SELECT COUNT(*) AS N FROM ROOM")$N > 1L) {
                DBI::dbExecute(dest, sprintf(
                    "
                    -- get the corresponding surface type
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
                        -- consider both sides of the main enclosure
                        LEFT JOIN (
                            SELECT SIDE1 AS SIDE, KIND FROM MAIN_ENCLOSURE
                            UNION
                            SELECT SIDE2 AS SIDE, KIND FROM MAIN_ENCLOSURE
                        ) E
                        -- NOTE: only consider surfaces that are part of a room,
                        -- adjacent to the outside, or on the ground, or are
                        -- shadings
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

                # NOTE: for surface that are not part of a room, adjacent to the
                # outside, or on the ground, or shadings, the name will be just
                # the prefix
                DBI::dbExecute(dest, sprintf(
                    "UPDATE `%s` SET `%s` = CASE WHEN `%s` IS NULL OR `%s` = '.' THEN '%s' ELSE `%s` END",
                    table, input["name"], input["name"], input["name"], input["prefix"], input["name"]
                ))
            # NOTE: it is possible that the same construction is used for
            # different kinds. Here we append the kind to the construction name
            # to make it unique.
            } else if (table == "SYS_OUTWALL") {
                DBI::dbExecute(dest, sprintf(
                    "UPDATE SYS_OUTWALL SET `%s` = 'ExtWall - ' || `%s`",
                    input["name"], input["name"]
                ))
            } else if (table == "SYS_INWALL") {
                DBI::dbExecute(dest, sprintf(
                    "UPDATE SYS_INWALL SET `%s` = 'IntWall - ' || `%s`",
                    input["name"], input["name"]
                ))
            } else if (table == "SYS_ROOF") {
                DBI::dbExecute(dest, sprintf(
                    "UPDATE SYS_ROOF SET `%s` = 'Roof - ' || `%s`",
                    input["name"], input["name"]
                ))
            } else if (table == "SYS_GROUNDFLOOR") {
                DBI::dbExecute(dest, sprintf(
                    "UPDATE SYS_GROUNDFLOOR SET `%s` = 'GroundFloor - ' || `%s`",
                    input["name"], input["name"]
                ))
            } else if (table == "SYS_MIDDLEFLOOR") {
                DBI::dbExecute(dest, sprintf(
                    "UPDATE SYS_MIDDLEFLOOR SET `%s` = 'Ceiling - ' || `%s`",
                    input["name"], input["name"]
                ))
            } else if (table == "SYS_AIRFLOOR") {
                DBI::dbExecute(dest, sprintf(
                    "UPDATE SYS_AIRFLOOR SET `%s` = 'Airfloor - ' || `%s`",
                    input["name"], input["name"]
                ))
            }

            # add suffix to duplicated names
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
        })
    }

    dest
}

# add comment about the DeST version to be converted
destep_comment_version <- function(dest, ep) {
    ver <- DBI::dbGetQuery(dest, "SELECT MAJOR, MINOR FROM VERSION_CONTROL")
    destep_comment(dest, ep, "Version",
        comment = sprintf("Converted from DeST v%i.%i", ver$MAJOR, ver$MINOR)
    )
}

# BUILDING -> Building
# TODO: one IDF per BUILDING?
destep_conv_building <- function(dest, ep, which = NULL) {
    bld <- DBI::dbGetQuery(dest, "SELECT BUILDING_ID AS ID, NAME FROM BUILDING")
    assert_unique_name(bld$NAME, "building")
    data.table::setDT(bld)

    if (!is.null(which)) {
        if (is_integerish(which)) {
            # check if the index is out of bound
            if (any(which < 1L) || any(which > nrow(bld))) {
                stop(sprintf(
                    "'which' out of bound. Should be in [1, %i], but found '%i'.",
                    nrow(bld), which
                ))
            }
            bld <- bld[unique(which)]
        } else if (is_character(which)) {
            # check if which is valid building name
            if (any(!which %in% bld$NAME)) {
                stop(sprintf(
                    "'%s' is not a valid building name in current DeST model. Available names: %s",
                    which, paste(unique(bld$NAME), collapse = ", ")
                ))
            }
            bld <- bld[J(unique(which)), on = "NAME"]
        } else {
            stop("'which' should be an integer or character vector.")
        }
    }

    out <- destep_add(dest, ep, "Building" := list(name = bld$NAME))

    # always attach the table to the output in case it is useful later
    attr(out, "table") <- bld

    out
}

# ENVIRONMENT -> Site:Location
# TODO: add time zone
destep_conv_location <- function(dest, ep) {
    loc <- DBI::dbGetQuery(dest,
        "SELECT
            ENVIRONMENT_ID       AS ID,
            NAME,
            round(LATITUDE, 2)  AS LATITUDE,
            round(LONGITUDE, 2) AS LONGITUDE,
            round(ELEVATION, 2) AS ELEVATION
        FROM ENVIRONMENT"
    )
    assert_unique_name(loc$NAME, "environment")
    data.table::setDT(loc)

    out <- destep_add(dest, ep,
        "Site:Location" := list(
            name      = loc$NAME,
            latitude  = loc$LATITUDE,
            longitude = loc$LONGITUDE,
            # TODO: handle time zone
            time_zone = NULL,
            elevation = loc$ELEVATION
        )
    )

    # always attach the table to the output in case it is useful later
    attr(out, "table") <- loc

    out
}

# ROOM -> Zone
# ROOM$OF_STOREY -> STOREY$ID + MULTIPLE
# ROOM$OF_STORY -> ZoneList
destep_conv_zone <- function(dest, ep) {
    room <- DBI::dbGetQuery(dest,
        "SELECT
            R.ID                 AS ID,
            R.NAME               AS NAME,
            ROUND(R.VOLUME, 3)   AS VOLUME,
            ROUND(R.AREA, 3)     AS AREA,
            S.NAME               AS STOREY_NAME,
            S.MULTIPLE           AS STOREY_MULTIPLIER
        FROM ROOM R
        INNER JOIN STOREY S
        ON R.OF_STOREY = S.ID
        "
    )
    assert_unique_name(room$NAME, "room")
    data.table::setDT(room)

    #  construct ZoneList input
    zonelist <- lapply(room[, by = "STOREY_NAME",
        list(value = list(c(.BY$STOREY_NAME, NAME)))
    ]$value, as.list)

    # construct ZoneGroup input
    # STOREY$MULTIPLE -> ZoneGroup$'Zone List Multipler'
    # NOTE: here we use the same name for ZoneList and ZoneGroup the name field
    # in ZoneGroup does not have an \reference property and thus is not referred
    # by any other objects
    zonegroup <- room[, by = "STOREY_NAME",
        list(
            name = .BY$STOREY_NAME,
            zone_list_name = .BY$STOREY_NAME,
            zone_list_multiplier = STOREY_MULTIPLIER[[1L]]
        )
    ]

    out <- eval(as.call(c(
        destep_add, dest, ep,
        # Zone
        quote("Zone" := list(
            name = room$NAME,
            # NOTE: DeST use room types to refer to default loads and etc.
            # However, 'type' field is not used by EnergyPlus and is always set
            # to 1.
            type = NULL,
            # NOTE: we did not set zone multiplier here, but instead follow DeST
            #       convention, i.e., setting the multiplier in ZoneGroup
            multiplier = NULL,
            # STOREY$HEIGHT
            ceiling_height = room$HEIGHT,
            volume         = room$VOLUME,
            floor_area     = room$AREA
        )),

        # ZoneList
        lapply(zonelist, function(zl) bquote("ZoneList" := .(zl))),

        # ZoneGroup
        bquote(
            "ZoneGroup" := list(
                name = .(zonegroup$name),
                zone_list_name = .(zonegroup$zone_list_name),
                zone_list_multiplier = .(zonegroup$zone_list_multiplier)
            )
        )
    )))

    # always attach the table to the output in case it is useful later
    attr(out, "table") <- room

    out
}

# MAIN_ENCLOSURE$CONSTRUCTION -> Construction -> Material
destep_conv_const <- function(dest, ep) {
    if (DBI::dbGetQuery(dest, "SELECT COUNT(*) AS N FROM MAIN_ENCLOSURE")$N == 0L) {
        return(NULL)
    }

    # The construction ID refers to different tables based on the kind of the
    # construction:
    #
    # KIND = 1 -> SYS_OUTWALL     -> SYS_OUTWALL_MATERIAL     -> MATERIAL
    # KIND = 2 -> SYS_INWALL      -> SYS_INWALL_MATERIAL      -> MATERIAL
    # KIND = 3 -> SYS_ROOF        -> SYS_ROOF_MATERIAL        -> MATERIAL
    # KIND = 4 -> SYS_GROUNDFLOOR -> SYS_GROUNDFLOOR_MATERIAL -> MATERIAL
    # KIND = 5 -> SYS_MIDDLEFLOOR -> SYS_MIDDLEFLOOR_MATERIAL -> MATERIAL
    # KIND = 6 -> SYS_AIRFLOOR    -> SYS_AIRFLOOR_MATERIAL    -> MATERIAL
    # TODO: translate Chinese names?
    const <- DBI::dbGetQuery(dest,
        "
        WITH SYS_CONST AS (
            SELECT STRUCT_ID, CNAME, 1 AS KIND
            FROM SYS_OUTWALL
            UNION
            SELECT STRUCT_ID, CNAME, 2 AS KIND
            FROM SYS_INWALL
            UNION
            SELECT STRUCT_ID, CNAME, 3 AS KIND
            FROM SYS_ROOF
            UNION
            SELECT STRUCT_ID, CNAME, 4 AS KIND
            FROM SYS_GROUNDFLOOR
            UNION
            SELECT STRUCT_ID, CNAME, 5 AS KIND
            FROM SYS_MIDDLEFLOOR
            UNION
            SELECT STRUCT_ID, CNAME, 6 AS KIND
            FROM SYS_AIRFLOOR
        ),
        SYS_CONST_MATERIAL AS (
            SELECT STRUCT_ID, 1 AS KIND, MATERIAL_ID, LAYER_NO, LENGTH
            FROM SYS_OUTWALL_MATERIAL
            UNION
            SELECT STRUCT_ID, 2 AS KIND, MATERIAL_ID, LAYER_NO, LENGTH
            FROM SYS_INWALL_MATERIAL
            UNION
            SELECT STRUCT_ID, 3 AS KIND, MATERIAL_ID, LAYER_NO, LENGTH
            FROM SYS_ROOF_MATERIAL
            UNION
            SELECT STRUCT_ID, 4 AS KIND, MATERIAL_ID, LAYER_NO, LENGTH
            FROM SYS_GROUNDFLOOR_MATERIAL
            UNION
            SELECT STRUCT_ID, 5 AS KIND, MATERIAL_ID, LAYER_NO, LENGTH
            FROM SYS_MIDDLEFLOOR_MATERIAL
            UNION
            SELECT STRUCT_ID, 6 AS KIND, MATERIAL_ID, LAYER_NO, LENGTH
            FROM SYS_AIRFLOOR_MATERIAL
        ),
        CONST AS (
            SELECT DISTINCT CONSTRUCTION, KIND FROM MAIN_ENCLOSURE WHERE CONSTRUCTION != 0
            UNION
            SELECT DISTINCT D.LONG AS CONSTRUCTION, E.KIND
            FROM MAIN_ENCLOSURE E
            LEFT JOIN DEFAULT_SETTING D
            -- handle default construction
            ON E.CONSTRUCTION = 0 AND E.KIND = D.KIND AND
            D.TABLE_NAME = 'MAIN_ENCLOSURE' AND D.FIELD_NAME = 'CONSTRUCTION' AND D.TYPE = 2
            WHERE D.LONG IS NOT NULL
        )
        SELECT
            C.STRUCT_ID     AS ID,
            C.CNAME         AS NAME,
            CONST.KIND      AS KIND,
            CM.LAYER_NO     AS LAYER_NO,
            CM.LENGTH       AS LENGTH,
            CM.MATERIAL_ID  AS MATERIAL_ID,
            M.CNAME         AS MATERIAL_NAME,
            M.CONDUCTIVITY  AS MATERIAL_CONDUCTIVITY,
            M.DENSITY       AS MATERIAL_DENSITY,
            M.SPECIFIC_HEAT AS MATERIAL_SPECIFIC_HEAT
        FROM CONST
        LEFT JOIN SYS_CONST C
        ON CONST.CONSTRUCTION = C.STRUCT_ID AND CONST.KIND = C.KIND
        LEFT JOIN SYS_CONST_MATERIAL CM
        ON C.STRUCT_ID = CM.STRUCT_ID AND C.KIND = CM.KIND
        LEFT JOIN SYS_MATERIAL M
        ON CM.MATERIAL_ID = M.MATERIAL_ID
        "
    )

    # WINDOW -> SYS_WINDOW -> SYS_WINDOW_MATERIAL -> SYS_APP_MATERIAL
    # TODO: handle 'TYPE' and 'SHADING' in 'WINDOW' table
    window <- DBI::dbGetQuery(dest,
        "
        WITH WIN AS (
            SELECT DISTINCT WINDOW_CONSTRUCTION FROM WINDOW WHERE WINDOW_CONSTRUCTION != 0
            UNION
            SELECT D.LONG AS WINDOW_CONSTRUCTION
            FROM WINDOW W
            LEFT JOIN DEFAULT_SETTING D
            ON W.WINDOW_CONSTRUCTION = 0 AND
               D.TABLE_NAME = 'WINDOW' AND D.FIELD_NAME = 'WINDOW_CONSTRUCTION' AND D.TYPE = 2
            WHERE D.LONG IS NOT NULL
        )
        SELECT
            S.WINDOW_ID           AS ID,
            S.CNAME               AS NAME,
            -- use KIND = -1 to indicate that the construction is a window
            -1                    AS KIND,
            SM.LAYER_NO           AS LAYER_NO,
            SM.LENGTH             AS LENGTH,
            SM.MATERIAL_ID        AS MATERIAL_ID,
            M.CNAME               AS MATERIAL_NAME,
            M.CONDUCTIVITY        AS MATERIAL_CONDUCTIVITY,
            M.DENSITY             AS MATERIAL_DENSITY,
            M.SPECIFIC_HEAT       AS MATERIAL_SPECIFIC_HEAT
        FROM WIN W
        LEFT JOIN SYS_WINDOW S
        ON W.WINDOW_CONSTRUCTION = S.WINDOW_ID
        LEFT JOIN SYS_WINDOW_MATERIAL SM
        ON S.WINDOW_ID = SM.WINDOW_ID
        LEFT JOIN SYS_APP_MATERIAL M
        ON SM.MATERIAL_ID = M.APP_MATERIAL_ID
        "
    )

    # DOOR -> SYS_DOOR -> SYS_MATERIAL
    # TODO: GlazedDoor or Door?
    # NOTE: Doors only have a single layer in DeST. There are 'APP_ID' and
    # 'APP_FLAG' values to indicate if there are glazings in the door. Here we
    # form the data in the same format as normal construction: mark the normal
    # layer as 0 and the glaze layer as 1.
    door <- DBI::dbGetQuery(dest,
        "
        WITH DR AS (
            SELECT DISTINCT DOOR_CONSTRUCTION, MIN(OF_ENCLOSURE) AS OF_ENCLOSURE
            FROM DOOR
            WHERE DOOR_CONSTRUCTION != 0
            UNION
            SELECT D.LONG AS DOOR_CONSTRUCTION, DR.OF_ENCLOSURE
            FROM DOOR DR
            LEFT JOIN DEFAULT_SETTING D
            ON DR.DOOR_CONSTRUCTION = 0 AND
               D.TABLE_NAME = 'DOOR' AND D.FIELD_NAME = 'DOOR_CONSTRUCTION' AND D.TYPE = 2
            WHERE D.LONG IS NOT NULL
        )
        SELECT * FROM (
            SELECT
                S.DOOR_ID             AS ID,
                S.CNAME               AS NAME,
                -- use KIND = -2 to indicate that the construction is a door
                -2                    AS KIND,
                -- mark the normal layer as 0
                0                     AS LAYER_NO,
                L.LENGTH              AS LENGTH,
                S.MATERIAL_ID         AS MATERIAL_ID,
                M.CNAME               AS MATERIAL_NAME,
                M.CONDUCTIVITY        AS MATERIAL_CONDUCTIVITY,
                M.DENSITY             AS MATERIAL_DENSITY,
                M.SPECIFIC_HEAT       AS MATERIAL_SPECIFIC_HEAT
            FROM DR D
            LEFT JOIN SYS_DOOR S
            ON D.DOOR_CONSTRUCTION = S.DOOR_ID
            LEFT JOIN SYS_MATERIAL M
            ON S.MATERIAL_ID = M.MATERIAL_ID
            LEFT JOIN MAIN_ENCLOSURE E
            ON D.OF_ENCLOSURE = E.ID
            LEFT JOIN (
                SELECT STRUCT_ID, KIND, SUM(LENGTH) AS LENGTH
                FROM (
                    SELECT STRUCT_ID, 1 AS KIND, LENGTH
                    FROM SYS_OUTWALL_MATERIAL
                    UNION
                    SELECT STRUCT_ID, 2 AS KIND, LENGTH
                    FROM SYS_INWALL_MATERIAL
                    UNION
                    SELECT STRUCT_ID, 3 AS KIND, LENGTH
                    FROM SYS_ROOF_MATERIAL
                    UNION
                    SELECT STRUCT_ID, 4 AS KIND, LENGTH
                    FROM SYS_GROUNDFLOOR_MATERIAL
                    UNION
                    SELECT STRUCT_ID, 5 AS KIND, LENGTH
                    FROM SYS_MIDDLEFLOOR_MATERIAL
                    UNION
                    SELECT STRUCT_ID, 6 AS KIND, LENGTH
                    FROM SYS_AIRFLOOR_MATERIAL
                )
                GROUP BY STRUCT_ID, KIND
            ) L
            ON E.CONSTRUCTION = L.STRUCT_ID AND E.KIND = L.KIND
        ) WHERE ID IS NOT NULL -- in case there are no doors
        UNION
        SELECT * FROM (
            SELECT
                S.DOOR_ID             AS ID,
                S.CNAME               AS NAME,
                -- use KIND = -2 to indicate that the construction is a door
                -2                    AS KIND,
                -- mark the glaze layer as 1
                1                     AS LAYER_NO,
                M.THICK               AS LENGTH,
                S.APP_ID              AS MATERIAL_ID,
                M.CNAME               AS MATERIAL_NAME,
                M.CONDUCTIVITY        AS MATERIAL_CONDUCTIVITY,
                M.DENSITY             AS MATERIAL_DENSITY,
                M.SPECIFIC_HEAT       AS MATERIAL_SPECIFIC_HEAT
            FROM DOOR D
            LEFT JOIN SYS_DOOR S
            ON D.DOOR_CONSTRUCTION = S.DOOR_ID AND S.APP_ID != 0 AND S.APP_FLAG = 1
            LEFT JOIN SYS_APP_MATERIAL M
            ON S.MATERIAL_ID = M.APP_MATERIAL_ID
        ) WHERE ID IS NOT NULL -- in case there are no glaze layers
        "
    )

    assert_unique_name(const$NAME[const$LAYER_NO == 0L], "construction")
    assert_unique_name(window$NAME[window$LAYER_NO == 0L], "window")
    assert_unique_name(door$NAME[door$LAYER_NO == 0L], "door")

    data.table::setDT(const)
    data.table::setDT(window)
    data.table::setDT(door)

    # check if there are air layer in window constructions
    if (any(is_air <- window$MATERIAL_ID == 0L)) {
        data.table::set(window, which(is_air), "MATERIAL_NAME", "Air")
    }

    # NOTE: Here we append thickness to material names. This is because the
    # same material can be used with different thicknesses. But in EnergyPlus,
    # each material binds to a specific thickness. During conversion, we have to
    # create a new material with each thickness. Appending the thickness to the
    # material name should make them unique, since duplicated names have been
    # handled by 'destep_update_name()'
    if (nrow(const) > 0L) {
        data.table::set(const, NULL, "MATERIAL_NAME",
            with(const, paste0(MATERIAL_NAME, " ", round(LENGTH), "mm"))
        )
    }
    if (nrow(window) > 0L) {
        data.table::set(window, NULL, "MATERIAL_NAME",
            with(window, paste0(MATERIAL_NAME, " ", round(LENGTH), "mm"))
        )
    }
    # only rename glaze layers for doors
    if (nrow(door) > 0L) {
        # door should have the same thickness as the parent wall
        if (nrow(const) > 0L) {

        }
        any(door$LAYER_NO == 1L)
        data.table::set(door, NULL, "MATERIAL_NAME",
            with(door, paste0(MATERIAL_NAME, " ", round(LENGTH), "mm"))
        )
    }

    # normal construction
    norm_const <- list()
    norm_mat <- list()

    # window construction
    win_const <- list()
    win_glaze <- list()
    win_air <- list()

    # door construction
    door_const <- list()
    door_glaze <- list()
    door_mat <- list()

    # columns to be used in the material input
    col_mat <- c(
        "MATERIAL_ID", "LENGTH", "MATERIAL_NAME", "MATERIAL_CONDUCTIVITY",
        "MATERIAL_DENSITY", "MATERIAL_SPECIFIC_HEAT"
    )
    if (nrow(const) > 0L) {
        norm_const <- const[,
            by = c("ID", "KIND"),
            list(name = NAME[[1L]], value = list(c(NAME[[1L]], MATERIAL_NAME)))
        ]

        # construct normal Material input
        norm_mat <- unique(const[, .SD, .SDcols = col_mat], by = c("MATERIAL_ID", "LENGTH"))
    }

    if (nrow(window) > 0L) {
        win_const <- window[, by = "ID",
            # use KIND = -1 to indicate that the construction is a window
            list(KIND = -1L, name = NAME[[1L]], value = list(c(NAME[[1L]], MATERIAL_NAME)))
        ]

        win_glaze <- unique(
            window[MATERIAL_ID != 0L, .SD, .SDcols = col_mat],
            by = c("MATERIAL_ID", "LENGTH")
        )

        win_air <- unique(
            window[MATERIAL_ID == 0L, .SD, .SDcols = col_mat],
            by = c("MATERIAL_ID", "LENGTH")
        )
    }

    if (nrow(door) > 0L) {
        # construct normal Material input
        door_const <- door[, by = "ID",
            # use KIND = -2 to indicate that the construction is a door
            list(KIND = -2L, name = NAME[[1L]], value = list(c(NAME[[1L]], MATERIAL_NAME)))
        ]

        door_mat <- unique(
            door[LAYER_NO == 0L, .SD, .SDcols = col_mat],
            by = c("MATERIAL_ID", "LENGTH")
        )

        door_glaze <- unique(
            door[LAYER_NO == 1L, .SD, .SDcols = col_mat],
            by = c("MATERIAL_ID", "LENGTH")
        )
    }

    dt_const <- unique(
        data.table::rbindlist(list(norm_const, win_const, door_const), TRUE),
        by = c("ID", "KIND", "name")
    )
    dt_mat   <- unique(
        data.table::rbindlist(list(norm_mat, door_mat), TRUE),
        by = c("MATERIAL_ID", "LENGTH")
    )
    dt_glaze <- unique(
        data.table::rbindlist(list(win_glaze, door_glaze), TRUE),
        by = c("MATERIAL_ID", "LENGTH")
    )
    dt_air   <- unique(win_air, by = "LENGTH")

    out <- eval(as.call(c(
        destep_add, dest, ep,

        # Material
        bquote(
            "Material" := list(
                name                = .(dt_mat$MATERIAL_NAME),
                # NOTE: here we use "MediumSmooth" for roughness"
                roughness           = "MediumSmooth",
                thickness           = .(dt_mat$LENGTH),
                conductivity        = .(dt_mat$MATERIAL_CONDUCTIVITY),
                density             = .(dt_mat$MATERIAL_DENSITY),
                specific_heat       = .(dt_mat$MATERIAL_SPECIFIC_HEAT),
                # use EnergyPlus defaults for the rest
                thermal_absorptance = 0.9,
                solar_absorptance   = 0.7,
                visible_absorptance = 0.7
            )
        ),

        if (nrow(dt_glaze) > 0L) {
            # NOTE: It is not an one-to-one match between the glazing optical
            # properties in DeST and EnergyPlus.
            #
            # We can found 2.5mm, 3mm, 6mm and 12mm clear glazing in the dataset
            # 'WindowGlassMaterials.idf' distributed from EnergyPlus
            #
            # However, the entries in 'SYS_APP_MATERIAL' in DeST have thickness of
            # 3mm, 5mm and 18mm.
            #
            # An approach is to use the LBNL Window program to extract the optical
            # properties of the glazing. But haven't try this yet.
            #
            # Here we will use the optical properties of a 3mm clear glazing for all
            # glazing in DeST and issue a message
            clear3mm <- list(
                Name = "CLEAR 3MM", Conductivity = 0.9, Thickness = 0.003,
                `Optical Data Type` = "SpectralAverage",
                `Solar Transmittance at Normal Incidence` = 0.837,
                `Front Side Solar Reflectance at Normal Incidence` = 0.075,
                `Back Side Solar Reflectance at Normal Incidence` = 0.075,
                `Visible Transmittance at Normal Incidence` = 0.898,
                `Front Side Visible Reflectance at Normal Incidence` = 0.081,
                `Back Side Visible Reflectance at Normal Incidence` = 0.081,
                `Infrared Transmittance at Normal Incidence` = 0,
                `Front Side Infrared Hemispherical Emissivity` = 0.84,
                `Back Side Infrared Hemispherical Emissivity` = 0.84
            )

            message(paste(
                "There are windows in the input DeST model. However, the optical",
                "properties of the glazing in DeST are not one-to-one match with",
                "the glazing in EnergyPlus. Here the optical properties of a 3mm",
                "clear glazing in EnergyPlus dataset 'WindowGlassMaterials.idf'",
                "will be used for all glazing in DeST. Please check the results in",
                "the converted IDF file."
            ))

            glaze              <- clear3mm
            glaze$Name         <- dt_glaze$MATERIAL_NAME
            glaze$Thickness    <- round(dt_glaze$LENGTH / 1000, 4L)
            glaze$Conductivity <- dt_glaze$MATERIAL_CONDUCTIVITY
            bquote("WindowMaterial:Glazing" := .(glaze))
        },

        if (nrow(dt_air) > 0L) {
            bquote("WindowMaterial:Gas" := list(
                name = .(dt_air$MATERIAL_NAME),
                gas_type = "Air",
                thickness = round(.(dt_air$LENGTH) / 1000, 4L)
            ))
        },

        # Construction
        lapply(dt_const$value, function(con) bquote("Construction" := as.list(.(con))))
    )))

    # always attach the table to the output in case it is useful later
    attr(out, "table") <- data.table::rbindlist(list(const, window, door), fill = TRUE)

    out
}

# SURFACE|MAIN_ENCLOSURE|PLANE -> BuildingSurface:Detailed
destep_conv_surface <- function(dest, ep) {
    # NOTE: In DeST, the main enclosure table is used to store the relationship
    # between surfaces and the rooms they belong to. Different from EnergyPlus,
    # 'adjacent' surfaces in DeST have different locations. The distance between
    # the outer surface and the inner surface is the thickness of the
    # construction. Here, the 'MIDDLE_PLANE' column in 'MAIN_ENCLOSURE' table
    # is used to get the vertices of adjacent surfaces
    surface <- DBI::dbGetQuery(dest,
        "
        SELECT
            E.SURFACE_ID                                   AS ID,
            E.PLANE                                        AS PLANE,
            S.NAME                                         AS NAME,
            E.KIND                                         AS KIND_ENCLOSURE,
            S.TYPE                                         AS TYPE_SURFACE,
            CASE
                WHEN E.KIND = 1 OR E.KIND = 2 THEN 'Wall'
                WHEN E.KIND = 3 OR E.KIND = 6 THEN 'Roof'
                WHEN E.KIND = 4 THEN 'Floor'
                WHEN E.KIND = 5 THEN 'Ceiling'
            END                                            AS TYPE,
            E.SIDE                                         AS SIDE,
            E.CONSTRUCTION                                 AS CONSTRUCTION,
            COALESCE(ROOM.NAME, OUTSIDE.NAME, GROUND.NAME) AS ROOM,
            CASE
                WHEN E.KIND = 1 OR S.TYPE = 1 THEN 'Outdoors'
                WHEN E.KIND = 4 OR S.TYPE = 2 THEN 'Ground'
                ELSE 'Surface'
            END                                            AS BOUNDARY,
            L.POINT_NO                                     AS POINT_NO,
            ROUND(P.X, 3)                                  AS POINT_X,
            ROUND(P.Y, 3)                                  AS POINT_Y,
            ROUND(P.Z, 3)                                  AS POINT_Z
        FROM (
            SELECT
                SURFACE_ID, S.KIND, SIDE, C.CNAME AS CONSTRUCTION, PLANE
            FROM (
                -- get both side surfaces
                SELECT SIDE1 AS SURFACE_ID, KIND, 1 AS SIDE, CONSTRUCTION, MIDDLE_PLANE AS PLANE FROM MAIN_ENCLOSURE
                UNION
                SELECT SIDE2 AS SURFACE_ID, KIND, 2 AS SIDE, CONSTRUCTION, MIDDLE_PLANE AS PLANE FROM MAIN_ENCLOSURE
            ) S
            -- get construction name
            LEFT JOIN (
                SELECT STRUCT_ID, CNAME, 1 AS KIND
                FROM SYS_OUTWALL
                UNION
                SELECT STRUCT_ID, CNAME, 2 AS KIND
                FROM SYS_INWALL
                UNION
                SELECT STRUCT_ID, CNAME, 3 AS KIND
                FROM SYS_ROOF
                UNION
                SELECT STRUCT_ID, CNAME, 4 AS KIND
                FROM SYS_GROUNDFLOOR
                UNION
                SELECT STRUCT_ID, CNAME, 5 AS KIND
                FROM SYS_MIDDLEFLOOR
                UNION
                SELECT STRUCT_ID, CNAME, 6 AS KIND
                FROM SYS_AIRFLOOR
            ) C
            ON S.CONSTRUCTION = C.STRUCT_ID AND S.KIND = C.KIND
        ) E
        LEFT JOIN SURFACE S
        ON E.SURFACE_ID = S.SURFACE_ID
        -- get room name
        LEFT JOIN ROOM
        ON S.OF_ROOM = ROOM.ID
        LEFT JOIN OUTSIDE
        ON S.TYPE = 1 AND S.OF_ROOM = OUTSIDE.OUTSIDE_ID
        LEFT JOIN GROUND
        ON S.TYPE = 2 AND S.OF_ROOM = GROUND.GROUND_ID
        LEFT JOIN PLANE P
        ON E.PLANE = P.PLANE_ID
        LEFT JOIN GEOMETRY G
        ON P.GEOMETRY = G.GEOMETRY_ID
        LEFT JOIN LOOP_POINT L
        ON G.BOUNDARY_LOOP_ID = L.LOOP_ID
        LEFT JOIN POINT P
        ON L.POINT = P.POINT_ID
        "
    )
    assert_unique_name(surface$NAME[surface$POINT_NO == 0L], "surface")
    data.table::setDT(surface)

    # find the adjacent surfaces
    surface[BOUNDARY == "Surface", by = "PLANE", BOUNDARY_OBJECT := rev(NAME)]
    # TODO: reverse the order of the vertices for the adjacent surface
    # If 'MAIN_ENCLOSURE$KIND' = 5, it means that this enclosure can either be a
    # ceiling or floor. The actual surface type is determined by the
    # 'SURFACE$TYPE'. If 'SURFACE$TYPE' = 4, this is a floor. The coordinates
    # should be reversed. If 'SURFACE$TYPE' = 5, this is a ceiling.
    surface[KIND_ENCLOSURE == 5L & TYPE_SURFACE == 4L, by = "ID", `:=`(
        TYPE = "Floor",
        POINT_X = rev(POINT_X), POINT_Y = rev(POINT_Y), POINT_Z = rev(POINT_Z)
    )]
    surface[KIND_ENCLOSURE == 5L & TYPE_SURFACE == 5L, TYPE := "Ceiling"]

    # remove the surface indicating outside environment and grounds
    surface <- surface[!J(c(1L, 2L)), on = "TYPE_SURFACE"]

    # TODO: how does DeST handle the case when the surface is both a floor and a ceiling?
    # TODO: how does EnergyPlus handle "empty floor slab"?

    value <- surface[, by = "ID",
        list(value = list(c(
            list(
                # 01: Name
                name = NAME[[1L]],
                # 02: Surface Type
                surface_type = TYPE[[1L]],
                # 03: Construction Name
                construction_name = CONSTRUCTION[[1L]],
                # 04: Zone Name
                zone_name = ROOM[[1L]],
                # 05: Space Name - Space was introduced in EnergyPlus v9.6
                if (ep$version() > "9.5") space_name = NULL,
                # 06: Outside Boundary Condition
                outside_boundary_condition = BOUNDARY[[1L]],
                # 07: Outside Boundary Condition Object
                outside_boundary_condition_object = if (!is.na(BOUNDARY_OBJECT[[1L]])) BOUNDARY_OBJECT[[1L]],
                # 08: Sun Exposure
                sun_exposure = NULL,
                # 09: Wind Exposure
                wind_exposure = NULL,
                # 10: View Factor to Ground
                view_factor_to_ground = "Autocalculate",
                # 11: Number of Vertices
                number_of_vertices = max(POINT_NO) + 1L
            ),
            # Vertices
            as.list(as.double(vapply(POINT_NO + 1L, FUN.VALUE = double(3),
                function(ind) {
                    c(POINT_X[ind], POINT_Y[ind], POINT_Z[ind])
                }
            )))
        )
    ))]$value

    out <- eval(as.call(c(
        destep_add, dest, ep,
        lapply(value, function(val) bquote("BuildingSurface:Detailed" := .(val)))
    )))

    # always attach the table to the output in case it is useful later
    attr(out, "table") <- surface

    out
}

# TODO: handle window shading
# TODO: handle window type
# WINDOW -> FenestrationSurface:Detailed
destep_conv_window <- function(dest, ep) {
}

# SCHEDULE_YEAR -> Schedule:Year -> Schedule:Week:Compact -> Schedule:Day:Interval -> ScheduleTypeLimits
destep_conv_schedule <- function(dest, ep) {
    # currently, schedules are used in the tables below:
    # - DOOR
    # - ENERGY_DEVICE
    # - ENERGY_HOTWATER
    # - ENERGY_LIFT_ESCALATOR
    # - ENERGY_PUMP_FAN
    # - ENERGY_PUMP_GROUP
    # - EQUIPMENT_GAINS
    # - HEATING_PIPE
    # - HEATING_SYSTEM
    # - LIGHT_GAINS
    # - OCCUPANT_GAINS
    # - ROOM
    # - ROOM_GROUP
    # - ROOM_TYPE_DATA
    # - WINDOW

    # get all schedules that are used in the model
    tbls <- DBI::dbListTables(dest)

    # find tables that reference schedules
    ids_ref <- unique(un_list(recursive = TRUE, lapply(tbls[tbls != "SCHEDULE_YEAR"], function(tbl) {
        # get the number of rows in the table
        n <- DBI::dbGetQuery(dest, sprintf("SELECT COUNT(*) as n FROM '%s'", tbl))$n
        if (n > 0L) {
            # get the column names that reference schedules
            col_ref <- grep("SCHEDULE", DBI::dbListFields(dest, tbl), value = TRUE)
            if (length(col_ref) > 0L) {
                # get the distinct values of the referenced schedules
                DBI::dbGetQuery(dest, sprintf("SELECT DISTINCT %s FROM '%s'", paste(col_ref, collapse = ", "), tbl))
            }
        }
    })))
    # TODO: what is the value of the default schedule with ID = 0 for WINDOW table?
    ids_ref <- ids_ref[ids_ref != 0L]

    schedule <- data.table::setDT(DBI::dbGetQuery(
        dest,
        sprintf("SELECT * FROM SCHEDULE_YEAR WHERE SCHEDULE_ID IN (%s)", paste(ids_ref, collapse = ", "))
    ))
    # In DeST, the actual schedule data is stored as doubles in a raw vector
    data.table::set(schedule, NULL, "DATA", lapply(schedule[["DATA"]], readBin, what = "double", n = 8760L))

    type_limits <- destep_conv_schedule_type_limits(dest, ep, schedule)
    days <- destep_conv_schedule_day(dest, ep, schedule, type_limits)
    weeks <- destep_conv_schedule_week(dest, ep, schedule, type_limits, days)
    years <- destep_conv_schedule_year(dest, ep, schedule, type_limits, weeks)

    # combine all data and load
    data.table::set(type_limits, NULL, "id", data.table::rleid(type_limits$id))
    data.table::set(days$data, NULL, "id", data.table::rleid(days$data$id) + type_limits$id[nrow(type_limits)])
    data.table::set(weeks$data, NULL, "id", data.table::rleid(weeks$data$id) + days$data$id[nrow(days$data)])
    data.table::set(years, NULL, "id", data.table::rleid(years$id) + weeks$data$id[nrow(weeks$data)])
    out <- destep_load(
        dest, ep,
        data.table::rbindlist(list(type_limits, days$data, weeks$data, years), use.names = TRUE)
    )

    # always attach the table to the output in case it is useful later
    attr(out, "table") <- schedule

    out
}

destep_conv_schedule_type_limits <- function(dest, ep, schedule) {
    types <- schedule$TYPE
    types[types == 5L] <- 4L
    types <- unique.default(types)
    type_limits <- data.table::fcase(
        types == 1L,
        list(list("Fraction", 0, 1, "Continuous")),
        types == 2L,
        list(list("On/Off", 0, 1, "Discrete")),
        types == 3L,
        list(list("Control Method", NA_real_, NA_real_, "Discrete")),
        types %in% c(4L, 5L),
        list(list("Any Number", NA_real_, NA_real_, "Continuous"))
    )
    type_limits <- data.table::rbindlist(type_limits)
    data.table::setnames(type_limits, c("Name", "Lower Limit Value", "Upper Limit Value", "Numeric Type"))
    data.table::set(type_limits, NULL, "Unit Type", "Dimensionless")
    data.table::set(type_limits, NULL, "id", types)
    data.table::set(type_limits, NULL, "class", "ScheduleTypeLimits")
    data.table::set(type_limits, NULL, "name", type_limits$Name)

    type_limits <- data.table::set(eplusr::dt_to_load(type_limits), NULL, "field", NULL)
    data.table::setcolorder(type_limits, c("id", "class", "name", "value"))

    type_limits
}

destep_conv_schedule_day <- function(dest, ep, schedule, type_limits, prefix = "Day-") {
    # the number of schedules to extract
    num_sch <- nrow(schedule)

    # combine all yearly schedule data into a single vector
    full_value <- data.table::data.table(
        rleid = rep(1L:(365L * num_sch), each = 24L),
        type = rep(schedule$TYPE, each = 365L * 24L),
        until = rep((1L:24L) * 60L, 365L * num_sch),
        value = un_list(schedule$DATA)
    )
    day_value <- unique_value(full_value)

    map_week <- data.table::setDT(attr(day_value, "map"))

    # compress the value
    grp <- collapse::groupv(day_value$rleid)
    changed <- collapse::fdiff.default(day_value$value, g = grp, fill = 1.0) != 0.0
    changed <- collapse::flag.default(changed, n = -1L, g = grp)
    collapse::replace_na(changed, TRUE, set = TRUE)
    day_value <- collapse::ss(day_value, changed)

    grp_day <- collapse::groupv(map_week$index_cur, starts = TRUE)

    # make unique names for each day schedule
    day_name <- rep(schedule$NAME, 365L * 24L)[
        map_week$index_ori[attr(grp_day, "starts", exact = TRUE)]
    ]
    day_name <- paste0(prefix, make_unique_name(day_name))

    type_day <- collapse::funique.data.frame(collapse::ss(day_value, j = c("rleid", "type")))
    data.table::setnames(type_day, "rleid", "id")

    grp_fld <- collapse::groupv(day_value$rleid, group.size = TRUE)
    num_val <- attr(grp_fld, "group.sizes", exact = TRUE)
    # *2 for time and value pairs
    # +3 for name, type limits, interpolate
    num_fld <- num_val * 2L + 3L

    sch_day <- destep_field(dest, ep, "Schedule:Day:Interval", num_fld)
    data.table::set(sch_day, NULL, "name", rep(day_name, num_fld))

    # field 1: name
    data.table::set(sch_day, collapse::whichv(sch_day$index, 1L), "value", day_name)

    # field 2: schedule type limits name
    data.table::set(
        sch_day, collapse::whichv(sch_day$index, 2L), "value",
        type_limits$name[collapse::fmatch(type_day$type, type_limits$id)]
    )

    # field 3: interpolate to timestep
    data.table::set(sch_day, collapse::whichv(sch_day$index, 3L), "value", "No")

    # field-sets
    data.table::set(
        sch_day, which(sch_day$index > 3L), "value",
        c(format_time(day_value$until), as.character(day_value$value))[
            collapse::radixorderv(rep(seq_along(day_value$until), 2L))
        ]
    )

    list(map = map_week, data = sch_day)
}

destep_conv_schedule_week <- function(dest, ep, schedule, type_limits, days, prefix = "Week-") {
    num_sch <- nrow(schedule)
    map <- data.table::copy(days$map)

    data.table::set(map, NULL, "rleid", rep(seq_len(num_sch), each = 365L))
    data.table::set(map, NULL, "week", rep(c(rep(1L:52L, each = 7L), 53L), num_sch))
    data.table::set(map, NULL, "rleid_week", rep((seq_len(num_sch) - 1L) * 53L, each = 365L) + map$week)

    # find the first monday that is the same as the one in the 53rd week
    ind_53 <- collapse::whichv(map$week, 53L)
    mth_53 <- collapse::fmatch(
        list(rleid = map$rleid[ind_53], index_cur = map$index_cur[ind_53]),
        list(rleid = map$rleid[-ind_53], index_cur = map$index_cur[-ind_53]),
        nomatch = 0L
    )

    # handle the case that there is no match for the 53rd week
    # in this case, we have to create a didicated week schedule for the 53rd
    # week
    if (any(mth_53 == 0L)) {
        stop("Not implemented yet")
    }

    week_53 <- collapse::ss(map, ind_53, c("index_ori", "rleid", "week", "rleid_week"))
    data.table::set(week_53, NULL, "week", map$week[-ind_53][mth_53])
    map <- data.table::rbindlist(list(
        collapse::ss(map, -ind_53),
        data.table::set(collapse::join(
            collapse::ss(map, j = c("index_cur", "rleid", "week"), check = FALSE),
            week_53,
            on = c("rleid", "week"), how = "right",
            verbose = FALSE, multiple = TRUE
        ), NULL, "week", 53L)
    ), use.names = TRUE)

    spl_week <- collapse::gsplit(map$index_cur, map$rleid_week)
    grp_week <- collapse::group(data.table::transpose(spl_week))
    grp_rleid <- collapse::groupv(rep(seq_len(num_sch), each = 53L))
    changed <- collapse::fdiff.default(grp_week, g = grp_rleid, fill = 1.0) != 0.0
    changed <- collapse::flag.default(changed, n = -1L, g = grp_rleid)
    collapse::replace_na(changed, TRUE, set = TRUE)

    week_name <- paste0(prefix, make_unique_name(rep(schedule$NAME, each = 53L)[changed]))

    # create the day types for each week schedule
    week_daytype <- collapse::ss(
        map,
        collapse::fmatch(map$rleid_week, which(changed), nomatch = 0L) != 0L,
        c("rleid_week", "index_cur"),
        check = FALSE
    )
    data.table::set(week_daytype, NULL, "daytype", rep(c(2L:7L, 1L), sum(changed)))
    data.table::setnames(week_daytype, "index_cur", "rleid_day")
    # NOTE: In DeST, there is no "SummerDesignDay", "WinterDesignDay",
    #       "Holiday", "CustomDay1" and "CustomDay2". So for "SummerDesignDay"
    #       and "WinterDesignDay", we use the value for "Monday". For
    #       "Holiday", "CustomDay1" and "CustomDay2", we use the value for
    #       "Sunday".
    # extract the value for "Monday"
    week_daytype_mon <- collapse::ss(
        week_daytype,
        collapse::whichv(week_daytype$daytype, ENUM_SCH_DAYTYPE[["Monday"]]),
        check = FALSE
    )
    week_daytype_designday <- data.table::rbindlist(list(
        data.table::set(data.table::copy(week_daytype_mon), NULL, "daytype", ENUM_SCH_DAYTYPE[["SummerDesignDay"]]),
        data.table::set(week_daytype_mon, NULL, "daytype", ENUM_SCH_DAYTYPE[["WinterDesignDay"]])
    ))
    # extract the value for "Sunday"
    week_daytype_sun <- collapse::ss(
        week_daytype,
        collapse::whichv(week_daytype$daytype, ENUM_SCH_DAYTYPE[["Sunday"]]),
        check = FALSE
    )
    week_daytype_specialday <- data.table::rbindlist(list(
        data.table::set(data.table::copy(week_daytype_sun), NULL, "daytype", ENUM_SCH_DAYTYPE[["Holiday"]]),
        data.table::set(data.table::copy(week_daytype_sun), NULL, "daytype", ENUM_SCH_DAYTYPE[["CustomDay1"]]),
        data.table::set(week_daytype_sun, NULL, "daytype", ENUM_SCH_DAYTYPE[["CustomDay2"]])
    ))
    # combine all
    week_daytype <- data.table::rbindlist(list(
        week_daytype,
        week_daytype_designday,
        week_daytype_specialday
    ))
    data.table::setorderv(week_daytype, c("rleid_week", "daytype"))
    data.table::setnames(week_daytype, "rleid_week", "rleid")

    # compress the day types
    other_days <- ENUM_SCH_DAYTYPE[c("Holiday", "CustomDay1", "CustomDay2")]
    grp_rleid <- collapse::groupv(week_daytype$rleid, starts = TRUE)

    pairs <- .mapply(
        function(rleid, daytypes, days) {
            # group by day schedules
            grp_days <- collapse::groupv(days, starts = TRUE, group.sizes = TRUE)
            rleid_day <- days[attr(grp_days, "starts", exact = TRUE)]
            len_day <- length(rleid_day)

            # if all day schedules are the same, return "AllDay"
            if (attr(grp_days, "N.groups", exact = TRUE) == 1L) {
                return(list(
                    rleid = rep(rleid, len_day),
                    daytype = rep(ENUM_SCH_DAYTYPE[["AllDay"]], len_day),
                    rleid_day = rleid_day
                ))
            }

            compacted <- lapply(
                collapse::gsplit(daytypes, grp_days),
                function(daytypes) {
                    out <- integer(0L)

                    m_weekday <- collapse::fmatch(ENUM_SCH_DAYTYPE_WEEKDAY, daytypes, 0L)
                    if (sum(m_weekday != 0L) == length(ENUM_SCH_DAYTYPE_WEEKDAY)) {
                        out <- c(ENUM_SCH_DAYTYPE[["Weekday"]])
                        daytypes <- daytypes[-m_weekday]
                    }

                    m_weekend <- collapse::fmatch(ENUM_SCH_DAYTYPE_WEEKEND, daytypes, 0L)
                    if (sum(m_weekend != 0L) == length(ENUM_SCH_DAYTYPE_WEEKEND)) {
                        out <- c(out, ENUM_SCH_DAYTYPE[["Weekend"]])
                        daytypes <- daytypes[-m_weekend]
                    }

                    c(out, daytypes)
                }
            )

            if (length(other_days) > 0L) {
                num_others <- vapply(compacted, function(daytypes) {
                    sum(collapse::fmatch(daytypes, other_days, 0L) != 0L)
                }, integer(1L))
                ind_others <- collapse::radixorderv(num_others, decreasing = TRUE)[num_others > 0L]
                if (length(ind_others) > 0L) {
                    ind_others <- ind_others[[1L]]
                    compacted[[ind_others]] <- c(
                        # only keep the special daytypes
                        compacted[[ind_others]][
                            collapse::fmatch(compacted[[ind_others]], ENUM_SCH_DAYTYPE_SPECIAL, 0L) != 0L
                        ],
                        ENUM_SCH_DAYTYPE[["AllOtherDay"]]
                    )
                }
            }

            len_daytype <- collapse::vlengths(compacted, use.names = FALSE)
            list(
                rleid = rep(rleid, sum(len_daytype)),
                daytype = un_list(compacted),
                rleid_day = rep(rleid_day, len_daytype)
            )
        },
        list(
            rleid = week_daytype$rleid[attr(grp_rleid, "starts", exact = TRUE)],
            daytypes = collapse::gsplit(week_daytype$daytype, grp_rleid),
            days = collapse::gsplit(week_daytype$rleid_day, grp_rleid)
        ),
        NULL
    )
    week_daytype <- data.table::rbindlist(pairs)

    grp_day <- collapse::group(
        list(week_daytype$rleid, week_daytype$rleid_day),
        group.sizes = TRUE, starts = TRUE
    )
    num_fld <- attr(collapse::groupv(
        week_daytype$rleid[attr(grp_day, "starts", exact = TRUE)],
        group.sizes = TRUE
    ), "group.sizes", exact = TRUE)
    num_fld <- num_fld * 2L + 1L

    fld_daytype <- vapply(
        collapse::gsplit(names(ENUM_SCH_DAYTYPE)[week_daytype$daytype], grp_day),
        paste,
        collapse = " ", FUN.VALUE = character(1L)
    )
    fld_daytype <- paste("For:", fld_daytype)

    fld_day <- days$name[
        collapse::fmatch(
            week_daytype$rleid_day[attr(grp_day, "starts", exact = TRUE)],
            days$id
        )
    ]

    sch_week <- destep_field(dest, ep, "Schedule:Week:Compact", num_fld)
    data.table::set(sch_week, NULL, "name", rep(week_name, num_fld))
    # keep the original rleid
    data.table::set(sch_week, NULL, "id", rep(unique(week_daytype$rleid), num_fld))

    # field 1: name
    data.table::set(sch_week, collapse::whichv(sch_week$index, 1L), "value", week_name)

    data.table::set(
        sch_week, which(sch_week$index > 1L), "value",
        c(fld_daytype, fld_day)[collapse::radixorderv(rep(seq_along(fld_daytype), 2L))]
    )

    list(map = map, changed = changed, data = sch_week)
}

destep_conv_schedule_year <- function(dest, ep, schedule, type_limits, weeks) {
    num_sch <- nrow(schedule)

    grp_rleid <- collapse::groupv(rep(seq_len(num_sch), each = 53L))

    year_span <- data.table::data.table(
        rleid = grp_rleid[weeks$changed],
        ordinal = rep(1L:53L, num_sch)[weeks$changed] * 7L,
        rleid_week = seq(1L, 53L * num_sch)[weeks$changed]
    )
    ind_371 <- collapse::whichv(year_span$ordinal, 371L)
    if (length(ind_371) > 0L) {
        data.table::set(year_span, ind_371, "ordinal", 365L)
    }

    # get the start and end date of each span
    grp_span <- collapse::groupv(year_span$rleid, starts = TRUE, group.sizes = TRUE)
    year_span_start <- collapse::flag.default(year_span$ordinal, n = 1L, g = grp_span, fill = NA_integer_) + 1L
    collapse::replace_na(year_span_start, 1L, set = TRUE)
    year_span_start <- lubridate::make_date(2025L, 12L, 31L) + lubridate::days(year_span_start)
    year_span_end <- lubridate::make_date(2025L, 12L, 31L) + lubridate::days(year_span$ordinal)
    data.table::set(
        year_span, NULL,
        c("start_month", "start_day", "end_month", "end_day"),
        list(
            as.integer(lubridate::month(year_span_start)),
            lubridate::mday(year_span_start),
            as.integer(lubridate::month(year_span_end)),
            lubridate::mday(year_span_end)
        )
    )

    # make unique names for each year schedule
    year_name <- make_unique_name(schedule$NAME)

    grp_week <- collapse::groupv(year_span$rleid, group.sizes = TRUE, starts = TRUE)
    num_fld <- attr(grp_week, "group.sizes", exact = TRUE)
    # *5 for week, start month, start day, end month, end day
    # +2 for name, type limits
    num_fld <- num_fld * 5L + 2L

    sch_year <- destep_field(dest, ep, "Schedule:Year", num_fld)
    data.table::set(sch_year, NULL, "name", rep(year_name, num_fld))

    # field 1: name
    data.table::set(sch_year, collapse::whichv(sch_year$index, 1L), "value", year_name)

    # field 2: schedule type limits name
    data.table::set(
        sch_year, collapse::whichv(sch_year$index, 2L), "value",
        type_limits$name[collapse::fmatch(schedule$TYPE, type_limits$id)]
    )

    # field-sets
    data.table::set(
        sch_year, which(sch_year$index > 2L), "value",
        c(
            weeks$data$name[collapse::fmatch(year_span$rleid_week, weeks$data$id)],
            as.character(year_span$start_month),
            as.character(year_span$start_day),
            as.character(year_span$end_month),
            as.character(year_span$end_day)
        )[collapse::radixorderv(rep(seq_along(year_span$rleid_week), 5L))]
    )

    sch_year
}

assert_unique_name <- function(names, type) {
    if (anyDuplicated(names)) {
        stop(sprintf(
            "Duplicated %s names found: [%s]. This should already be handled when updating the names.",
            type, paste(unique(names[duplicated(names)]), collapse = ", ")
        ))
    }
}

make_unique_name <- function(name) {
    spl_name <- collapse::gsplit(name, name)
    spl_name <- .mapply(
        function(name, len) if (len == 1L) name else sprintf("%s (%d)", name, seq_len(len)),
        list(name = spl_name, len = collapse::vlengths(spl_name)),
        NULL
    )
    collapse::greorder(un_list(spl_name), name)
}

unique_value <- function(value, cols = NULL, full = TRUE) {
    len <- collapse::groupv(value$rleid, starts = TRUE, group.sizes = TRUE)
    grp_len <- collapse::groupv(attr(len, "group.sizes", exact = TRUE))
    if (is.null(cols)) {
        # all columns except rleid
        col_data <- colnames(value)[-1L]
    } else {
        col_data <- cols
    }

    args <- c(
        list(index = collapse::gsplit(collapse::ss(value$rleid, attr(len, "starts")), grp_len)),
        data.table::setattr(
            lapply(col_data, function(col) {
                collapse::gsplit(collapse::gsplit(value[[col]], len), grp_len)
            }),
            "names",
            col_data
        )
    )
    paired <- .mapply(
        function(...) {
            input <- list(...)
            # transpose and combine
            trans <- lapply(input[-1L], data.table::transpose)
            pair <- un_list(trans)

            # get the group id
            grp <- collapse::groupv(pair, starts = TRUE)

            # use 'fsubset.data.frame' instead of 'funique.data.frame' since we
            # already have the group info
            pair <- collapse:::fsubset.data.frame(pair, attr(grp, "starts"))

            n_grp <- attr(grp, "N.groups", exact = TRUE)
            out <- list(
                index = input$index,
                group = grp,
                n_grp = n_grp
            )

            if (full) {
                out$rleid <- rep(seq_len(n_grp), each = length(trans[[2L]]))

                offset <- 0L
                for (col in col_data) {
                    out[[col]] <- un_list(data.table::transpose(
                        collapse::fsubset.default(pair, seq_along(trans[[col]]) + offset)
                    ))
                    offset <- offset + length(trans[[col]])
                }
            }
            out
        },
        args,
        NULL
    )

    offset <- c(
        0L,
        # accumulate the group size except the last
        cumsum(vapply(paired, .subset2, integer(1L), "n_grp")[-length(paired)])
    )

    map <- data.table::data.table(
        index_ori = un_list(lapply(paired, .subset2, "index")),
        index_cur = un_list(.mapply(
            function(group, offset) group + offset,
            list(group = lapply(paired, .subset2, "group"), offset = offset),
            NULL
        ))
    )

    if (!full) {
        return(map)
    }

    value <- data.table::setDT(c(
        list(
            rleid = un_list(.mapply(
                function(rleid, offset) rleid + offset,
                list(rleid = lapply(paired, .subset2, "rleid"), offset = offset),
                NULL
            ))
        ),
        data.table::setattr(lapply(col_data, function(col) {
            un_list(lapply(paired, .subset2, col))
        }), "names", col_data)
    ))
    data.table::setattr(value, "map", map)

    value
}

un_list <- function(lst, recursive = FALSE, use.names = FALSE) {
    unlist(lst, recursive = recursive, use.names = use.names)
}

format_time <- function(x) {
    hours <- x %/% 60L
    mins <- x - hours * 60L
    sprintf("%02i:%02i", hours, mins)
}
