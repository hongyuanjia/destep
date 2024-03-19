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
#'
#' @param copy \[logical\] Whether to copy the input DeST database to a
#'        temporary SQLite database. Note that if `FALSE`, the input database
#'        will be modified during the conversion. Default is `TRUE`.
#'
#' @return \[eplusr::Idf\] The converted EnergyPlus model.
#'
#' @export
# TODO: How about ROOM_GROUP? and STOREY_GROUP?
to_eplus <- function(dest, ver = "latest", copy = TRUE) {
    if (is.character(dest) && length(dest) == 1L && file.exists(dest)) {
        dest <- read_dest(dest)
        on.exit(DBI::dbDisconnect(dest), add = TRUE)
    } else if (!inherits(dest, "DBIConnection")) {
        stop("'dest' should be a path to a DeST model file or a DBIConnection object.")
    }

    if (!(length(copy) == 1L && is.logical(copy) && !is.na(copy))) {
        stop("'copy' should be a single logical value of 'TRUE' or 'FALSE'")
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
    ep <- eplusr::empty_idf(ver)

    # update object names and make sure all names are unique
    destep_update_name(tmpdb)

    # update Version comments
    # TODO: currently eplusr does not support updating comments for Version
    # object
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
        const    = destep_conv_const(tmpdb, ep)
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
    bld <- DBI::dbGetQuery(dest,
        "SELECT BUILDING_ID AS ID, NAME FROM BUILDING"
    )
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
            ROUND(R.X / 1000, 3) AS X,
            ROUND(R.Y / 1000, 3) AS Y,
            ROUND(R.Z / 1000, 3) AS Z,
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
            # TODO: handle origin
            # x_origin = room$X,
            # y_origin = room$Y,
            # z_origin = room$Z,
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
# WINDOW -> FenestrationSurface:Detailed
destep_conv_window <- function(dest, ep) {
}

assert_unique_name <- function(names, type) {
    if (anyDuplicated(names)) {
        stop(sprintf(
            "Duplicated %s names found: [%s]. This should already be handled when updating the names.",
            type, paste(unique(names[duplicated(names)]), collapse = ", ")
        ))
    }
}
