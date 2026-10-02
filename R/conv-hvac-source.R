# Convert a DeST volumetric flow from m3/h to the EnergyPlus SI unit m3/s.
hvac__flow_m3_h_to_m3_s <- function(value) {
    checkmate::assert_numeric(value, any.missing = FALSE, finite = TRUE)
    as.numeric(value) / 3600
}

# Convert a DeST equipment capacity from kW to the EnergyPlus SI unit W.
hvac__capacity_kw_to_w <- function(value) {
    checkmate::assert_numeric(value, any.missing = FALSE, finite = TRUE)
    as.numeric(value) * 1000
}

# Convert a DeST duct dimension from mm to the EnergyPlus SI unit m.
hvac__length_mm_to_m <- function(value) {
    checkmate::assert_numeric(value, any.missing = FALSE, finite = TRUE)
    as.numeric(value) / 1000
}

# Require source table schemas while permitting optional component tables to be
# empty; the relation checks below determine whether selected records exist.
hvac__assert_source_table <- function(
    dest,
    table,
    fields,
    source,
    allow_empty = FALSE
) {
    present <- table %in% DBI::dbListTables(dest)
    if (
        !present ||
            !db_has_fields(dest, table, fields) ||
            (!allow_empty && !db_has_rows(dest, table))
    ) {
        abort(
            paste0(
                source,
                " must contain a ",
                if (allow_empty) "" else "non-empty ",
                table,
                " table with fields: ",
                paste(fields, collapse = ", "),
                "."
            ),
            class = "destep_invalid_hvac_source_schema"
        )
    }
    invisible(TRUE)
}

# Reject missing or duplicate source keys before they can create ambiguous joins.
hvac__assert_unique_key <- function(value, name) {
    if (anyNA(value) || anyDuplicated(value)) {
        abort(
            paste0(name, " must contain unique non-missing values."),
            class = "destep_invalid_hvac_source_key"
        )
    }
    invisible(TRUE)
}

# Follow each selected AHU's linked extended properties without interpreting
# legacy TYPE codes or inferring equipment topology. In current source models,
# fan powers occupy DATA_DOUBLE even when TYPE is zero; retain both raw values.
hvac__read_ahu_properties <- function(dest, ahu_ids = NULL) {
    hvac__assert_source_table(dest, "AHU", "AHU_ID", "The DeST model", TRUE)
    empty <- data.table::data.table(
        ahu_id = integer(),
        property_id = integer(),
        property_order = integer(),
        name = character(),
        declared_type = integer(),
        data_long = numeric(),
        data_double = numeric(),
        fan_power_w = numeric(),
        schedule_id = integer()
    )
    if (!db_has_fields(dest, "AHU", "EXT_PROPERTY")) {
        return(empty)
    }
    handlers <- DBI::dbGetQuery(dest, "SELECT AHU_ID, EXT_PROPERTY FROM AHU")
    if (!is.null(ahu_ids)) {
        checkmate::assert_integerish(
            ahu_ids,
            any.missing = FALSE,
            unique = TRUE
        )
        missing <- setdiff(ahu_ids, handlers$AHU_ID)
        if (length(missing)) {
            abort(
                paste0("Missing AHU IDs: ", fmt_integer_sample(missing), "."),
                class = "destep_unresolved_hvac_property"
            )
        }
        handlers <- handlers[handlers$AHU_ID %in% ahu_ids, , drop = FALSE]
    }
    hvac__assert_unique_key(handlers$AHU_ID, "AHU.AHU_ID")
    roots <- handlers$EXT_PROPERTY
    active <- !is.na(roots) & roots != 0L
    if (!any(active)) {
        return(empty)
    }
    hvac__assert_source_table(
        dest,
        "EXT_PROPERTY",
        c(
            "PROPERTY_ID",
            "NEXT_PROPERTY",
            "NAME",
            "TYPE",
            "DATA_LONG",
            "DATA_DOUBLE"
        ),
        "The DeST model"
    )
    properties <- DBI::dbGetQuery(
        dest,
        "SELECT PROPERTY_ID, NEXT_PROPERTY, NAME, TYPE, DATA_LONG, DATA_DOUBLE FROM EXT_PROPERTY"
    )
    ids <- properties$PROPERTY_ID
    duplicates <- unique(ids[duplicated(ids)])
    active_rows <- which(active)
    pieces <- vector("list", length(active_rows))
    # Linked-list traversal depends on the preceding pointer. Allocate once per
    # chain and stop on cycles, dangling pointers or ambiguous reachable keys.
    for (i in seq_along(pieces)) {
        row <- active_rows[[i]]
        pointer <- roots[[row]]
        indices <- integer(nrow(properties))
        visited <- rep(FALSE, nrow(properties))
        count <- 0L
        while (pointer != 0L) {
            index <- match(pointer, ids)
            if (is.na(index) || pointer %in% duplicates || visited[[index]]) {
                abort(
                    paste0(
                        "AHU ",
                        handlers$AHU_ID[[row]],
                        " has a missing, duplicate or cyclic EXT_PROPERTY reference: ",
                        pointer,
                        "."
                    ),
                    class = "destep_unresolved_hvac_property"
                )
            }
            visited[[index]] <- TRUE
            count <- count + 1L
            indices[[count]] <- index
            pointer <- properties$NEXT_PROPERTY[[index]]
            if (is.na(pointer)) {
                abort(
                    "EXT_PROPERTY.NEXT_PROPERTY must end with zero, not NA.",
                    class = "destep_unresolved_hvac_property"
                )
            }
        }
        selected <- properties[indices[seq_len(count)], , drop = FALSE]
        if (anyNA(selected$NAME) || anyDuplicated(selected$NAME)) {
            abort(
                "An AHU property chain must contain unique non-missing names.",
                class = "destep_unresolved_hvac_property"
            )
        }
        pieces[[i]] <- data.table::data.table(
            ahu_id = as.integer(handlers$AHU_ID[[row]]),
            property_id = as.integer(selected$PROPERTY_ID),
            property_order = seq_len(count),
            name = as.character(selected$NAME),
            declared_type = as.integer(selected$TYPE),
            data_long = as.numeric(selected$DATA_LONG),
            data_double = as.numeric(selected$DATA_DOUBLE)
        )
    }
    out <- data.table::rbindlist(pieces)
    data.table::set(out, NULL, "fan_power_w", rep(NA_real_, nrow(out)))
    data.table::set(out, NULL, "schedule_id", rep(NA_integer_, nrow(out)))
    power <- which(
        out$name %in%
            c(
                "AHU_POWER_OF_SUPPLY_FAN",
                "AHU_POWER_OF_RETURN_FAN"
            )
    )
    checkmate::assert_numeric(
        out$data_double[power],
        any.missing = FALSE,
        finite = TRUE,
        lower = 0
    )
    # GUI fan inputs use kW. Normalize the raw power only; its electrical,
    # shaft-work and airstream-heat roles are not universally established.
    data.table::set(out, power, "fan_power_w", out$data_double[power] * 1000)
    schedule <- which(
        out$name %in%
            c(
                "AHU_TWO_PIPE_WATER_SCH",
                "AHU_FOUR_PIPE_COLD_WATER_SCH",
                "AHU_FOUR_PIPE_HOT_WATER_SCH"
            )
    )
    checkmate::assert_integerish(
        out$data_long[schedule],
        any.missing = FALSE,
        lower = 0
    )
    data.table::set(
        out,
        schedule,
        "schedule_id",
        as.integer(out$data_long[schedule])
    )
    out
}

# Count model-local component records separately from embedded catalogue rows.
# Neither a non-empty LIB_* table nor a component row alone establishes plant
# ownership by the converted air system; later stages must resolve references.
hvac__source_component_counts <- function(
    dest,
    tables = DBI::dbListTables(dest)
) {
    names <- c(
        "CHILLER",
        "BOILER",
        "COOLINGTOWER",
        "CPS",
        "WATER_SYSTEM",
        "FAN",
        "DUCTNET",
        "LIB_CHILLER",
        "LIB_BOILER",
        "LIB_COOLINGTOWER"
    )
    counts <- rep(NA_integer_, length(names))
    present <- names %in% tables
    for (i in which(present)) {
        counts[[i]] <- DBI::dbGetQuery(
            dest,
            paste0('SELECT COUNT(*) AS N FROM "', names[[i]], '"')
        )$N[[1L]]
    }
    data.table::data.table(
        TABLE = names,
        SOURCE_ROLE = c(
            rep("model_component", 7L),
            rep("catalogue", 3L)
        ),
        PRESENT = present,
        ROW_COUNT = counts
    )
}

# Summarize plant records without treating catalogue rows as selected equipment.
# The result is used in early diagnostics and saved IDF provenance comments.
hvac__plant_record_summary <- function(inventory) {
    components <- inventory$components
    plant <- components[
        components$TABLE %in% c("CHILLER", "BOILER", "COOLINGTOWER", "CPS")
    ]
    paste(
        plant$TABLE,
        data.table::fifelse(
            plant$PRESENT,
            as.character(plant$ROW_COUNT),
            "table missing"
        ),
        sep = "=",
        collapse = "; "
    )
}

# Resolve one model-local plant component to its embedded library row without
# assigning the component to an air system or projecting its performance to
# EnergyPlus. Nonpositive library IDs are retained as unselected source records.
hvac__plant_product_table <- function(
    dest,
    model_table,
    model_id,
    library_ref,
    library_table,
    library_id,
    fields
) {
    tables <- DBI::dbListTables(dest)
    if (!model_table %in% tables) {
        return(data.table::data.table())
    }
    model_fields <- c(model_id, library_ref)
    if (!db_has_fields(dest, model_table, model_fields)) {
        abort(
            paste0(
                model_table,
                " requires fields: ",
                paste(model_fields, collapse = ", ")
            ),
            class = "destep_invalid_hvac_source_schema"
        )
    }
    records <- data.table::as.data.table(DBI::dbGetQuery(
        dest,
        paste0(
            'SELECT "',
            model_id,
            '", "',
            library_ref,
            '" FROM "',
            model_table,
            '"'
        )
    ))
    if (!nrow(records)) {
        return(records)
    }
    hvac__assert_unique_key(
        records[[model_id]],
        paste0(model_table, ".", model_id)
    )
    selected <- !is.na(records[[library_ref]]) & records[[library_ref]] > 0L
    data.table::set(
        records,
        NULL,
        "SELECTION_STATE",
        data.table::fifelse(selected, "selected", "unselected")
    )
    if (!any(selected)) {
        for (field in names(fields)) {
            data.table::set(records, NULL, field, rep(NA_real_, nrow(records)))
        }
        return(records)
    }
    required <- c(library_id, unname(fields))
    if (
        !library_table %in% tables ||
            !db_has_fields(dest, library_table, required)
    ) {
        abort(
            paste0(
                model_table,
                " references ",
                library_table,
                " but its product fields are unavailable: ",
                paste(required, collapse = ", ")
            ),
            class = "destep_invalid_hvac_source_schema"
        )
    }
    products <- data.table::as.data.table(DBI::dbGetQuery(
        dest,
        paste0(
            "SELECT ",
            paste0('"', unique(required), '"', collapse = ", "),
            ' FROM "',
            library_table,
            '"'
        )
    ))
    products <- products[
        products[[library_id]] %in% records[[library_ref]][selected]
    ]
    hvac__assert_unique_key(
        products[[library_id]],
        paste0(library_table, ".", library_id)
    )
    product_rows <- match(records[[library_ref]], products[[library_id]])
    unresolved <- selected & is.na(product_rows)
    if (any(unresolved)) {
        abort(
            paste0(
                model_table,
                ".",
                library_ref,
                " references missing ",
                library_table,
                " IDs: ",
                fmt_integer_sample(unique(records[[library_ref]][unresolved])),
                "."
            ),
            class = "destep_unresolved_hvac_plant_product"
        )
    }
    for (field in names(fields)) {
        data.table::set(
            records,
            NULL,
            field,
            products[[fields[[field]]]][product_rows]
        )
    }
    records
}

# Inventory chiller, boiler, and cooling-tower product selections using the
# model's own LIB_* tables. This records available source parameters only; the
# plant's ownership and EnergyPlus component mapping remain separate work.
hvac__read_model_plant <- function(dest) {
    list(
        chillers = hvac__plant_product_table(
            dest,
            "CHILLER",
            "CHILLER_ID",
            "LIB_CHILLER_ID",
            "LIB_CHILLER",
            "LIB_CHILLER_ID",
            c(
                TYPE_CODE = "TYPE",
                CAPACITY_KW = "CAPACITY",
                RATED_COP = "COP",
                COP_CURVE_ID = "COP_CURVE",
                COLD_WATER_FLOW_M3_H = "FLOWRATE_COLD",
                COOLING_WATER_FLOW_M3_H = "FLOWRATE_COOL"
            )
        ),
        boilers = hvac__plant_product_table(
            dest,
            "BOILER",
            "BOILER_ID",
            "LIB_BOILER_ID",
            "LIB_BOILER",
            "LIB_BOILER_ID",
            c(
                TYPE_CODE = "TYPE",
                CAPACITY_KW = "CAPACITY",
                RATED_EFFICIENCY = "EFFICIENCY",
                FUEL_TYPE_CODE = "FUEL_TYPE",
                SUPPLY_TEMPERATURE_C = "SUPPLY_TEMPERATURE",
                RETURN_TEMPERATURE_C = "RETURN_TEMPERATURE"
            )
        ),
        cooling_towers = hvac__plant_product_table(
            dest,
            "COOLINGTOWER",
            "COOLINGTOWER_ID",
            "LIB_COOLINGTOWER_ID",
            "LIB_COOLINGTOWER",
            "LIB_COOLINGTOWER_ID",
            c(
                TYPE_CODE = "TYPE",
                CAPACITY_KW = "CAPACITY",
                WATER_FLOW_M3_H = "FLOWRATE",
                FAN_POWER_KW = "FAN_POWER",
                SUPPLY_TEMPERATURE_C = "SUPPLY_TEMPERATURE",
                RETURN_TEMPERATURE_C = "RETURN_TEMPERATURE"
            )
        )
    )
}

# Classify effective room controls and referenced air systems before choosing
# an EnergyPlus HVAC representation. Unreferenced AC_SYS records are retained in
# the source database but do not define equipment for a converted room.
hvac__source_inventory <- function(dest) {
    required <- list(
        ROOM = c("ID", "NAME", "TYPE", "OF_ROOM_GROUP"),
        ROOM_GROUP = c("ROOM_GROUP_ID", "IS_AC_ROOM", "OF_AC_SYS"),
        ROOM_TYPE_DATA = c("ID", "AC_SCHEDULE_ID")
    )
    tables <- DBI::dbListTables(dest)
    for (table in names(required)) {
        if (
            !table %in% tables || !db_has_fields(dest, table, required[[table]])
        ) {
            abort(
                paste0(
                    "HVAC inventory requires DeST table ",
                    table,
                    " with fields: ",
                    paste(required[[table]], collapse = ", "),
                    "."
                ),
                class = "destep_invalid_hvac_source_schema"
            )
        }
    }

    rooms <- data.table::as.data.table(DBI::dbGetQuery(
        dest,
        "
        SELECT R.ID AS ROOM_ID, R.NAME AS ROOM_NAME,
            R.OF_ROOM_GROUP AS ROOM_GROUP_REFERENCE,
            G.ROOM_GROUP_ID, G.IS_AC_ROOM, G.OF_AC_SYS,
            T.AC_SCHEDULE_ID
        FROM ROOM R
        LEFT JOIN ROOM_GROUP G ON R.OF_ROOM_GROUP = G.ROOM_GROUP_ID
        LEFT JOIN ROOM_TYPE_DATA T ON R.TYPE = T.ID
        ORDER BY R.ID
        "
    ))
    hvac__assert_unique_key(rooms$ROOM_ID, "ROOM.ID after HVAC joins")
    data.table::set(
        rooms,
        NULL,
        "HAS_AC_SCHEDULE",
        !is.na(rooms$AC_SCHEDULE_ID) & rooms$AC_SCHEDULE_ID > 0L
    )
    # A conditioned room without a system is a load-analysis input only when
    # it also has an active room-type availability schedule.
    data.table::set(
        rooms,
        NULL,
        "SOURCE_HVAC_STATE",
        data.table::fcase(
            is.na(rooms$ROOM_GROUP_ID)                                               , "unresolved_room_group" ,
            is.na(rooms$IS_AC_ROOM)                                                  , "unknown_conditioning"  ,
            rooms$IS_AC_ROOM == 0L                                                   , "unconditioned"         ,
            !is.na(rooms$OF_AC_SYS) & rooms$OF_AC_SYS < 0L                           ,
            "invalid_system_reference"                                               ,
            (is.na(rooms$OF_AC_SYS) | rooms$OF_AC_SYS == 0L) & rooms$HAS_AC_SCHEDULE ,
            "load_only"                                                              ,
            is.na(rooms$OF_AC_SYS) | rooms$OF_AC_SYS == 0L                           ,
            "conditioned_without_schedule"                                           ,
            default = "system_reference"
        )
    )

    system_ids <- sort(unique(rooms$OF_AC_SYS[
        rooms$SOURCE_HVAC_STATE == "system_reference"
    ]))
    systems <- data.table::data.table(AC_SYS_ID = as.integer(system_ids))
    if (length(system_ids)) {
        system_fields <- c("AC_SYS_ID", "NAME", "AC_SYS_TYPE", "FRESH_AIR_TYPE")
        if (
            !"AC_SYS" %in% tables ||
                !db_has_fields(dest, "AC_SYS", system_fields)
        ) {
            abort(
                "Referenced DeST air systems require AC_SYS with ID, name, type, and fresh-air type.",
                class = "destep_invalid_hvac_source_schema"
            )
        }
        source_systems <- data.table::as.data.table(DBI::dbGetQuery(
            dest,
            "SELECT AC_SYS_ID, NAME, AC_SYS_TYPE, FRESH_AIR_TYPE FROM AC_SYS"
        ))
        # Duplicate or incomplete library rows outside the room-owned subset
        # must not decide whether the converted building has a valid system.
        source_systems <- source_systems[
            source_systems$AC_SYS_ID %in% system_ids
        ]
        hvac__assert_unique_key(source_systems$AC_SYS_ID, "AC_SYS.AC_SYS_ID")
        system_rows <- match(system_ids, source_systems$AC_SYS_ID)
        data.table::set(systems, NULL, "NAME", source_systems$NAME[system_rows])
        data.table::set(
            systems,
            NULL,
            "AC_SYS_TYPE",
            source_systems$AC_SYS_TYPE[system_rows]
        )
        data.table::set(
            systems,
            NULL,
            "FRESH_AIR_TYPE",
            source_systems$FRESH_AIR_TYPE[system_rows]
        )

        ahu_fields <- c("AHU_ID", "OF_AC_SYS", "COOLING_COIL", "FAN", "AHURES")
        if (!"AHU" %in% tables || !db_has_fields(dest, "AHU", ahu_fields)) {
            abort(
                "Referenced DeST air systems require AHU fields: AHU_ID, OF_AC_SYS, COOLING_COIL, FAN, AHURES.",
                class = "destep_invalid_hvac_source_schema"
            )
        }
        handlers <- data.table::as.data.table(DBI::dbGetQuery(
            dest,
            "SELECT AHU_ID, OF_AC_SYS, COOLING_COIL, FAN, AHURES FROM AHU"
        ))
        handlers <- handlers[handlers$OF_AC_SYS %in% system_ids]
        hvac__assert_unique_key(handlers$AHU_ID, "AHU.AHU_ID")
        ahu_counts <- tabulate(
            match(handlers$OF_AC_SYS, system_ids),
            nbins = length(system_ids)
        )
        ahu_rows <- match(system_ids, handlers$OF_AC_SYS)
        ahu_rows[ahu_counts != 1L] <- NA_integer_
        data.table::set(systems, NULL, "AHU_COUNT", ahu_counts)
        for (field in c("AHU_ID", "COOLING_COIL", "FAN", "AHURES")) {
            data.table::set(systems, NULL, field, handlers[[field]][ahu_rows])
        }
        data.table::set(
            systems,
            NULL,
            "ROOM_COUNT",
            tabulate(
                match(
                    rooms$OF_AC_SYS[
                        rooms$SOURCE_HVAC_STATE == "system_reference"
                    ],
                    system_ids
                ),
                nbins = length(system_ids)
            )
        )
        data.table::set(
            systems,
            NULL,
            "SYSTEM_STATE",
            data.table::fcase(
                is.na(system_rows)                  , "missing_system"   ,
                !systems$AC_SYS_TYPE %in% c(0L, 1L) , "unsupported_type" ,
                systems$AHU_COUNT == 0L             , "missing_ahu"      ,
                systems$AHU_COUNT > 1L              , "multiple_ahu"     ,
                default = "airside_defined"
            )
        )
        data.table::set(
            systems,
            NULL,
            "COOLING_COIL_STATE",
            data.table::fcase(
                systems$SYSTEM_STATE != "airside_defined"                , "not_evaluated" ,
                is.na(systems$COOLING_COIL) | systems$COOLING_COIL <= 0L ,
                "unselected"                                             ,
                default = "selected_unverified"
            )
        )
    }

    room_states <- rooms$SOURCE_HVAC_STATE
    incomplete <- room_states %in%
        c(
            "unresolved_room_group",
            "unknown_conditioning",
            "invalid_system_reference",
            "conditioned_without_schedule"
        ) |
        (room_states == "system_reference" & !rooms$HAS_AC_SCHEDULE)
    if (
        any(incomplete) ||
            (nrow(systems) && any(systems$SYSTEM_STATE != "airside_defined"))
    ) {
        overall <- "incomplete"
    } else if (nrow(systems)) {
        overall <- if (any(room_states == "load_only")) {
            "mixed_load_and_system"
        } else {
            "system_defined"
        }
    } else if (any(room_states == "load_only")) {
        overall <- "load_only"
    } else {
        overall <- "unconditioned"
    }
    list(
        rooms = rooms,
        systems = systems,
        components = hvac__source_component_counts(dest, tables),
        state = overall
    )
}

# Choose the HVAC representation from effective source ownership. An explicit
# ideal-load request remains available for a load-analysis copy of a model with
# a real air system, but the default must not silently replace that system.
hvac__resolve_representation <- function(requested, inventory) {
    checkmate::assert_choice(requested, c("auto", "ideal_loads", "physical"))
    if (requested == "ideal_loads") {
        return("ideal_loads")
    }

    state <- inventory$state
    if (requested == "physical") {
        if (state != "system_defined") {
            abort(
                paste0(
                    "Physical HVAC requires every conditioned room to reference ",
                    "one supported air system; source state is '",
                    state,
                    "'."
                ),
                class = "destep_incomplete_hvac_source"
            )
        }
        return("physical")
    }
    if (state == "unconditioned") {
        return("none")
    }
    if (state == "load_only") {
        return("ideal_loads")
    }
    if (state == "system_defined") {
        return("physical")
    }
    abort(
        paste0(
            "Automatic HVAC conversion cannot represent source state '",
            state,
            "' without dropping or inventing room equipment. ",
            "Inspect the source HVAC inventory or explicitly request ",
            "hvac = 'ideal_loads' for a load-only analysis copy."
        ),
        class = "destep_incomplete_hvac_source"
    )
}

# Read source-backed air-system equipment relations from a DeST model and its
# matching external DeST equipment database. A supplied ID set limits relation
# validation to air systems owned by converted rooms.
hvac__read_source_equipment <- function(dest, equipment, system_ids = NULL) {
    if (!inherits(dest, "DBIConnection")) {
        abort(
            "'dest' must be a DBI connection to a DeST model.",
            class = "destep_invalid_hvac_source_connection"
        )
    }
    if (!inherits(equipment, "DBIConnection")) {
        abort(
            "'equipment' must be a DBI connection to a DeST equipment database.",
            class = "destep_invalid_hvac_equipment_connection"
        )
    }
    if (!is.null(system_ids)) {
        checkmate::assert_integerish(
            system_ids,
            min.len = 1L,
            lower = 1L,
            any.missing = FALSE,
            unique = TRUE,
            .var.name = "room-referenced DeST AC_SYS IDs"
        )
        system_ids <- as.integer(system_ids)
    }

    model_tables <- list(
        AC_SYS = c(
            "AC_SYS_ID",
            "NAME",
            "AC_SYS_TYPE",
            "FRESH_AIR_TYPE"
        ),
        AHU = c(
            "AHU_ID",
            "NAME",
            "OF_AC_SYS",
            "COOLING_COIL",
            "COIL_NUM",
            "FAN",
            "AHURES"
        ),
        DUCTNET = c(
            "ID",
            "AHUType",
            "RunMode",
            "Constant_P_Point_value",
            "Filter_P",
            "Filter_G",
            "Coil_P",
            "Coil_G",
            "Reheater_P",
            "Reheater_G",
            "SprayRoom_P",
            "SprayRoom_G",
            "RecoverHeat_P",
            "RecoverHeat_G",
            "S_Muffler_Ksai",
            "S_Muffler_Number",
            "S_Elbow_Ksai",
            "S_Elbow_Number",
            "R_Muffler_Ksai",
            "R_Muffler_Number",
            "R_Elbow_Ksai",
            "R_Elbow_Number",
            "F_Muffler_Ksai",
            "F_Muffler_Number",
            "F_Elbow_Ksai",
            "F_Elbow_Number",
            "E_Muffler_Ksai",
            "E_Muffler_Number",
            "E_Elbow_Ksai",
            "E_Elbow_Number",
            "S_FAN",
            "R_FAN",
            "F_FAN",
            "E_FAN",
            "Fresh_Duct_H",
            "Fresh_Duct_W",
            "Fresh_Duct_L",
            "Exhaust_Duct_H",
            "Exhaust_Duct_W",
            "Exhaust_Duct_L"
        ),
        FAN = c(
            "ID",
            "FLOWRATE",
            "PRESSURE",
            "EFFICIENCY",
            "CURVE_PF",
            "CURVE_EF"
        ),
        LIB_CURVE = c(
            "LIB_CURVE_ID",
            "TYPE",
            "NAME",
            "COUNT",
            LETTERS[1:12]
        )
    )
    for (table in names(model_tables)) {
        hvac__assert_source_table(
            dest,
            table,
            model_tables[[table]],
            "The DeST model",
            allow_empty = table %in% c("DUCTNET", "FAN", "LIB_CURVE")
        )
    }

    equipment_tables <- list(
        `_Coil_Cooling` = c(
            "DevID",
            "ProductID",
            "ProductName",
            "RowNum",
            "Load",
            "Air_volume",
            "Fd",
            "Fy",
            "Fw",
            "Ep2_a",
            "Ep2_b",
            "Ep2_c",
            "Ks_a",
            "Ks_b",
            "Ks_c",
            "Ks_d",
            "Ks_e"
        ),
        Esp1CCoil = c(
            "DevID",
            "Facing area (m^2)",
            "Transfer area per row (m^2)",
            "Pipe section area (m^2)",
            "Number of rows",
            "Coef A",
            "Coef B",
            "Coef m",
            "Coef n",
            "Coef p"
        )
    )
    for (table in names(equipment_tables)) {
        hvac__assert_source_table(
            equipment,
            table,
            equipment_tables[[table]],
            "The DeST equipment database"
        )
    }

    # Keep punctuation in equipment-library field names after materialization.
    ac_systems <- data.table::as.data.table(DBI::dbReadTable(
        dest,
        "AC_SYS",
        check.names = FALSE
    ))
    air_handlers <- data.table::as.data.table(DBI::dbReadTable(
        dest,
        "AHU",
        check.names = FALSE
    ))
    duct_networks <- data.table::as.data.table(DBI::dbReadTable(
        dest,
        "DUCTNET",
        check.names = FALSE
    ))
    model_fans <- data.table::as.data.table(DBI::dbReadTable(
        dest,
        "FAN",
        check.names = FALSE
    ))
    model_curves <- data.table::as.data.table(DBI::dbReadTable(
        dest,
        "LIB_CURVE",
        check.names = FALSE
    ))
    coil_products <- data.table::as.data.table(
        DBI::dbReadTable(equipment, "_Coil_Cooling", check.names = FALSE)
    )
    coil_specific <- data.table::as.data.table(
        DBI::dbReadTable(equipment, "Esp1CCoil", check.names = FALSE)
    )

    if (!is.null(system_ids)) {
        missing_systems <- setdiff(system_ids, ac_systems$AC_SYS_ID)
        if (length(missing_systems)) {
            abort(
                paste0(
                    "Room-referenced AC_SYS IDs are missing from the model: ",
                    fmt_integer_sample(missing_systems),
                    "."
                ),
                class = "destep_unresolved_hvac_system_relation"
            )
        }
        # The database can include templates and other unowned air systems.
        # Filter both sides of the ownership join before validating equipment.
        ac_systems <- ac_systems[ac_systems$AC_SYS_ID %in% system_ids]
        air_handlers <- air_handlers[air_handlers$OF_AC_SYS %in% system_ids]
    }

    hvac__assert_unique_key(ac_systems$AC_SYS_ID, "AC_SYS.AC_SYS_ID")
    hvac__assert_unique_key(air_handlers$AHU_ID, "AHU.AHU_ID")
    hvac__assert_unique_key(duct_networks$ID, "DUCTNET.ID")
    hvac__assert_unique_key(model_fans$ID, "FAN.ID")
    hvac__assert_unique_key(model_curves$LIB_CURVE_ID, "LIB_CURVE.LIB_CURVE_ID")
    hvac__assert_unique_key(coil_products$ProductID, "_Coil_Cooling.ProductID")
    hvac__assert_unique_key(coil_specific$DevID, "Esp1CCoil.DevID")

    ac_systems <- ac_systems[, .(
        ac_system_id = as.integer(AC_SYS_ID),
        ac_system_name = as.character(NAME),
        ac_system_type = as.integer(AC_SYS_TYPE),
        fresh_air_type = as.integer(FRESH_AIR_TYPE)
    )]
    air_handlers <- air_handlers[, .(
        ahu_id = as.integer(AHU_ID),
        ahu_name = as.character(NAME),
        ac_system_id = as.integer(OF_AC_SYS),
        cooling_coil_id = as.integer(COOLING_COIL),
        cooling_coil_count = as.integer(COIL_NUM),
        ahu_rated_air_flow_m3_h = as.numeric(FAN),
        duct_network_id = as.integer(AHURES)
    )]
    systems <- merge(
        ac_systems,
        air_handlers,
        by = "ac_system_id",
        all = TRUE,
        sort = FALSE
    )
    if (anyNA(systems$ac_system_type) || anyNA(systems$ahu_id)) {
        abort(
            "AC_SYS and AHU contain an unresolved ownership relation.",
            class = "destep_unresolved_hvac_system_relation"
        )
    }

    networks <- duct_networks[, .(
        duct_network_id = as.integer(ID),
        topology_code = as.integer(AHUType),
        run_mode_code = as.integer(RunMode),
        constant_pressure_setpoint_pa = as.numeric(Constant_P_Point_value),
        filter_pressure_pa = as.numeric(Filter_P),
        filter_rated_flow_m3_s = hvac__flow_m3_h_to_m3_s(Filter_G),
        cooling_coil_pressure_pa = as.numeric(Coil_P),
        cooling_coil_rated_flow_m3_s = hvac__flow_m3_h_to_m3_s(Coil_G),
        reheater_pressure_pa = as.numeric(Reheater_P),
        reheater_rated_flow_m3_s = hvac__flow_m3_h_to_m3_s(Reheater_G),
        spray_room_pressure_pa = as.numeric(SprayRoom_P),
        spray_room_rated_flow_m3_s = hvac__flow_m3_h_to_m3_s(SprayRoom_G),
        heat_recovery_pressure_pa = as.numeric(RecoverHeat_P),
        heat_recovery_rated_flow_m3_s = hvac__flow_m3_h_to_m3_s(RecoverHeat_G),
        supply_muffler_loss_coefficient = as.numeric(S_Muffler_Ksai),
        supply_muffler_count = as.integer(S_Muffler_Number),
        supply_elbow_loss_coefficient = as.numeric(S_Elbow_Ksai),
        supply_elbow_count = as.integer(S_Elbow_Number),
        return_muffler_loss_coefficient = as.numeric(R_Muffler_Ksai),
        return_muffler_count = as.integer(R_Muffler_Number),
        return_elbow_loss_coefficient = as.numeric(R_Elbow_Ksai),
        return_elbow_count = as.integer(R_Elbow_Number),
        fresh_muffler_loss_coefficient = as.numeric(F_Muffler_Ksai),
        fresh_muffler_count = as.integer(F_Muffler_Number),
        fresh_elbow_loss_coefficient = as.numeric(F_Elbow_Ksai),
        fresh_elbow_count = as.integer(F_Elbow_Number),
        exhaust_muffler_loss_coefficient = as.numeric(E_Muffler_Ksai),
        exhaust_muffler_count = as.integer(E_Muffler_Number),
        exhaust_elbow_loss_coefficient = as.numeric(E_Elbow_Ksai),
        exhaust_elbow_count = as.integer(E_Elbow_Number),
        supply_fan_id = as.integer(S_FAN),
        return_fan_id = as.integer(R_FAN),
        fresh_fan_id = as.integer(F_FAN),
        exhaust_fan_id = as.integer(E_FAN),
        fresh_duct_height_m = hvac__length_mm_to_m(Fresh_Duct_H),
        fresh_duct_width_m = hvac__length_mm_to_m(Fresh_Duct_W),
        fresh_duct_length_m = hvac__length_mm_to_m(Fresh_Duct_L),
        exhaust_duct_height_m = hvac__length_mm_to_m(Exhaust_Duct_H),
        exhaust_duct_width_m = hvac__length_mm_to_m(Exhaust_Duct_W),
        exhaust_duct_length_m = hvac__length_mm_to_m(Exhaust_Duct_L)
    )]
    referenced_networks <- unique(systems$duct_network_id[
        !is.na(systems$duct_network_id) & systems$duct_network_id > 0L
    ])
    missing_networks <- setdiff(referenced_networks, networks$duct_network_id)
    if (length(missing_networks)) {
        abort(
            paste0(
                "AHU.AHURES references missing DUCTNET IDs: ",
                fmt_integer_sample(missing_networks),
                "."
            ),
            class = "destep_unresolved_hvac_duct_network"
        )
    }
    networks <- networks[duct_network_id %in% referenced_networks]
    data.table::setorder(networks, duct_network_id)

    fan_links <- data.table::melt(
        networks[, .(
            duct_network_id,
            supply = supply_fan_id,
            return = return_fan_id,
            fresh = fresh_fan_id,
            exhaust = exhaust_fan_id
        )],
        id.vars = "duct_network_id",
        variable.name = "fan_role",
        value.name = "fan_id",
        variable.factor = FALSE
    )
    fan_links[, fan_id := as.integer(fan_id)]
    fan_links[,
        resolution_status := data.table::fcase(
            fan_id == 0L              ,
            "automatic"               ,
            fan_id == 1L              ,
            "custom_unresolved"       ,
            fan_id %in% model_fans$ID ,
            "model_record"            ,
            default = "external_unresolved"
        )
    ]
    data.table::setorder(fan_links, duct_network_id, fan_role)

    resolved_fan_ids <- unique(fan_links[
        resolution_status == "model_record",
        fan_id
    ])
    fans <- model_fans[
        ID %in% resolved_fan_ids,
        .(
            fan_id = as.integer(ID),
            rated_flow_m3_s = hvac__flow_m3_h_to_m3_s(FLOWRATE),
            pressure_rise_pa = as.numeric(PRESSURE),
            rated_efficiency = as.numeric(EFFICIENCY),
            pressure_curve_id = as.integer(CURVE_PF),
            efficiency_curve_id = as.integer(CURVE_EF)
        )
    ]
    if (any(fans$rated_efficiency <= 0 | fans$rated_efficiency > 1)) {
        abort(
            "FAN.EFFICIENCY must be greater than zero and no greater than one.",
            class = "destep_invalid_hvac_fan_efficiency"
        )
    }
    data.table::setorder(fans, fan_id)

    referenced_curve_ids <- unique(c(
        fans$pressure_curve_id,
        fans$efficiency_curve_id
    ))
    referenced_curve_ids <- referenced_curve_ids[
        !is.na(referenced_curve_ids) & referenced_curve_ids > 0L
    ]
    missing_curves <- setdiff(referenced_curve_ids, model_curves$LIB_CURVE_ID)
    if (length(missing_curves)) {
        abort(
            paste0(
                "FAN references missing LIB_CURVE IDs: ",
                fmt_integer_sample(missing_curves),
                "."
            ),
            class = "destep_unresolved_hvac_fan_curve"
        )
    }
    curves <- model_curves[
        LIB_CURVE_ID %in% referenced_curve_ids,
        .(
            curve_id = as.integer(LIB_CURVE_ID),
            curve_type = as.integer(TYPE),
            curve_name = as.character(NAME),
            coefficient_count = as.integer(COUNT),
            coefficient_a = as.numeric(A),
            coefficient_b = as.numeric(B),
            coefficient_c = as.numeric(C),
            coefficient_d = as.numeric(D),
            coefficient_e = as.numeric(E),
            coefficient_f = as.numeric(F),
            coefficient_g = as.numeric(G),
            coefficient_h = as.numeric(H),
            coefficient_i = as.numeric(I),
            coefficient_j = as.numeric(J),
            coefficient_k = as.numeric(K),
            coefficient_l = as.numeric(L)
        )
    ]
    if (any(curves$coefficient_count < 0L | curves$coefficient_count > 12L)) {
        abort(
            "LIB_CURVE.COUNT must be between zero and twelve.",
            class = "destep_invalid_hvac_fan_curve"
        )
    }
    data.table::setorder(curves, curve_id)

    selected_coil_ids <- unique(systems$cooling_coil_id[
        !is.na(systems$cooling_coil_id) & systems$cooling_coil_id > 0L
    ])
    missing_coils <- setdiff(selected_coil_ids, coil_products$ProductID)
    if (length(missing_coils)) {
        abort(
            paste0(
                "AHU.COOLING_COIL references missing equipment products: ",
                fmt_integer_sample(missing_coils),
                "."
            ),
            class = "destep_unresolved_hvac_cooling_coil"
        )
    }
    cooling_coils <- coil_products[
        ProductID %in% selected_coil_ids,
        .(
            cooling_coil_id = as.integer(ProductID),
            device_id = as.integer(DevID),
            product_name = as.character(ProductName),
            row_count = as.integer(RowNum),
            rated_capacity_w = hvac__capacity_kw_to_w(Load),
            rated_air_flow_m3_s = hvac__flow_m3_h_to_m3_s(Air_volume),
            heat_transfer_area_per_row_m2 = as.numeric(Fd),
            face_area_m2 = as.numeric(Fy),
            water_flow_area_m2 = as.numeric(Fw),
            ep2_a = as.numeric(Ep2_a),
            ep2_b = as.numeric(Ep2_b),
            ep2_c = as.numeric(Ep2_c),
            ks_a = as.numeric(Ks_a),
            ks_b = as.numeric(Ks_b),
            ks_c = as.numeric(Ks_c),
            ks_d = as.numeric(Ks_d),
            ks_e = as.numeric(Ks_e)
        )
    ]

    specific <- coil_specific[
        DevID %in% selected_coil_ids,
        .(
            cooling_coil_id = as.integer(DevID),
            specific_face_area_m2 = as.numeric(`Facing area (m^2)`),
            specific_transfer_area_per_row_m2 = as.numeric(
                `Transfer area per row (m^2)`
            ),
            specific_water_flow_area_m2 = as.numeric(`Pipe section area (m^2)`),
            specific_row_count = as.integer(`Number of rows`),
            specific_ks_a = as.numeric(`Coef A`),
            specific_ks_d = as.numeric(`Coef B`),
            specific_ks_b = as.numeric(`Coef m`),
            specific_ks_e = as.numeric(`Coef n`),
            specific_ks_c = as.numeric(`Coef p`)
        )
    ]
    missing_specific <- setdiff(selected_coil_ids, specific$cooling_coil_id)
    if (length(missing_specific)) {
        abort(
            paste0(
                "Esp1CCoil is missing products: ",
                fmt_integer_sample(missing_specific),
                "."
            ),
            class = "destep_unresolved_hvac_cooling_coil"
        )
    }
    coil_check <- merge(
        cooling_coils,
        specific,
        by = "cooling_coil_id",
        all.x = TRUE,
        sort = FALSE
    )
    tolerance <- 1e-5
    checks <- cbind(
        coil_check$row_count == coil_check$specific_row_count,
        abs(coil_check$face_area_m2 - coil_check$specific_face_area_m2) <=
            tolerance,
        abs(
            coil_check$heat_transfer_area_per_row_m2 -
                coil_check$specific_transfer_area_per_row_m2
        ) <=
            tolerance,
        abs(
            coil_check$water_flow_area_m2 -
                coil_check$specific_water_flow_area_m2
        ) <=
            tolerance,
        abs(coil_check$ks_a - coil_check$specific_ks_a) <= tolerance,
        abs(coil_check$ks_b - coil_check$specific_ks_b) <= tolerance,
        abs(coil_check$ks_c - coil_check$specific_ks_c) <= tolerance,
        abs(coil_check$ks_d - coil_check$specific_ks_d) <= tolerance,
        abs(coil_check$ks_e - coil_check$specific_ks_e) <= tolerance
    )
    if (anyNA(checks) || !all(checks)) {
        abort(
            "_Coil_Cooling and Esp1CCoil contain inconsistent product parameters.",
            class = "destep_inconsistent_hvac_cooling_coil"
        )
    }
    cooling_coils[, specific_parameters_match := TRUE]
    data.table::setorder(cooling_coils, cooling_coil_id)

    systems[,
        ahu_rated_air_flow_m3_s := hvac__flow_m3_h_to_m3_s(
            ahu_rated_air_flow_m3_h
        )
    ]
    coil_rows <- match(
        systems$cooling_coil_id,
        cooling_coils$cooling_coil_id
    )
    systems[,
        selected_coil_rated_air_flow_m3_s := cooling_coils$rated_air_flow_m3_s[
            coil_rows
        ]
    ]
    systems[,
        cooling_coil_air_flow_matches := !is.na(
            selected_coil_rated_air_flow_m3_s
        ) &
            abs(
                ahu_rated_air_flow_m3_s -
                    selected_coil_rated_air_flow_m3_s
            ) <=
                tolerance
    ]
    if (any(!systems$cooling_coil_air_flow_matches)) {
        warn(
            paste0(
                "AHU.FAN does not match the selected cooling-coil rated air ",
                "flow for AHU IDs: ",
                fmt_integer_sample(
                    systems$ahu_id[!systems$cooling_coil_air_flow_matches]
                ),
                "."
            ),
            class = "destep_inconsistent_hvac_coil_airflow"
        )
    }
    data.table::setorder(systems, ac_system_id, ahu_id)

    list(
        systems = systems,
        networks = networks,
        fan_links = fan_links,
        fans = fans,
        curves = curves,
        cooling_coils = cooling_coils
    )
}

# Register source and normalized HVAC columns used through data.table NSE.
utils::globalVariables(c(
    "AC_SYS_ID",
    "AC_SYS_TYPE",
    "FRESH_AIR_TYPE",
    "AHU_ID",
    "OF_AC_SYS",
    "COOLING_COIL",
    "COIL_NUM",
    "FAN",
    "AHURES",
    "AHUType",
    "RunMode",
    "Constant_P_Point_value",
    "Filter_P",
    "Filter_G",
    "Coil_P",
    "Coil_G",
    "Reheater_P",
    "Reheater_G",
    "SprayRoom_P",
    "SprayRoom_G",
    "RecoverHeat_P",
    "RecoverHeat_G",
    "S_Muffler_Ksai",
    "S_Muffler_Number",
    "S_Elbow_Ksai",
    "S_Elbow_Number",
    "R_Muffler_Ksai",
    "R_Muffler_Number",
    "R_Elbow_Ksai",
    "R_Elbow_Number",
    "F_Muffler_Ksai",
    "F_Muffler_Number",
    "F_Elbow_Ksai",
    "F_Elbow_Number",
    "E_Muffler_Ksai",
    "E_Muffler_Number",
    "E_Elbow_Ksai",
    "E_Elbow_Number",
    "S_FAN",
    "R_FAN",
    "F_FAN",
    "E_FAN",
    "Fresh_Duct_H",
    "Fresh_Duct_W",
    "Fresh_Duct_L",
    "Exhaust_Duct_H",
    "Exhaust_Duct_W",
    "Exhaust_Duct_L",
    "duct_network_id",
    "supply_fan_id",
    "return_fan_id",
    "fresh_fan_id",
    "exhaust_fan_id",
    "fan_id",
    "resolution_status",
    "fan_role",
    "FLOWRATE",
    "PRESSURE",
    "EFFICIENCY",
    "CURVE_PF",
    "CURVE_EF",
    "LIB_CURVE_ID",
    "COUNT",
    "A",
    "B",
    "C",
    "D",
    "E",
    "F",
    "G",
    "H",
    "I",
    "J",
    "K",
    "L",
    "curve_id",
    "ProductID",
    "DevID",
    "ProductName",
    "RowNum",
    "Load",
    "Air_volume",
    "Fd",
    "Fy",
    "Fw",
    "Ep2_a",
    "Ep2_b",
    "Ep2_c",
    "Ks_a",
    "Ks_b",
    "Ks_c",
    "Ks_d",
    "Ks_e",
    "Facing area (m^2)",
    "Transfer area per row (m^2)",
    "Pipe section area (m^2)",
    "Number of rows",
    "Coef A",
    "Coef B",
    "Coef m",
    "Coef n",
    "Coef p",
    "specific_parameters_match",
    "cooling_coil_id",
    "ahu_rated_air_flow_m3_s",
    "ahu_rated_air_flow_m3_h",
    "selected_coil_rated_air_flow_m3_s",
    "cooling_coil_air_flow_matches",
    "ac_system_id",
    "ahu_id"
))
