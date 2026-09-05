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

# Require one source table and every field needed to resolve an HVAC relation.
hvac__assert_source_table <- function(dest, table, fields, source) {
    if (!db_has_rows(dest, table) || !db_has_fields(dest, table, fields)) {
        abort(
            paste0(
                source,
                " must contain a non-empty ",
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

# Read source-backed air-system equipment relations from a DeST model and its
# matching external DeST equipment database.
hvac__read_source_equipment <- function(dest, equipment) {
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
            "The DeST model"
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
