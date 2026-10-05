# ROOM.SET_TERMINAL_MAX is total W per room, independent of AHU reheat.
# Explicit ROOM properties take precedence. An absent type uses a disclosed
# converter electric default, informed by the audited installation, without
# claiming that every DeST version or runtime shares this default.
hvac__room_terminal_source <- function(dest, room_ids) {
    checkmate::assert_integerish(room_ids, any.missing = FALSE, unique = TRUE)
    hvac__assert_source_table(
        dest,
        "ROOM",
        c("ID", "SET_TERMINAL_MAX"),
        "The DeST model",
        TRUE
    )
    rooms <- DBI::dbReadTable(dest, "ROOM")
    hvac__assert_unique_key(rooms$ID, "ROOM.ID")
    rows <- match(room_ids, rooms$ID)
    if (anyNA(rows)) {
        abort(
            "Unresolved ROOM terminal references.",
            class = "destep_unresolved_hvac_terminal"
        )
    }
    rooms <- rooms[rows, , drop = FALSE]
    capacity <- as.numeric(rooms$SET_TERMINAL_MAX)
    if (anyNA(capacity) || any(!is.finite(capacity)) || any(capacity < 0)) {
        abort(
            "ROOM.SET_TERMINAL_MAX must be finite nonnegative total watts per room.",
            class = "destep_invalid_hvac_terminal_capacity"
        )
    }
    roots <- if ("EXT_PROPERTY" %in% names(rooms)) {
        rooms$EXT_PROPERTY
    } else {
        rep(0L, nrow(rooms))
    }
    type <- hvac__linked_integer_property(dest, roots, "ROOM_REHEATER_TYPE")
    explicit <- !is.na(type)
    if (any(explicit & !type %in% 0:2)) {
        abort(
            "ROOM_REHEATER_TYPE must be 0 (none), 1 (electric), or 2 (hot water).",
            class = "destep_invalid_hvac_terminal_type"
        )
    }
    # The GUI key alone does not establish ROOM_GROUP property ownership.
    # Detect such records rather than silently ignoring a possibly active type.
    if (
        all(c("OF_ROOM_GROUP") %in% names(rooms)) &&
            db_has_fields(
                dest,
                "ROOM_GROUP",
                c("ROOM_GROUP_ID", "EXT_PROPERTY")
            )
    ) {
        groups <- DBI::dbReadTable(dest, "ROOM_GROUP")
        hvac__assert_unique_key(
            groups$ROOM_GROUP_ID,
            "ROOM_GROUP.ROOM_GROUP_ID"
        )
        group_roots <- groups$EXT_PROPERTY[match(
            rooms$OF_ROOM_GROUP,
            groups$ROOM_GROUP_ID
        )]
        group_type <- hvac__linked_integer_property(
            dest,
            group_roots,
            "ROOM_REHEATER_TYPE"
        )
        if (any(!is.na(group_type))) {
            abort(
                "ROOM_GROUP contains ROOM_REHEATER_TYPE; its ownership/inheritance is not yet verified.",
                class = "destep_unresolved_hvac_terminal"
            )
        }
    }
    missing <- !explicit & capacity > 0
    type[!explicit] <- data.table::fifelse(capacity[!explicit] > 0, 1L, 0L)
    data.table::data.table(
        room_id = as.integer(room_ids),
        terminal_capacity_w = capacity,
        terminal_type = as.integer(type),
        terminal_type_origin = data.table::fcase(
            explicit , "source_room_property"       ,
            missing  , "converter_default_electric" ,
            default = "zero_source_capacity"
        ),
        terminal_has_reheat = capacity > 0 & type != 0L
    )
}

# Derive zone OA from source system total and terminal operating flow limits.
# This preserves the system total; it is not a DeST hourly allocation solver.
# Optional explicit allocations retain their former per-system checks.
hvac__terminal_outdoor_air <- function(source, allocation = NULL) {
    zones <- data.table::copy(source$zones)
    room_ids <- as.character(zones$room_id)
    total <- source$system$outdoor_air_flow_m3_s[[1L]]
    checkmate::assert_number(total, finite = TRUE, lower = 0)
    minimum <- zones$source_minimum_outdoor_air_flow_m3_s
    if (is.null(minimum)) {
        minimum <- rep(0, nrow(zones))
    }
    checkmate::assert_numeric(
        minimum,
        len = nrow(zones),
        any.missing = FALSE,
        finite = TRUE,
        lower = 0
    )
    if (is.null(allocation)) {
        supply <- zones$maximum_supply_flow_m3_s
        checkmate::assert_numeric(
            supply,
            any.missing = FALSE,
            finite = TRUE,
            lower = 0
        )
        if (!length(supply) || sum(supply) <= 0) {
            abort(
                "Cannot allocate source outdoor air without positive design supply flow.",
                class = "destep_invalid_hvac_air_balance"
            )
        }
        if (any(minimum > supply) || sum(minimum) > total + 1e-8) {
            abort(
                "ROOM minimum outdoor-air requirements conflict with source system or design supply limits.",
                class = "destep_invalid_hvac_air_balance"
            )
        }
        # Preserve declared room minima and distribute only the remaining
        # system total over available minimum operating supply capacity. For
        # VAV, design-flow shares alone can exceed a zone's minimum supply;
        # CAV minimum and maximum flows coincide. This is a target allocation.
        remaining <- max(0, total - sum(minimum))
        limit <- zones$minimum_supply_flow_m3_s
        if (is.null(limit)) {
            limit <- supply
        }
        checkmate::assert_numeric(
            limit,
            len = length(supply),
            any.missing = FALSE,
            finite = TRUE,
            lower = 0
        )
        if (any(minimum > limit) || total > sum(limit) + 1e-8) {
            abort(
                "Fixed outdoor air exceeds terminal minimum operating supply limits.",
                class = "destep_invalid_hvac_air_balance"
            )
        }
        available <- limit - minimum
        if (remaining > 0 && sum(available) <= 0) {
            abort(
                "Source system outdoor air exceeds available design supply flow.",
                class = "destep_invalid_hvac_air_balance"
            )
        }
        values <- minimum
        if (remaining > 0) {
            values <- values + remaining * available / sum(available)
        }
        # Close floating-point summation only; never normalize conflicting source values.
        values[[length(values)]] <- values[[length(values)]] +
            total -
            sum(values)
        origin <- if (any(minimum > 0)) {
            "source_room_minima_plus_system_remainder"
        } else {
            "source_system_total_by_terminal_supply_share"
        }
    } else {
        checkmate::assert_numeric(
            allocation,
            lower = 0,
            finite = TRUE,
            any.missing = FALSE,
            names = "unique"
        )
        checkmate::assert_subset(room_ids, names(allocation))
        values <- as.numeric(allocation[room_ids])
        checkmate::assert_true(isTRUE(all.equal(
            sum(values),
            total,
            tolerance = 1e-8
        )))
        origin <- "user_override"
    }
    # An override changes allocation, never the source room's ventilation need.
    if (any(values < minimum - 1e-8)) {
        abort(
            "Zone outdoor air is below the source ROOM minimum requirement.",
            class = "destep_invalid_hvac_air_balance"
        )
    }
    ceiling <- zones$minimum_supply_flow_m3_s
    if (is.null(ceiling)) {
        ceiling <- zones$maximum_supply_flow_m3_s
    }
    if (any(values > ceiling + 1e-8)) {
        abort(
            "Zone outdoor air exceeds source minimum operating supply flow.",
            class = "destep_invalid_hvac_air_balance"
        )
    }
    data.table::set(zones, NULL, "outdoor_air_flow_m3_s", values)
    data.table::set(
        zones,
        NULL,
        "outdoor_air_allocation_origin",
        rep(origin, nrow(zones))
    )
    source$zones <- zones
    source
}

# Emit one disclosure only after successful object assembly; input readers
# remain pure and repeated validation does not repeat default warnings.
hvac__warn_terminal_defaults <- function(terminals) {
    assumed <- terminals$room_id[
        terminals$terminal_type_origin == "converter_default_electric"
    ]
    if (length(assumed)) {
        warn(
            paste0(
                "ROOM_REHEATER_TYPE is absent for ROOM IDs ",
                fmt_integer_sample(assumed),
                "; using the converter electric-terminal default while preserving source capacity in W."
            ),
            class = "destep_assumed_hvac_terminal_type"
        )
    }
    invisible(NULL)
}

# Persist source units, explicit/default type and derived OA allocation in
# saved IDFs as well as the richer in-memory conversion audit.
hvac__terminal_comments <- function(terminals) {
    if (is.null(terminals) || !nrow(terminals)) {
        return(character())
    }
    sprintf(
        "destep ROOM %s terminal: capacity=%g W; type=%s; origin=%s; OA=%g m3/s; allocation=%s",
        terminals$room_id,
        terminals$terminal_capacity_w,
        terminals$terminal_type,
        terminals$terminal_type_origin,
        terminals$outdoor_air_flow_m3_s,
        terminals$outdoor_air_allocation_origin
    )
}
# Replace the one-room passive terminal with native constant-volume electric
# reheat when ROOM supplies an active terminal. Preserve the zone inlet and
# split the upstream node only for legacy Uncontrolled objects, which have
# no separate inlet. The native terminal controls heat from zone demand;
# source watts cap the coil and no AHU reheat is inferred.
hvac__refine_single_terminal <- function(model, source, terminal) {
    if (!terminal$terminal_has_reheat[[1L]]) {
        return(invisible(model))
    }
    checkmate::assert_true(terminal$terminal_type[[1L]] == 1L)
    zone <- source$zone_name[[1L]]
    connections <- model$to_table(
        class = "ZoneHVAC:EquipmentConnections",
        wide = TRUE
    )
    connection <- connections[connections$`Zone Name` == zone]
    checkmate::assert_data_frame(connection, nrows = 1L)
    equipment <- model$object(connection$`Zone Conditioning Equipment List Name`[[
        1L
    ]])
    equipment_type <- unname(unlist(equipment$value(
        "zone_equipment_1_object_type"
    )))
    equipment_name <- unname(unlist(equipment$value("zone_equipment_1_name")))
    unit <- NULL
    if (equipment_type == "ZoneHVAC:AirDistributionUnit") {
        unit <- model$object(equipment_name)
        old <- model$object(unname(unlist(unit$value("air_terminal_name"))))
        inlet <- unname(unlist(old$value("air_inlet_node_name")))
        outlet <- unname(unlist(old$value("air_outlet_node_name")))
    } else {
        checkmate::assert_choice(
            equipment_type,
            "AirTerminal:SingleDuct:Uncontrolled"
        )
        old <- model$object(equipment_name)
        outlet <- unname(unlist(old$value("zone_supply_air_node_name")))
        inlet <- paste(zone, "Terminal Reheat Inlet")
        # Change only the splitter outlet feeding this zone, not references
        # to the original zone inlet used by thermostat/control objects.
        splitter <- model$to_table(class = "AirLoopHVAC:ZoneSplitter")
        rows <- splitter[!is.na(splitter$value) & splitter$value == outlet]
        checkmate::assert_data_frame(rows, nrows = 1L)
        do.call(
            model$object(rows$id[[1L]])$set,
            stats::setNames(list(inlet), rows$field[[1L]])
        )
    }
    availability <- unname(unlist(old$value("availability_schedule_name")))
    # A blank native availability field means always available. Leave it
    # blank rather than passing eplusr's NA sentinel as an IDF field value.
    if (!length(availability) || is.na(availability)) {
        availability <- NULL
    }
    name <- old$name()
    coil <- paste(zone, "Terminal Electric Reheat")
    suppressMessages(model$del(old$id(), .force = TRUE))
    model$add(
        `Coil:Heating:Electric` = list(
            name = coil,
            availability_schedule_name = availability,
            efficiency = 1,
            nominal_capacity = terminal$terminal_capacity_w[[1L]],
            air_inlet_node_name = inlet,
            air_outlet_node_name = outlet
        ),
        `AirTerminal:SingleDuct:ConstantVolume:Reheat` = list(
            name = name,
            availability_schedule_name = availability,
            air_outlet_node_name = outlet,
            air_inlet_node_name = inlet,
            maximum_air_flow_rate = source$supply_flow_m3_s[[1L]],
            reheat_coil_object_type = "Coil:Heating:Electric",
            reheat_coil_name = coil
        )
    )
    if (is.null(unit)) {
        unit_name <- paste(zone, "Terminal Air Distribution Unit")
        model$add(
            `ZoneHVAC:AirDistributionUnit` = list(
                name = unit_name,
                air_distribution_unit_outlet_node_name = outlet,
                air_terminal_object_type = "AirTerminal:SingleDuct:ConstantVolume:Reheat",
                air_terminal_name = name
            )
        )
        equipment$set(
            zone_equipment_1_object_type = "ZoneHVAC:AirDistributionUnit",
            zone_equipment_1_name = unit_name
        )
    } else {
        unit$set(
            air_terminal_object_type = "AirTerminal:SingleDuct:ConstantVolume:Reheat"
        )
    }
    model$object(coil)$comment(
        c(
            sprintf(
                "DeST ROOM %s SET_TERMINAL_MAX=%g W; terminal type origin=%s.",
                terminal$room_id[[1L]],
                terminal$terminal_capacity_w[[1L]],
                terminal$terminal_type_origin[[1L]]
            ),
            "Target electric-to-heat efficiency=1; native zone-demand control; no inferred AHU reheat."
        ),
        append = TRUE
    )
    invisible(model)
}
