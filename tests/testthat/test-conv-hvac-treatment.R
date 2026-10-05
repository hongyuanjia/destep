# Linked properties must resolve within the selected system, even when other
# systems reuse names or include unrelated invalid pointers.
test_that("system humidity properties follow reachable ownership", {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbWriteTable(
        con,
        "AC_SYS",
        data.frame(AC_SYS_ID = 1L, EXT_PROPERTY = 10L)
    )
    DBI::dbWriteTable(
        con,
        "EXT_PROPERTY",
        data.frame(
            PROPERTY_ID = c(10L, 11L, 12L),
            NEXT_PROPERTY = c(11L, 0L, 99L),
            NAME = c("unused", "AC_SYS_MINF_SCH", "AC_SYS_MINF_SCH"),
            DATA_LONG = c(0L, 16L, 90L)
        )
    )
    expect_identical(hvac__system_property(con, 1L, "AC_SYS_MINF_SCH"), 16L)
    for (value in c(16.5, NA_real_, Inf, .Machine$integer.max + 1)) {
        DBI::dbExecute(
            con,
            "UPDATE EXT_PROPERTY SET DATA_LONG=? WHERE PROPERTY_ID=11",
            params = list(value)
        )
        expect_error(
            hvac__system_property(con, 1L, "AC_SYS_MINF_SCH"),
            class = "destep_unresolved_hvac_property"
        )
    }
    DBI::dbExecute(
        con,
        "UPDATE EXT_PROPERTY SET DATA_LONG=16 WHERE PROPERTY_ID=11"
    )

    DBI::dbExecute(
        con,
        "UPDATE EXT_PROPERTY SET NEXT_PROPERTY=10 WHERE PROPERTY_ID=11"
    )
    expect_error(
        hvac__system_property(con, 1L, "AC_SYS_MINF_SCH"),
        class = "destep_unresolved_hvac_property"
    )
    DBI::dbExecute(
        con,
        "UPDATE EXT_PROPERTY SET NEXT_PROPERTY=99 WHERE PROPERTY_ID=11"
    )
    expect_error(
        hvac__system_property(con, 1L, "AC_SYS_MINF_SCH"),
        class = "destep_unresolved_hvac_property"
    )
})

# Reject invalid annual fractional humidity data instead of treating it as
# absence of demand. The converter must validate all hours, not just extrema.
test_that("humidity source schedules require valid annual fractions", {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbExecute(
        con,
        "CREATE TABLE SCHEDULE_YEAR (SCHEDULE_ID INTEGER, NAME TEXT, DATA BLOB)"
    )
    DBI::dbExecute(
        con,
        "INSERT INTO SCHEDULE_YEAR VALUES (1, 'RH', ?)",
        params = list(list(writeBin(rep(0.4, 8760), raw(), endian = "little")))
    )
    expect_equal(hvac__rh_schedule(con, 1L), rep(0.4, 8760))
    DBI::dbExecute(
        con,
        "UPDATE SCHEDULE_YEAR SET DATA=?",
        params = list(list(writeBin(
            c(rep(0.4, 8759), NA_real_),
            raw(),
            endian = "little"
        )))
    )
    expect_error(hvac__rh_schedule(con, 1L))
    DBI::dbExecute(
        con,
        "UPDATE SCHEDULE_YEAR SET DATA=?",
        params = list(list(writeBin(rep(40, 8760), raw(), endian = "little")))
    )
    expect_error(hvac__rh_schedule(con, 1L))
})

# Hourly bounds must be compared in chronological pairs, including shared
# references and a late conflict hidden by identical annual extrema.
test_that("humidity bounds validate each room at each source hour", {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbExecute(
        con,
        "CREATE TABLE SCHEDULE_YEAR (SCHEDULE_ID INTEGER, NAME TEXT, DATA BLOB)"
    )
    lower <- rep(c(0.2, 0.4), 4380)
    upper <- lower
    # Same extrema, but the last two hourly values cross when reversed.
    upper[8759:8760] <- rev(upper[8759:8760])
    for (i in 1:2) {
        DBI::dbExecute(
            con,
            "INSERT INTO SCHEDULE_YEAR VALUES (?, ?, ?)",
            params = list(
                i,
                paste("RH", i),
                list(writeBin(
                    if (i == 1L) lower else upper,
                    raw(),
                    endian = "little"
                ))
            )
        )
    }
    expect_equal(
        hvac__rh_bounds(con, c(1L, 1L), c(1L, 1L), c("ROOM 1", "ROOM 2")),
        list(maximum_lower = c(0.4, 0.4), minimum_upper = c(0.2, 0.2))
    )
    expect_error(
        hvac__rh_bounds(con, c(1L, 1L), c(1L, 2L), c("ROOM 1", "ROOM 2")),
        "ROOM 2.*8760.*schedules 1/2",
        class = "destep_invalid_hvac_humidity_control"
    )
    expect_error(hvac__rh_bounds(con, 1L, c(1L, 2L), "ROOM 1"))
    expect_equal(
        hvac__rh_bounds(con, integer(), integer(), character()),
        list(maximum_lower = numeric(), minimum_upper = numeric())
    )
})

# A real source snapshot distinguishes installed but inactive steam equipment
# from active demand. Use only an in-memory copy for changed source inputs.
test_that("air treatment preserves source types and diagnoses unmet mappings", {
    skip_on_cran()
    path <- Sys.getenv("DESTEP_TEST_HVAC_SINGLE_SQLITE", unset = "")
    skip_if(!file.exists(path), "Archived single-zone AE source is required")
    original <- DBI::dbConnect(
        RSQLite::SQLite(),
        path,
        flags = RSQLite::SQLITE_RO
    )
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    RSQLite::sqliteCopyDatabase(original, con)
    DBI::dbDisconnect(original)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    system <- DBI::dbGetQuery(con, "SELECT AC_SYS_ID FROM AC_SYS")$AC_SYS_ID[[
        1L
    ]]
    inactive <- hvac__air_treatment_source(con, system)
    expect_identical(inactive$status, "inactive_zero_lower_rh")
    expect_equal(inactive$humidifier_code, 1)
    expect_equal(inactive$minimum_supply_maximum_rh, 1)
    expect_equal(inactive$minimum_room_maximum_rh, 1)
    supply_upper_id <- inactive$supply_maximum_rh_schedule_id
    # Preserve the real shared schedule while testing its effect in memory.
    upper_blob <- DBI::dbGetQuery(
        con,
        "SELECT DATA FROM SCHEDULE_YEAR WHERE SCHEDULE_ID=?",
        params = list(supply_upper_id)
    )$DATA
    DBI::dbExecute(
        con,
        "UPDATE SCHEDULE_YEAR SET DATA=? WHERE SCHEDULE_ID=?",
        params = list(
            list(writeBin(rep(0.8, 8760), raw(), endian = "little")),
            supply_upper_id
        )
    )
    expect_error(
        hvac__air_treatment_source(con, system),
        "supply-RH upper limit",
        class = "destep_unsupported_hvac_humidity_control"
    )
    DBI::dbExecute(
        con,
        "UPDATE SCHEDULE_YEAR SET DATA=? WHERE SCHEDULE_ID=?",
        params = list(upper_blob, supply_upper_id)
    )
    controls <- hvac__conditioned_controls(con)
    id <- controls$SET_RH_MIN_SCHEDULE[[1L]]
    DBI::dbExecute(
        con,
        "UPDATE SCHEDULE_YEAR SET DATA=? WHERE SCHEDULE_ID=?",
        params = list(
            list(writeBin(rep(0.4, 8760), raw(), endian = "little")),
            id
        )
    )
    room_upper_id <- controls$SET_RH_MAX_SCHEDULE[[1L]]
    room_upper_blob <- DBI::dbGetQuery(
        con,
        "SELECT DATA FROM SCHEDULE_YEAR WHERE SCHEDULE_ID=?",
        params = list(room_upper_id)
    )$DATA
    DBI::dbExecute(
        con,
        "UPDATE SCHEDULE_YEAR SET DATA=? WHERE SCHEDULE_ID=?",
        params = list(
            list(writeBin(rep(0.3, 8760), raw(), endian = "little")),
            room_upper_id
        )
    )
    # An invalid source pair takes priority over an unsupported device type.
    expect_error(
        hvac__air_treatment_source(con, system),
        "ROOM.*source hour 1",
        class = "destep_invalid_hvac_humidity_control"
    )
    DBI::dbExecute(
        con,
        "UPDATE SCHEDULE_YEAR SET DATA=? WHERE SCHEDULE_ID=?",
        params = list(room_upper_blob, room_upper_id)
    )
    expect_error(
        hvac__air_treatment_source(con, system),
        class = "destep_unsupported_hvac_humidifier"
    )
    DBI::dbExecute(con, "UPDATE AHU SET HUMIDIFIER=3")
    expect_error(
        hvac__air_treatment_source(con, system),
        class = "destep_unsupported_hvac_humidifier"
    )
    DBI::dbExecute(con, "UPDATE AHU SET HUMIDIFIER=2")
    active <- hvac__air_treatment_source(con, system)
    expect_identical(active$status, "native_electric_steam")
    expect_equal(active$controls$SET_RH_MIN_SCHEDULE, id)
    DBI::dbExecute(con, "UPDATE AHU SET HEAT_RECOVER=1")
    expect_error(
        hvac__air_treatment_source(con, system),
        class = "destep_unsupported_hvac_heat_recovery"
    )
    DBI::dbExecute(con, "UPDATE AHU SET HEAT_RECOVER=0, HUMIDIFIER=0")
    expect_warning(
        absent <- hvac__air_treatment_source(con, system),
        class = "destep_source_humidifier_absent"
    )
    expect_identical(absent$status, "absent")
    expect_identical(absent$target_type, "None")
    expect_identical(absent$humidity_control_status, "no_source_humidifier")
})

# Equipment selection cannot suppress source humidity constraints. Mutations
# use a private database and restore shared schedules after each scenario.
test_that("humidity capability checks apply with and without a humidifier", {
    skip_on_cran()
    path <- Sys.getenv("DESTEP_TEST_HVAC_SINGLE_SQLITE", unset = "")
    skip_if(!file.exists(path), "Archived single-zone AE source is required")
    original <- DBI::dbConnect(
        RSQLite::SQLite(),
        path,
        flags = RSQLite::SQLITE_RO
    )
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    RSQLite::sqliteCopyDatabase(original, con)
    DBI::dbDisconnect(original)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    system <- DBI::dbGetQuery(con, "SELECT AC_SYS_ID FROM AC_SYS")$AC_SYS_ID[[
        1L
    ]]
    controls <- hvac__conditioned_controls(con)
    room_lower <- controls$SET_RH_MIN_SCHEDULE[[1L]]
    room_upper <- controls$SET_RH_MAX_SCHEDULE[[1L]]
    supply_lower <- hvac__system_property(con, system, "AC_SYS_MINF_SCH")
    supply_upper <- hvac__system_property(con, system, "AC_SYS_MAXF_SCH")
    # Source schedules may be reused elsewhere; keep changes private to this
    # read-only-source fixture, and explicitly restore each changed trajectory.
    set_rh <- function(id, values) {
        DBI::dbExecute(
            con,
            "UPDATE SCHEDULE_YEAR SET DATA=? WHERE SCHEDULE_ID=?",
            params = list(
                list(writeBin(rep_len(values, 8760), raw(), endian = "little")),
                id
            )
        )
    }
    for (type in 0:3) {
        DBI::dbExecute(con, "UPDATE AHU SET HUMIDIFIER=?", params = list(type))
        set_rh(room_upper, 0.6)
        expect_identical(
            hvac__air_treatment_source(con, system)$dehumidification,
            "native_cooling_coil"
        )
        set_rh(room_upper, 1)
        set_rh(supply_upper, 0.8)
        expect_error(
            hvac__air_treatment_source(con, system),
            "supply-RH upper limit",
            class = "destep_unsupported_hvac_humidity_control"
        )
        set_rh(supply_upper, 1)
        set_rh(supply_lower, 0.2)
        if (type == 0L) {
            expect_warning(
                no_device <- hvac__air_treatment_source(con, system),
                class = "destep_source_humidifier_absent"
            )
            expect_identical(
                no_device$supply_humidity_control,
                "no_source_humidifier"
            )
        } else if (type == 2L) {
            expect_identical(
                hvac__air_treatment_source(con, system)$supply_humidity_control,
                "ems_minimum_rh"
            )
        } else {
            expect_error(
                hvac__air_treatment_source(con, system),
                class = "destep_unsupported_hvac_humidifier"
            )
        }
        set_rh(supply_lower, 0)
    }
    DBI::dbExecute(con, "UPDATE AHU SET HUMIDIFIER=0")
    expect_no_warning(absent <- hvac__air_treatment_source(con, system))
    expect_identical(absent$status, "absent")
    expect_identical(absent$humidity_control_status, "unrestricted")
    expect_identical(absent$dehumidification, "inactive_unrestricted_upper_rh")
    set_rh(room_lower, 0.7)
    set_rh(room_upper, 0.6)
    expect_error(
        hvac__air_treatment_source(con, system),
        class = "destep_invalid_hvac_humidity_control"
    )
    set_rh(room_lower, 0)
    set_rh(room_upper, 1)
    # Missing source properties remain errors even when equipment is absent.
    DBI::dbExecute(
        con,
        "UPDATE EXT_PROPERTY SET NAME='missing' WHERE NAME='AC_SYS_MAXF_SCH'"
    )
    expect_error(
        hvac__air_treatment_source(con, system),
        class = "destep_unresolved_hvac_property"
    )
})

# Verify the public conversion path inserts the humidifier into the physical
# branch and retains source RH controls, including through IDF transition.
test_that("electric humidity control is connected in the converted model", {
    skip_on_cran()
    path <- Sys.getenv("DESTEP_TEST_HVAC_SINGLE_SQLITE", unset = "")
    skip_if(!file.exists(path), "Archived single-zone AE source is required")
    original <- DBI::dbConnect(
        RSQLite::SQLite(),
        path,
        flags = RSQLite::SQLITE_RO
    )
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    RSQLite::sqliteCopyDatabase(original, con)
    DBI::dbDisconnect(original)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    id <- hvac__conditioned_controls(con)$SET_RH_MIN_SCHEDULE[[1L]]
    DBI::dbExecute(con, "UPDATE AHU SET HUMIDIFIER=2")
    DBI::dbExecute(
        con,
        "UPDATE SCHEDULE_YEAR SET DATA=? WHERE SCHEDULE_ID=?",
        params = list(
            list(writeBin(rep(0.4, 8760), raw(), endian = "little")),
            id
        )
    )
    upper_id <- hvac__conditioned_controls(con)$SET_RH_MAX_SCHEDULE[[1L]]
    DBI::dbExecute(
        con,
        "UPDATE SCHEDULE_YEAR SET DATA=? WHERE SCHEDULE_ID=?",
        params = list(
            list(writeBin(rep(0.6, 8760), raw(), endian = "little")),
            upper_id
        )
    )
    model <- suppressWarnings(to_idf(
        con,
        "9.1",
        options = destep_opts(run_period = c(1, 1))
    ))
    expect_true(model$is_valid())
    humidifiers <- model$objects_in_class("Humidifier:Steam:Electric")
    expect_length(humidifiers, 1L)
    expect_equal(
        unname(unlist(humidifiers[[1L]]$value("rated_capacity"))),
        "Autosize"
    )
    branch <- model$to_table(class = "Branch")
    expect_true(humidifiers[[1L]]$name() %in% branch$value)
    # Check directed connectivity, not just object presence: the humidifier
    # must precede the draw-through fan and leave the loop outlet unchanged.
    system <- paste0(
        "DeST AC_SYS ",
        attr(model, "conversion")$hvac$air_treatment[[1L]]$system_id
    )
    main <- model$object(paste(system, "Main Branch"))
    fan <- model$object(paste(system, "Supply Fan"))
    expect_equal(
        unname(unlist(main$value("component_5_name"))),
        humidifiers[[1L]]$name()
    )
    expect_equal(
        unname(unlist(main$value("component_6_name"))),
        fan$name()
    )
    expect_equal(
        unname(unlist(humidifiers[[1L]]$value("air_outlet_node_name"))),
        unname(unlist(fan$value("air_inlet_node_name")))
    )
    expect_equal(
        unname(unlist(main$value("component_6_outlet_node_name"))),
        unname(unlist(fan$value("air_outlet_node_name")))
    )
    expect_identical(
        attr(model, "conversion")$hvac$air_treatment[[1L]]$status,
        "native_electric_steam"
    )
    expect_identical(
        attr(model, "conversion")$hvac$air_treatment[[
            1L
        ]]$humidity_control_status,
        "single_zone_room_range"
    )
    # Both devices share one room humidistat, with distinct outlet feedback.
    expect_length(model$objects_in_class("ZoneControl:Humidistat"), 1L)
    expect_length(
        model$objects_in_class("SetpointManager:SingleZone:Humidity:Maximum"),
        1L
    )
    controller <- model$object(paste(system, "Cooling Coil Controller"))
    expect_equal(
        unname(unlist(controller$value("control_variable"))),
        "TemperatureAndHumidityRatio"
    )
    coil <- model$object(paste(system, "Cooling Coil"))
    dehumidifier <- model$object(paste(
        system,
        "Dehumidification Setpoint Manager"
    ))
    expect_equal(
        unname(unlist(dehumidifier$value("setpoint_node_or_nodelist_name"))),
        unname(unlist(coil$value("air_outlet_node_name")))
    )
    expect_equal(
        unname(unlist(controller$value("actuator_node_name"))),
        unname(unlist(coil$value("water_inlet_node_name")))
    )
    temp <- model$object(paste(system, "Cooling Supply Air Temp Manager"))
    expect_equal(
        unname(unlist(temp$value("minimum_supply_air_temperature"))),
        14
    )
    expect_equal(
        unname(unlist(temp$value("maximum_supply_air_temperature"))),
        32
    )
    transitioned <- conv__transition(model, numeric_version("23.1.0"))
    expect_true(transitioned$is_valid())
    expect_length(
        transitioned$objects_in_class("Humidifier:Steam:Electric"),
        1L
    )
    expect_equal(
        unname(unlist(transitioned$object(controller$name())$value(
            "control_variable"
        ))),
        "TemperatureAndHumidityRatio"
    )
    DBI::dbExecute(con, "UPDATE AHU SET HUMIDIFIER=0")
    without <- suppressWarnings(to_idf(
        con,
        "9.1",
        options = destep_opts(run_period = c(1, 1))
    ))
    expect_false("Humidifier:Steam:Electric" %in% without$class_name())
    expect_length(without$objects_in_class("ZoneControl:Humidistat"), 1L)
    expect_equal(
        unname(unlist(without$object(controller$name())$value(
            "control_variable"
        ))),
        "TemperatureAndHumidityRatio"
    )
    audit <- attr(without, "conversion")$hvac$air_treatment[[1L]]
    expect_identical(audit$humidity_control_status, "no_source_humidifier")
    expect_equal(audit$controls$SET_RH_MIN_SCHEDULE, id)
    expect_match(
        paste(
            without$objects_in_class("Version")[[1L]]$comment(),
            collapse = "\n"
        ),
        "humidity_control=no_source_humidifier"
    )
    expect_match(
        paste(
            without$objects_in_class("Version")[[1L]]$comment(),
            collapse = "\n"
        ),
        "native humidity override may supersede the temperature target"
    )
})

# Each source room contributes a separate demand to the shared native coil.
test_that("multizone dehumidification preserves room ownership and native controls", {
    skip_on_cran()
    path <- Sys.getenv("DESTEP_TEST_HVAC_MULTI_SQLITE", unset = "")
    skip_if(!file.exists(path), "Archived multizone AE source is required")
    original <- DBI::dbConnect(
        RSQLite::SQLite(),
        path,
        flags = RSQLite::SQLITE_RO
    )
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    RSQLite::sqliteCopyDatabase(original, con)
    DBI::dbDisconnect(original)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    controls <- hvac__conditioned_controls(con)
    expect_gt(nrow(controls), 1L)
    DBI::dbExecute(
        con,
        "UPDATE SCHEDULE_YEAR SET DATA=? WHERE SCHEDULE_ID=?",
        params = list(
            list(writeBin(rep(0.6, 8760), raw(), endian = "little")),
            controls$SET_RH_MAX_SCHEDULE[[1L]]
        )
    )
    DBI::dbExecute(con, "UPDATE AHU SET HUMIDIFIER=0")
    treatment <- hvac__air_treatment_source(con, controls$OF_AC_SYS[[1L]])
    expect_equal(treatment$humidity_control_status, "multizone_room_maximum")
    expect_equal(treatment$dehumidification, "native_cooling_coil")
    model <- suppressWarnings(to_idf(
        con,
        "9.1",
        options = destep_opts(run_period = c(1, 1))
    ))
    expect_true(model$is_valid())
    humidistats <- model$to_table(class = "ZoneControl:Humidistat", wide = TRUE)
    expect_setequal(humidistats[["Zone Name"]], controls$ROOM_NAME)
    expect_equal(nrow(humidistats), nrow(controls))
    manager <- model$to_table(
        class = "SetpointManager:MultiZone:Humidity:Maximum",
        wide = TRUE
    )
    expect_equal(nrow(manager), 1L)
    expect_equal(
        manager[["HVAC Air Loop Name"]],
        paste0("DeST AC_SYS ", treatment$system_id)
    )
    expect_equal(as.numeric(manager[["Minimum Setpoint Humidity Ratio"]]), 1e-9)
    expect_equal(as.numeric(manager[["Maximum Setpoint Humidity Ratio"]]), 1)
    expect_false("Humidifier:Steam:Electric" %in% model$class_name())
    expect_false(
        "SetpointManager:SingleZone:Humidity:Maximum" %in% model$class_name()
    )
    controller <- model$object(paste0(
        "DeST AC_SYS ",
        treatment$system_id,
        " Cooling Coil Controller"
    ))
    expect_equal(
        unname(unlist(controller$value("control_variable"))),
        "TemperatureAndHumidityRatio"
    )

    # A combined range keeps one humidistat per room and independent native
    # minimum/maximum managers; neither control creates a second cooling coil.
    DBI::dbExecute(con, "UPDATE AHU SET HUMIDIFIER=2")
    DBI::dbExecute(
        con,
        "UPDATE SCHEDULE_YEAR SET DATA=? WHERE SCHEDULE_ID=?",
        params = list(
            list(writeBin(rep(.3, 8760), raw(), endian = "little")),
            controls$SET_RH_MIN_SCHEDULE[[1L]]
        )
    )
    treatment <- hvac__air_treatment_source(con, controls$OF_AC_SYS[[1L]])
    expect_equal(treatment$humidity_control_status, "multizone_room_range")
    combined <- suppressWarnings(to_idf(
        con,
        "9.1",
        options = destep_opts(run_period = c(1, 1))
    ))
    expect_true(combined$is_valid())
    expect_length(
        combined$objects_in_class("ZoneControl:Humidistat"),
        nrow(controls)
    )
    expect_length(
        combined$objects_in_class("SetpointManager:MultiZone:Humidity:Maximum"),
        1L
    )
    expect_length(
        combined$objects_in_class("SetpointManager:MultiZone:Humidity:Minimum"),
        1L
    )
    expect_length(combined$objects_in_class("Coil:Cooling:Water"), 1L)
    expect_length(combined$objects_in_class("Humidifier:Steam:Electric"), 1L)
})

# Exercise source-room ownership through public conversion, not hand-added IDF.
test_that("shared CAV humidification keeps every room's source schedule", {
    skip_on_cran()
    path <- Sys.getenv("DESTEP_TEST_HVAC_MULTI_SQLITE", unset = "")
    skip_if(!file.exists(path), "Archived multizone AE source is required")
    original <- DBI::dbConnect(
        RSQLite::SQLite(),
        path,
        flags = RSQLite::SQLITE_RO
    )
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    RSQLite::sqliteCopyDatabase(original, con)
    DBI::dbDisconnect(original)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    controls <- hvac__conditioned_controls(con)
    DBI::dbExecute(con, "UPDATE AHU SET HUMIDIFIER=2")
    template <- DBI::dbGetQuery(
        con,
        "SELECT * FROM SCHEDULE_YEAR WHERE SCHEDULE_ID=?",
        params = list(controls$SET_RH_MIN_SCHEDULE[[1L]])
    )
    last <- DBI::dbGetQuery(
        con,
        "SELECT MAX(SCHEDULE_ID) AS ID FROM SCHEDULE_YEAR"
    )$ID
    for (i in seq_len(nrow(controls))) {
        row <- template
        row$SCHEDULE_ID <- as.integer(last + i)
        row$NAME <- paste("Shared RH", i)
        row$DATA <- list(writeBin(
            rep(0.2 + i * 0.1, 8760L),
            raw(),
            endian = "little"
        ))
        DBI::dbAppendTable(con, "SCHEDULE_YEAR", row)
        DBI::dbExecute(
            con,
            "UPDATE ROOM_TYPE_DATA SET SET_RH_MIN_SCHEDULE=? WHERE ID=?",
            params = list(row$SCHEDULE_ID, controls$ROOM_TYPE_DATA_ID[[i]])
        )
    }
    treatment <- hvac__air_treatment_source(con, controls$OF_AC_SYS[[1L]])
    expect_equal(treatment$humidity_control_status, "multizone_room_minimum")
    model <- suppressWarnings(to_idf(
        con,
        "9.1",
        options = destep_opts(run_period = c(1, 1))
    ))
    expect_true(model$is_valid())
    humidistats <- model$to_table(class = "ZoneControl:Humidistat", wide = TRUE)
    expect_equal(nrow(humidistats), nrow(controls))
    for (i in seq_len(nrow(controls))) {
        zone <- humidistats[
            humidistats[["Zone Name"]] == controls$ROOM_NAME[[i]]
        ]
        expect_equal(
            zone[["Humidifying Relative Humidity Setpoint Schedule Name"]],
            paste("Shared RH", i)
        )
    }
    manager <- model$to_table(
        class = "SetpointManager:MultiZone:Humidity:Minimum",
        wide = TRUE
    )
    expect_equal(nrow(manager), 1L)
    expect_equal(as.numeric(manager[["Minimum Setpoint Humidity Ratio"]]), 1e-9)
    expect_equal(as.numeric(manager[["Maximum Setpoint Humidity Ratio"]]), 1)
    expect_length(model$objects_in_class("Humidifier:Steam:Electric"), 1L)
    expect_false(
        "SetpointManager:SingleZone:Humidity:Minimum" %in% model$class_name()
    )
    # Positive supply-RH requirements remain outside this shared-room subset.
    DBI::dbExecute(
        con,
        "UPDATE SCHEDULE_YEAR SET DATA=? WHERE SCHEDULE_ID=?",
        params = list(
            list(writeBin(rep(.1, 8760L), raw(), endian = "little")),
            treatment$supply_minimum_rh_schedule_id
        )
    )
    expect_error(
        hvac__air_treatment_source(con, controls$OF_AC_SYS[[1L]]),
        "multizone supply-RH",
        class = "destep_unsupported_hvac_humidity_control"
    )
})

# Linked-only and shared RH schedules must resolve in the correct target units;
# the public converter must retain an otherwise unreferenced extension schedule.
test_that("supply RH lower bounds use source schedules and existing humidifiers", {
    skip_on_cran()
    path <- Sys.getenv("DESTEP_TEST_HVAC_SINGLE_SQLITE", unset = "")
    skip_if(!file.exists(path), "Archived single-zone AE source is required")
    original <- DBI::dbConnect(
        RSQLite::SQLite(),
        path,
        flags = RSQLite::SQLITE_RO
    )
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    RSQLite::sqliteCopyDatabase(original, con)
    DBI::dbDisconnect(original)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    controls <- hvac__conditioned_controls(con)
    system_id <- controls$OF_AC_SYS[[1L]]
    lower_id <- hvac__system_property(con, system_id, "AC_SYS_MINF_SCH")
    row <- DBI::dbGetQuery(
        con,
        "SELECT * FROM SCHEDULE_YEAR WHERE SCHEDULE_ID=?",
        params = list(lower_id)
    )
    id <- as.integer(
        DBI::dbGetQuery(
            con,
            "SELECT MAX(SCHEDULE_ID)+1 AS ID FROM SCHEDULE_YEAR"
        )$ID
    )
    row$SCHEDULE_ID <- id
    row$NAME <- "Supply RH linked only"
    row$DATA <- list(writeBin(rep(0.3, 8760L), raw(), endian = "little"))
    DBI::dbAppendTable(con, "SCHEDULE_YEAR", row)
    DBI::dbExecute(
        con,
        "UPDATE EXT_PROPERTY SET DATA_LONG=? WHERE NAME='AC_SYS_MINF_SCH'",
        params = list(id)
    )
    DBI::dbExecute(con, "UPDATE AHU SET HUMIDIFIER=2")
    expect_true(id %in% hvac__supply_rh_schedule_ids(con, system_id))
    expect_equal(hvac__supply_rh_reference(con, id)$divisor, 1)
    source <- hvac__air_treatment_source(con, system_id)
    expect_identical(source$supply_humidity_control, "ems_minimum_rh")
    expect_identical(source$status, "native_electric_steam")
    model <- suppressWarnings(to_idf(
        con,
        "9.1",
        options = destep_opts(run_period = c(1, 1))
    ))
    expect_true(model$is_valid())
    prefix <- paste0("DeST_Supply_RH_", system_id)
    sensor <- model$object(paste0(prefix, "_RH"))
    schedule_name <- unname(unlist(sensor$value(
        "output_variable_or_output_meter_index_key_name"
    )))
    expect_identical(
        model$object(schedule_name)$class_name(),
        "Schedule:Compact"
    )
    program <- paste(
        model$object(paste0(prefix, "_Convert"))$value(),
        collapse = " "
    )
    expect_match(program, paste0(prefix, "_RH / 1"), fixed = TRUE)
    expect_match(program, "@WFnTdbRhPb", fixed = TRUE)
    expect_match(program, "IF Fraction > 0", fixed = TRUE)
    manager <- model$object(paste0(prefix, "_Iterations"))
    expect_equal(
        unname(unlist(manager$value("energyplus_model_calling_point"))),
        "InsideHVACSystemIterationLoop"
    )
    expect_length(model$objects_in_class("Humidifier:Steam:Electric"), 1L)
    expect_equal(
        unname(unlist(model$objects_in_class("Humidifier:Steam:Electric")[[
            1L
        ]]$value("rated_capacity"))),
        "Autosize"
    )
    # Sharing solely with a room RH bound makes the original target schedule
    # a percent schedule. Sharing with an ordinary schedule retains fractions.
    DBI::dbExecute(
        con,
        "UPDATE ROOM_TYPE_DATA SET SET_RH_MIN_SCHEDULE=?",
        params = list(id)
    )
    expect_equal(hvac__supply_rh_reference(con, id)$divisor, 100)
    DBI::dbExecute(con, "CREATE TABLE PROBE_REFERENCE (OTHER_SCHEDULE INTEGER)")
    DBI::dbExecute(
        con,
        "INSERT INTO PROBE_REFERENCE VALUES (?)",
        params = list(id)
    )
    expect_equal(hvac__supply_rh_reference(con, id)$divisor, 1)
})

# Room humidity controls must leave the source VAV flow limits and topology intact.
test_that("VAV humidification preserves flow limits and rejects active dehumidification", {
    skip_on_cran()
    path <- Sys.getenv("DESTEP_TEST_HVAC_VAV_SQLITE", unset = "")
    skip_if(!file.exists(path), "Archived VAV AE source is required")
    original <- DBI::dbConnect(
        RSQLite::SQLite(),
        path,
        flags = RSQLite::SQLITE_RO
    )
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    RSQLite::sqliteCopyDatabase(original, con)
    DBI::dbDisconnect(original)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    descriptor <- suppressWarnings(hvac__system_sources(con))[[1L]]
    expect_equal(descriptor$path, "multizone_vav")
    controls <- hvac__conditioned_controls(con)
    DBI::dbExecute(con, "UPDATE AHU SET HUMIDIFIER=2")
    row <- DBI::dbGetQuery(
        con,
        "SELECT * FROM SCHEDULE_YEAR WHERE SCHEDULE_ID=?",
        params = list(controls$SET_RH_MIN_SCHEDULE[[1L]])
    )
    last <- DBI::dbGetQuery(
        con,
        "SELECT MAX(SCHEDULE_ID) AS ID FROM SCHEDULE_YEAR"
    )$ID
    # Four source schedules make accidental first-room reuse observable.
    for (i in seq_len(nrow(controls))) {
        for (role in c("MIN", "MAX")) {
            last <- last + 1L
            row$SCHEDULE_ID <- as.integer(last)
            row$NAME <- paste("VAV test RH", role, i)
            target <- if (role == "MIN") .2 + i * .1 else 1
            row$DATA <- list(writeBin(
                rep(target, 8760L),
                raw(),
                endian = "little"
            ))
            DBI::dbAppendTable(con, "SCHEDULE_YEAR", row)
            DBI::dbExecute(
                con,
                paste0(
                    "UPDATE ROOM_TYPE_DATA SET SET_RH_",
                    role,
                    "_SCHEDULE=? WHERE ID=?"
                ),
                params = list(row$SCHEDULE_ID, controls$ROOM_TYPE_DATA_ID[[i]])
            )
        }
    }
    model <- suppressWarnings(to_idf(
        con,
        "9.1",
        options = destep_opts(run_period = c(1, 1))
    ))
    expect_true(model$is_valid())
    expect_length(model$objects_in_class("Humidifier:Steam:Electric"), 1L)
    expect_length(model$objects_in_class("Coil:Cooling:Water"), 1L)
    system <- paste0("DeST AC_SYS ", controls$OF_AC_SYS[[1L]])
    for (role in "Minimum") {
        manager <- model$to_table(
            class = paste0("SetpointManager:MultiZone:Humidity:", role),
            wide = TRUE
        )
        expect_equal(nrow(manager), 1L)
        expect_equal(manager[["HVAC Air Loop Name"]], system)
        expect_equal(
            as.numeric(manager[["Minimum Setpoint Humidity Ratio"]]),
            1e-9
        )
        expect_equal(
            as.numeric(manager[["Maximum Setpoint Humidity Ratio"]]),
            1
        )
    }
    stats <- model$to_table(class = "ZoneControl:Humidistat", wide = TRUE)
    expect_equal(nrow(stats), nrow(controls))
    for (i in seq_len(nrow(controls))) {
        selected <- stats[stats[["Zone Name"]] == controls$ROOM_NAME[[i]]]
        expect_equal(
            selected[["Humidifying Relative Humidity Setpoint Schedule Name"]],
            paste("VAV test RH MIN", i)
        )
        expect_equal(
            selected[[
                "Dehumidifying Relative Humidity Setpoint Schedule Name"
            ]],
            paste("VAV test RH MAX", i)
        )
    }
    terminals <- model$to_table(
        class = "AirTerminal:SingleDuct:VAV:Reheat",
        wide = TRUE
    )
    zones <- descriptor$source$zones
    indices <- match(paste(zones$zone_name, "VAV Reheat"), terminals$Name)
    expect_false(anyNA(indices))
    expect_equal(
        as.numeric(terminals[["Maximum Air Flow Rate"]][indices]),
        zones$maximum_supply_flow_m3_s
    )
    expect_equal(
        as.numeric(terminals[["Fixed Minimum Air Flow Rate"]][indices]),
        zones$minimum_supply_flow_m3_s
    )
    expect_true(all(
        zones$minimum_supply_flow_m3_s < zones$maximum_supply_flow_m3_s
    ))
    expect_false(
        "SetpointManager:MultiZone:Humidity:Maximum" %in% model$class_name()
    )
    # Failed native VAV dehumidification probes must not be exposed as support.
    updated <- hvac__conditioned_controls(con)
    upper_id <- updated$SET_RH_MAX_SCHEDULE[[1L]]
    DBI::dbExecute(
        con,
        "UPDATE SCHEDULE_YEAR SET DATA=? WHERE SCHEDULE_ID=?",
        params = list(
            list(writeBin(rep(.6, 8760L), raw(), endian = "little")),
            upper_id
        )
    )
    expect_error(
        suppressWarnings(to_idf(
            con,
            "9.1",
            options = destep_opts(run_period = c(1, 1))
        )),
        class = "destep_unsupported_hvac_dehumidification"
    )
    DBI::dbExecute(
        con,
        "UPDATE SCHEDULE_YEAR SET DATA=? WHERE SCHEDULE_ID=?",
        params = list(
            list(writeBin(rep(1, 8760L), raw(), endian = "little")),
            upper_id
        )
    )
    # A source supply-RH restriction remains a separate unsupported control.
    id <- hvac__system_property(
        con,
        controls$OF_AC_SYS[[1L]],
        "AC_SYS_MINF_SCH"
    )
    DBI::dbExecute(
        con,
        "UPDATE SCHEDULE_YEAR SET DATA=? WHERE SCHEDULE_ID=?",
        params = list(
            list(writeBin(rep(.2, 8760L), raw(), endian = "little")),
            id
        )
    )
    expect_error(
        hvac__air_treatment_source(con, controls$OF_AC_SYS[[1L]]),
        class = "destep_unsupported_hvac_humidity_control"
    )
})
