# Build a compact ROOM/ROOM_GROUP/SCHEDULE_YEAR fixture for thermostat tests
# without pulling in the full schedule BLOB conversion path.
destep_test_thermostat_db <- function() {
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

    DBI::dbWriteTable(dest, "ROOM", data.frame(
        ID = c(1L, 2L, 3L),
        NAME = c("Room 1", "Room 2", "Room 3"),
        OF_ROOM_GROUP = c(10L, 20L, 30L)
    ))
    DBI::dbWriteTable(dest, "ROOM_GROUP", data.frame(
        ROOM_GROUP_ID = c(10L, 20L, 30L),
        NAME = c("Group 1", "Group 2", "Group 3"),
        IS_AC_ROOM = c(1L, 0L, 1L),
        AC_SCHEDULE_ID = c(500L, 500L, 501L),
        SET_T_MIN_SCHEDULE = c(100L, 100L, 101L),
        SET_T_MAX_SCHEDULE = c(200L, 200L, 201L)
    ))
    DBI::dbWriteTable(dest, "SCHEDULE_YEAR", data.frame(
        SCHEDULE_ID = c(100L, 101L, 200L, 201L, 500L, 501L),
        NAME = c(
            "Heating 18C", "Heating 20C",
            "Cooling 26C", "Cooling 24C",
            "AC On A", "AC On B"
        )
    ))

    dest
}

test_that("can convert ROOM_GROUP thermostat setpoints with shared dual setpoints", {
    ep <- ensure_empty_idf()
    dest <- destep_test_thermostat_db()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    thermostat <- destep_conv_thermostat(dest, ep)
    tab <- attr(thermostat, "table")
    value <- thermostat$value

    expect_equal(sum(thermostat$object$class_name == "Schedule:Constant"), 1L)
    expect_equal(sum(thermostat$object$class_name == "ThermostatSetpoint:DualSetpoint"), 2L)
    expect_equal(sum(thermostat$object$class_name == "ZoneControl:Thermostat"), 2L)

    expect_equal(tab$CAN_CONVERT, c(TRUE, FALSE, TRUE))
    expect_equal(
        tab$ENERGYPLUS_SETPOINT_NAME,
        c(
            "DeST Dual Setpoint H100 C200",
            "DeST Dual Setpoint H100 C200",
            "DeST Dual Setpoint H101 C201"
        )
    )
    expect_equal(sum(tab$IS_AC_ROOM == 0L), 1L)

    expect_equal(
        value$value_num[
            value$class_name == "Schedule:Constant" &
                value$field_name == "Hourly Value"
        ],
        4
    )
    expect_setequal(
        value$value_chr[
            value$class_name == "ThermostatSetpoint:DualSetpoint" &
                value$field_name == "Heating Setpoint Temperature Schedule Name"
        ],
        c("Heating 18C", "Heating 20C")
    )
    expect_setequal(
        value$value_chr[
            value$class_name == "ThermostatSetpoint:DualSetpoint" &
                value$field_name == "Cooling Setpoint Temperature Schedule Name"
        ],
        c("Cooling 26C", "Cooling 24C")
    )
    expect_true(all(
        value$value_chr[
            value$class_name == "ZoneControl:Thermostat" &
                value$field_name == "Control Type Schedule Name"
        ] == "DeST Dual Setpoint Control Type"
    ))
})

test_that("stops when ROOM_GROUP setpoint schedules cannot be resolved", {
    ep <- ensure_empty_idf()
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(dest, "ROOM", data.frame(
        ID = 1L,
        NAME = "Room 1",
        OF_ROOM_GROUP = 10L
    ))
    DBI::dbWriteTable(dest, "ROOM_GROUP", data.frame(
        ROOM_GROUP_ID = 10L,
        NAME = "Group 1",
        IS_AC_ROOM = 1L,
        AC_SCHEDULE_ID = 500L,
        SET_T_MIN_SCHEDULE = 999L,
        SET_T_MAX_SCHEDULE = 200L
    ))
    DBI::dbWriteTable(dest, "SCHEDULE_YEAR", data.frame(
        SCHEDULE_ID = 200L,
        NAME = "Cooling 26C"
    ))

    expect_error(
        destep_conv_thermostat(dest, ep),
        "Cannot resolve ROOM_GROUP thermostat schedule"
    )
})

test_that("skips rooms without complete ROOM_GROUP setpoints", {
    ep <- ensure_empty_idf()
    dest <- destep_test_thermostat_db()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbExecute(dest, "
        UPDATE ROOM_GROUP
        SET SET_T_MIN_SCHEDULE = 0
        WHERE ROOM_GROUP_ID = 20
    ")

    expect_warning(
        thermostat <- destep_conv_thermostat(dest, ep),
        "Skipped 1 ROOM row"
    )

    tab <- attr(thermostat, "table")
    expect_equal(sum(tab$CAN_CONVERT), 2L)
    expect_equal(sum(thermostat$object$class_name == "ZoneControl:Thermostat"), 2L)
})

test_that("can convert ROOM_GROUP thermostat setpoints from a real DeST model", {
    skip_on_cran()

    ep <- ensure_empty_idf()
    src <- ensure_dest_sqlite_file()
    on.exit(DBI::dbDisconnect(src), add = TRUE)

    path_tmp <- tempfile(fileext = ".sql")
    dest <- DBI::dbConnect(RSQLite::SQLite(), path_tmp)
    on.exit({
        DBI::dbDisconnect(dest)
        unlink(path_tmp)
    }, add = TRUE)
    RSQLite::sqliteCopyDatabase(src, dest)
    destep_update_name(dest)

    thermostat <- destep_conv_thermostat(dest, ep)
    tab <- attr(thermostat, "table")

    expect_equal(sum(tab$CAN_CONVERT), 27L)
    expect_equal(sum(!tab$CAN_CONVERT & tab$IS_AC_ROOM == 0L), 9L)
    expect_equal(
        unique(tab$ENERGYPLUS_SETPOINT_NAME),
        "DeST Dual Setpoint H10 C7"
    )
    expect_equal(sum(thermostat$object$class_name == "ThermostatSetpoint:DualSetpoint"), 1L)
    expect_equal(sum(thermostat$object$class_name == "ZoneControl:Thermostat"), 27L)
})

test_that("to_eplus() includes resolvable thermostat references", {
    skip_on_cran()

    src <- ensure_dest_sqlite_file()
    on.exit(DBI::dbDisconnect(src), add = TRUE)

    idf <- to_eplus(src, 23.1)
    control <- idf$to_table(class = "ZoneControl:Thermostat", all = TRUE)
    setpoint <- idf$to_table(class = "ThermostatSetpoint:DualSetpoint", all = TRUE)
    constant <- idf$to_table(class = "Schedule:Constant", all = TRUE)
    year <- idf$to_table(class = "Schedule:Year", all = TRUE)

    control_names <- control$value[control$field == "Name"]
    setpoint_names <- setpoint$value[setpoint$field == "Name"]
    constant_names <- constant$value[constant$field == "Name"]
    year_names <- year$value[year$field == "Name"]

    expect_equal(length(control_names), 27L)
    expect_equal(length(setpoint_names), 1L)
    expect_true("DeST Dual Setpoint Control Type" %in% constant_names)
    expect_true(all(
        control$value[control$field == "Control Type Schedule Name"] %in%
            constant_names
    ))
    expect_true(all(
        control$value[control$field == "Control 1 Name"] %in%
            setpoint_names
    ))
    expect_true(all(
        setpoint$value[
            setpoint$field %in% c(
                "Heating Setpoint Temperature Schedule Name",
                "Cooling Setpoint Temperature Schedule Name"
            )
        ] %in% year_names
    ))
    expect_true(idf$is_valid())
})
