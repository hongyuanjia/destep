# Build a compact ROOM/ROOM_GROUP/SCHEDULE_YEAR fixture that exercises ideal
# loads conversion without requiring the full schedule BLOB pipeline.
destep_test_ideal_loads_db <- function() {
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

    DBI::dbWriteTable(dest, "ROOM", data.frame(
        ID = c(1L, 2L, 3L),
        NAME = c("Room 1", "Room 2", "Room 3"),
        OF_ROOM_GROUP = c(10L, 20L, 30L)
    ))
    DBI::dbWriteTable(dest, "ROOM_GROUP", data.frame(
        ROOM_GROUP_ID = c(10L, 20L, 30L),
        NAME = c("Group 1", "Group 2", "Group 3"),
        OF_AC_SYS = c(0L, 0L, 0L),
        IS_AC_ROOM = c(1L, 0L, 1L),
        AC_SCHEDULE_ID = c(100L, 100L, 101L),
        SET_RH_MIN_SCHEDULE = c(0L, 0L, 0L),
        SET_RH_MAX_SCHEDULE = c(0L, 0L, 0L),
        AC_T_MIN_SCHEDULE = c(300L, 300L, 301L),
        AC_T_MAX_SCHEDULE = c(400L, 400L, 401L)
    ))
    DBI::dbWriteTable(dest, "SCHEDULE_YEAR", data.frame(
        SCHEDULE_ID = c(100L, 101L),
        NAME = c("AC Weekday", "AC Weekend")
    ))

    dest
}

test_that("can convert ROOM_GROUP ideal loads for air-conditioned rooms", {
    ep <- ensure_empty_idf()
    dest <- destep_test_ideal_loads_db()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    expect_warning(
        ideal <- destep_conv_ideal_loads(dest, ep),
        NA
    )
    tab <- attr(ideal, "table")
    value <- ideal$value

    expect_equal(sum(ideal$object$class_name == "ZoneHVAC:IdealLoadsAirSystem"), 2L)
    expect_equal(sum(ideal$object$class_name == "ZoneHVAC:EquipmentList"), 2L)
    expect_equal(sum(ideal$object$class_name == "ZoneHVAC:EquipmentConnections"), 2L)
    expect_equal(tab$CAN_CONVERT, c(TRUE, FALSE, TRUE))
    expect_equal(tab$SKIP_REASON[[2L]], "ROOM_GROUP.IS_AC_ROOM is zero")

    expect_setequal(
        value$value_chr[
            value$class_name == "ZoneHVAC:IdealLoadsAirSystem" &
                value$field_name == "Availability Schedule Name"
        ],
        c("AC Weekday", "AC Weekend")
    )
    expect_equal(
        value$value_num[
            value$class_name == "ZoneHVAC:IdealLoadsAirSystem" &
                value$field_name == "Maximum Heating Supply Air Temperature"
        ],
        c(50, 50)
    )
    expect_equal(
        value$value_num[
            value$class_name == "ZoneHVAC:IdealLoadsAirSystem" &
                value$field_name == "Minimum Cooling Supply Air Temperature"
        ],
        c(13, 13)
    )
    expect_true(all(
        value$value_chr[
            value$class_name == "ZoneHVAC:EquipmentList" &
                value$field_name == "Zone Equipment 1 Object Type"
        ] == "ZoneHVAC:IdealLoadsAirSystem"
    ))
})

test_that("stops when ROOM_GROUP AC schedules cannot be resolved", {
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
        OF_AC_SYS = 0L,
        IS_AC_ROOM = 1L,
        AC_SCHEDULE_ID = 999L,
        SET_RH_MIN_SCHEDULE = 0L,
        SET_RH_MAX_SCHEDULE = 0L,
        AC_T_MIN_SCHEDULE = 0L,
        AC_T_MAX_SCHEDULE = 0L
    ))
    DBI::dbWriteTable(dest, "SCHEDULE_YEAR", data.frame(
        SCHEDULE_ID = integer(),
        NAME = character()
    ))

    expect_error(
        destep_conv_ideal_loads(dest, ep),
        "Cannot resolve ROOM_GROUP ideal-loads schedule"
    )
})

test_that("skips air-conditioned rooms without availability schedules", {
    ep <- ensure_empty_idf()
    dest <- destep_test_ideal_loads_db()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbExecute(dest, "
        UPDATE ROOM_GROUP
        SET AC_SCHEDULE_ID = 0
        WHERE ROOM_GROUP_ID = 30
    ")

    expect_warning(
        ideal <- destep_conv_ideal_loads(dest, ep),
        "Skipped 1 ROOM row"
    )

    tab <- attr(ideal, "table")
    expect_equal(sum(tab$CAN_CONVERT), 1L)
    expect_equal(sum(ideal$object$class_name == "ZoneHVAC:IdealLoadsAirSystem"), 1L)
})

test_that("can convert ROOM_GROUP ideal loads from a real DeST model", {
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

    expect_warning(
        ideal <- destep_conv_ideal_loads(dest, ep),
        NA
    )
    tab <- attr(ideal, "table")

    expect_equal(sum(tab$CAN_CONVERT), 27L)
    expect_equal(sum(!tab$CAN_CONVERT & tab$IS_AC_ROOM == 0L), 9L)
    expect_equal(
        unique(tab$AC_SCHEDULE_NAME[tab$CAN_CONVERT]),
        "办公室空调启停作息-加班4h"
    )
    expect_equal(sum(ideal$object$class_name == "ZoneHVAC:IdealLoadsAirSystem"), 27L)
    expect_equal(sum(ideal$object$class_name == "ZoneHVAC:EquipmentList"), 27L)
    expect_equal(sum(ideal$object$class_name == "ZoneHVAC:EquipmentConnections"), 27L)
})

test_that("to_eplus() includes resolvable ideal loads references", {
    skip_on_cran()

    src <- ensure_dest_sqlite_file()
    on.exit(DBI::dbDisconnect(src), add = TRUE)

    idf <- to_eplus(src, 23.1)
    ideal <- idf$to_table(class = "ZoneHVAC:IdealLoadsAirSystem", all = TRUE)
    equipment <- idf$to_table(class = "ZoneHVAC:EquipmentList", all = TRUE)
    connection <- idf$to_table(class = "ZoneHVAC:EquipmentConnections", all = TRUE)
    year <- idf$to_table(class = "Schedule:Year", all = TRUE)

    ideal_names <- ideal$value[ideal$field == "Name"]
    equipment_names <- equipment$value[equipment$field == "Name"]
    year_names <- year$value[year$field == "Name"]

    expect_equal(length(ideal_names), 27L)
    expect_equal(length(equipment_names), 27L)
    expect_equal(length(connection$value[connection$field == "Zone Name"]), 27L)
    expect_true(all(
        ideal$value[ideal$field == "Availability Schedule Name"] %in% year_names
    ))
    expect_true(all(
        equipment$value[equipment$field == "Zone Equipment 1 Name"] %in%
            ideal_names
    ))
    expect_true(all(
        connection$value[
            connection$field == "Zone Conditioning Equipment List Name"
        ] %in% equipment_names
    ))
    expect_true(idf$is_valid())
})
