test_that("can convert ROOM_RELATION outdoor ventilation", {
    ep <- eplusr::empty_idf(23.1)
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(
        dest,
        "ROOM",
        data.frame(
            ID = c(1L, 2L),
            NAME = c("Room 101", "Room 102"),
            OF_ROOM_GROUP = c(11L, 12L),
            TYPE = c(1L, 2L)
        )
    )
    DBI::dbWriteTable(
        dest,
        "ROOM_GROUP",
        data.frame(ROOM_GROUP_ID = c(11L, 12L), IS_AC_ROOM = c(1L, 0L))
    )
    DBI::dbWriteTable(
        dest,
        "OUTSIDE",
        data.frame(
            OUTSIDE_ID = 10L,
            NAME = "DefaultOutside"
        )
    )
    DBI::dbWriteTable(
        dest,
        "SCHEDULE_YEAR",
        data.frame(
            SCHEDULE_ID = c(20L, 22L, 30L, 31L),
            NAME = c(
                "Ventilation 0.5 ACH",
                "Ventilation 10 ACH",
                "Heating Setpoint",
                "Cooling Setpoint"
            ),
            TYPE = c(4L, 4L, 4L, 4L),
            DATA = I(list(
                destep_test_schedule_blob(rep(0.5, 8760L)),
                destep_test_schedule_blob(rep(10, 8760L)),
                destep_test_schedule_blob(rep(18, 8760L)),
                destep_test_schedule_blob(rep(26, 8760L))
            ))
        )
    )
    DBI::dbWriteTable(
        dest,
        "ROOM_TYPE_DATA",
        data.frame(
            ID = c(1L, 2L),
            SET_T_MIN_SCHEDULE = 30L,
            SET_T_MAX_SCHEDULE = 31L
        )
    )
    DBI::dbWriteTable(
        dest,
        "ROOM_RELATION",
        data.frame(
            ID = c(100L, 101L),
            NAME = c(".", "Custom Ventilation"),
            OF_BUILDING = 1L,
            ROOM_ID = c(1L, 2L),
            RELA_ROOM_ID = 10L,
            VENT_SCHEDULE_ID = 20L,
            VENT_SET_MAX = c(22, 10),
            VENT_TYPE = c(1L, 0L),
            START_POINT_ID = 0L,
            END_POINT_ID = 0L,
            EXT_PROPERTY = 0L
        )
    )

    schedule <- schedule__convert(dest, ep)
    expect_warning(
        ventilation <- ventilation__convert(dest, ep),
        "documented outdoor-temperature-band rule"
    )

    expect_type(ventilation, "list")
    expect_named(ventilation, c("object", "value"))
    expect_equal(
        unique(ventilation$object$class_name),
        "ZoneVentilation:DesignFlowRate"
    )
    expect_s3_class(attr(ventilation, "table"), "data.table")
    expect_equal(
        attr(ventilation, "table")$ENERGYPLUS_NAME,
        c("Room 101 Outdoor Ventilation", "Custom Ventilation")
    )
    expect_equal(
        ventilation$value$value_chr[
            ventilation$value$field_name == "Schedule Name"
        ],
        c(
            rep("Ventilation 0.5 ACH", 2L),
            "DeST Derived Ventilation Range 22 Minus 20"
        )
    )
    expect_equal(
        ventilation$value$value_num[
            ventilation$value$field_name == "Air Changes per Hour"
        ],
        c(1, 1, 9.5)
    )
    expect_equal(
        attr(ventilation, "table")[VENT_TYPE == 1L, MAX_SCHEDULE_NAME],
        "Ventilation 10 ACH"
    )
    expect_true(
        attr(ventilation, "table")[VENT_TYPE == 1L, RANGE_CONTROL_CONVERTED]
    )
    expect_equal(
        attr(ventilation, "table")[
            VENT_TYPE == 1L,
            RANGE_CONTROL_FIDELITY
        ],
        "documented_rule_not_solver_equivalent"
    )
    expect_false(
        attr(ventilation, "table")[
            VENT_TYPE == 1L,
            HVAC_AVAILABILITY_GATED
        ]
    )
    expect_equal(
        ventilation$value$value_chr[
            ventilation$value$field_name ==
                "Minimum Outdoor Temperature Schedule Name"
        ],
        "Heating Setpoint"
    )
    expect_equal(
        ventilation$value$value_chr[
            ventilation$value$field_name ==
                "Maximum Outdoor Temperature Schedule Name"
        ],
        "Cooling Setpoint"
    )
    expect_true(
        "DeST Derived Ventilation Range 22 Minus 20" %in%
            attr(schedule, "table")$NAME
    )
    expect_equal(
        unique(attr(schedule, "table")[
            NAME == "DeST Derived Ventilation Range 22 Minus 20"
        ]$DATA[[1L]]),
        1
    )

    # A non-AC zone keeps the minimum even when its maximum and temperature
    # schedules would otherwise allow the supplement. Fixed ventilation is
    # independent of this flag and must remain present for the second room.
    DBI::dbExecute(
        dest,
        "UPDATE ROOM_GROUP SET IS_AC_ROOM = 0 WHERE ROOM_GROUP_ID = 11"
    )
    non_ac_ep <- eplusr::empty_idf(23.1)
    non_ac_schedules <- schedule__convert(dest, non_ac_ep)
    expect_no_warning(non_ac <- ventilation__convert(dest, non_ac_ep))
    expect_equal(nrow(non_ac$object), 2L)
    expect_equal(
        non_ac$value$value_chr[non_ac$value$field_name == "Schedule Name"],
        rep("Ventilation 0.5 ACH", 2L)
    )
    expect_equal(
        attr(non_ac, "table")$RANGE_CONTROL_METHOD,
        c("non_air_conditioned_minimum_only", "fixed_schedule")
    )
    expect_false(any(grepl(
        "DeST Derived Ventilation",
        attr(non_ac_schedules, "table")$NAME
    )))
    expect_false(any(attr(non_ac, "table")$HVAC_AVAILABILITY_GATED))

    # A shared min/max pair must still yield a derived schedule when its first
    # room is non-AC and a later room is AC-enabled.
    DBI::dbExecute(
        dest,
        "UPDATE ROOM_GROUP SET IS_AC_ROOM = 1 WHERE ROOM_GROUP_ID = 12"
    )
    DBI::dbExecute(
        dest,
        "UPDATE ROOM_RELATION SET VENT_TYPE = 1, VENT_SET_MAX = 22 WHERE ID = 101"
    )
    mixed_ep <- eplusr::empty_idf(23.1)
    mixed_schedules <- schedule__convert(dest, mixed_ep)
    expect_warning(mixed <- ventilation__convert(dest, mixed_ep), "in AC zones")
    expect_equal(nrow(mixed$object), 3L)
    expect_equal(
        sum(grepl(
            "DeST Derived Ventilation",
            attr(mixed_schedules, "table")$NAME
        )),
        1L
    )

    # An unresolved flag is not evidence that the room is unconditioned.
    DBI::dbExecute(dest, "DELETE FROM ROOM_GROUP WHERE ROOM_GROUP_ID = 11")
    expect_error(ventilation__range_controls(dest), "Cannot resolve")
})

test_that("ventilation resolves the target zone-reference field", {
    skip_if_not("9.0.1" %in% eplusr::avail_eplus())

    expect_identical(
        ventilation__zone_field_name(eplusr::empty_idf("9.0.1")),
        "Zone or ZoneList Name"
    )
    expect_identical(
        ventilation__zone_field_name(eplusr::empty_idf(23.1)),
        "Zone or ZoneList or Space or SpaceList Name"
    )
})

test_that("normalizes a varying DeST ventilation range increment", {
    ep <- eplusr::empty_idf(23.1)
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    minimum <- rep(0.5, 8760L)
    maximum <- c(rep(5.25, 4380L), rep(10, 4380L))
    DBI::dbWriteTable(
        dest,
        "ROOM",
        data.frame(
            ID = 1L,
            NAME = "Room",
            OF_ROOM_GROUP = 11L,
            TYPE = 1L
        )
    )
    DBI::dbWriteTable(
        dest,
        "ROOM_GROUP",
        data.frame(ROOM_GROUP_ID = 11L, IS_AC_ROOM = 1L)
    )
    DBI::dbWriteTable(
        dest,
        "OUTSIDE",
        data.frame(
            OUTSIDE_ID = 10L,
            NAME = "Outside"
        )
    )
    DBI::dbWriteTable(
        dest,
        "ROOM_TYPE_DATA",
        data.frame(
            ID = 1L,
            SET_T_MIN_SCHEDULE = 30L,
            SET_T_MAX_SCHEDULE = 31L
        )
    )
    DBI::dbWriteTable(
        dest,
        "SCHEDULE_YEAR",
        data.frame(
            SCHEDULE_ID = c(20L, 22L, 30L, 31L),
            NAME = c("Minimum", "Maximum", "Heating", "Cooling"),
            TYPE = 4L,
            DATA = I(list(
                destep_test_schedule_blob(minimum),
                destep_test_schedule_blob(maximum),
                destep_test_schedule_blob(rep(18, 8760L)),
                destep_test_schedule_blob(rep(26, 8760L))
            ))
        )
    )
    DBI::dbWriteTable(
        dest,
        "ROOM_RELATION",
        data.frame(
            ID = 100L,
            NAME = ".",
            OF_BUILDING = 1L,
            ROOM_ID = 1L,
            RELA_ROOM_ID = 10L,
            VENT_SCHEDULE_ID = 20L,
            VENT_SET_MAX = 22L,
            VENT_TYPE = 1L,
            START_POINT_ID = 0L,
            END_POINT_ID = 0L,
            EXT_PROPERTY = 0L
        )
    )

    control <- ventilation__range_controls(dest)
    derived <- ventilation__range_schedule_rows(dest)

    expect_equal(control$INCREMENT_AIR_CHANGES_PER_HOUR, 9.5)
    expect_equal(
        unique(control$INCREMENT_FRACTION[[1L]]),
        c(0.5, 1)
    )
    expect_equal(unique(derived$DATA[[1L]]), c(0.5, 1))

    # The saved run switch must suppress only the increment, even when unused
    # maximum and temperature references no longer resolve in the source.
    DBI::dbWriteTable(
        dest,
        "OPTION",
        data.frame(
            KEYWORD = "VARIANT_VENT",
            OPTION_STRING = "0"
        )
    )
    DBI::dbExecute(dest, "UPDATE ROOM_RELATION SET VENT_SET_MAX = 999")
    DBI::dbExecute(dest, "UPDATE ROOM_TYPE_DATA SET SET_T_MIN_SCHEDULE = 998")
    disabled_schedule <- schedule__convert(dest, ep)
    expect_no_warning(disabled <- ventilation__convert(dest, ep))
    expect_equal(nrow(disabled$object), 1L)
    expect_equal(
        disabled$value$value_chr[disabled$value$field_name == "Schedule Name"],
        "Minimum"
    )
    expect_equal(
        disabled$value$value_num[
            disabled$value$field_name == "Air Changes per Hour"
        ],
        1
    )
    expect_false(any(grepl(
        "DeST Derived Ventilation",
        attr(disabled_schedule, "table")$NAME
    )))
    expect_identical(
        attr(disabled, "table")$RANGE_CONTROL_METHOD,
        "saved_switch_minimum_only"
    )
    expect_false(attr(disabled, "table")$VARIANT_VENT_ENABLED)
    expect_identical(
        attr(disabled, "table")$VARIANT_VENT_SELECTION,
        "saved_option"
    )

    # Re-enabling range ventilation must validate the missing dependencies.
    DBI::dbExecute(dest, "UPDATE OPTION SET OPTION_STRING = '1'")
    expect_error(ventilation__range_controls(dest), "Cannot resolve")
})

test_that("validates saved ventilation switches without guessing malformed values", {
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    expect_identical(
        ventilation__variant_selection(dest),
        list(enabled = TRUE, source = "legacy_missing_option")
    )
    DBI::dbWriteTable(
        dest,
        "OPTION",
        data.frame(
            KEYWORD = "VARIANT_VENT",
            OPTION_STRING = "1"
        )
    )
    expect_identical(
        ventilation__variant_selection(dest),
        list(enabled = TRUE, source = "saved_option")
    )
    for (invalid in c("2", "true", "", NA_character_)) {
        DBI::dbExecute(
            dest,
            "UPDATE OPTION SET OPTION_STRING = ?",
            params = list(invalid)
        )
        expect_error(
            ventilation__variant_selection(dest),
            "Invalid or ambiguous VARIANT_VENT"
        )
    }
    DBI::dbExecute(dest, "UPDATE OPTION SET OPTION_STRING = '0'")
    DBI::dbExecute(dest, "INSERT INTO OPTION VALUES ('VARIANT_VENT', '0')")
    expect_error(
        ventilation__variant_selection(dest),
        "Invalid or ambiguous VARIANT_VENT"
    )
})

test_that("rejects an inverted DeST ventilation range", {
    ep <- eplusr::empty_idf(23.1)
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(
        dest,
        "ROOM",
        data.frame(
            ID = 1L,
            NAME = "Room",
            OF_ROOM_GROUP = 11L,
            TYPE = 1L
        )
    )
    DBI::dbWriteTable(
        dest,
        "ROOM_GROUP",
        data.frame(ROOM_GROUP_ID = 11L, IS_AC_ROOM = 1L)
    )
    DBI::dbWriteTable(
        dest,
        "OUTSIDE",
        data.frame(
            OUTSIDE_ID = 10L,
            NAME = "Outside"
        )
    )
    DBI::dbWriteTable(
        dest,
        "ROOM_TYPE_DATA",
        data.frame(
            ID = 1L,
            SET_T_MIN_SCHEDULE = 30L,
            SET_T_MAX_SCHEDULE = 31L
        )
    )
    DBI::dbWriteTable(
        dest,
        "SCHEDULE_YEAR",
        data.frame(
            SCHEDULE_ID = c(20L, 22L, 30L, 31L),
            NAME = c("Minimum", "Maximum", "Heating", "Cooling"),
            TYPE = 4L,
            DATA = I(list(
                destep_test_schedule_blob(rep(10, 8760L)),
                destep_test_schedule_blob(rep(0.5, 8760L)),
                destep_test_schedule_blob(rep(18, 8760L)),
                destep_test_schedule_blob(rep(26, 8760L))
            ))
        )
    )
    DBI::dbWriteTable(
        dest,
        "ROOM_RELATION",
        data.frame(
            ID = 100L,
            NAME = ".",
            OF_BUILDING = 1L,
            ROOM_ID = 1L,
            RELA_ROOM_ID = 10L,
            VENT_SCHEDULE_ID = 20L,
            VENT_SET_MAX = 22L,
            VENT_TYPE = 1L,
            START_POINT_ID = 0L,
            END_POINT_ID = 0L,
            EXT_PROPERTY = 0L
        )
    )

    expect_error(
        ventilation__range_controls(dest),
        "Maximum ventilation schedule 22 is below minimum schedule 20"
    )
})

test_that("skips ROOM_RELATION rows that are not outdoor ventilation", {
    ep <- eplusr::empty_idf(23.1)
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(
        dest,
        "ROOM",
        data.frame(
            ID = c(1L, 2L),
            NAME = c("Room 101", "Room 102")
        )
    )
    DBI::dbWriteTable(
        dest,
        "OUTSIDE",
        data.frame(
            OUTSIDE_ID = integer(),
            NAME = character()
        )
    )
    DBI::dbWriteTable(
        dest,
        "SCHEDULE_YEAR",
        data.frame(
            SCHEDULE_ID = 20L,
            NAME = "Ventilation 0.5 ACH"
        )
    )
    DBI::dbWriteTable(
        dest,
        "ROOM_RELATION",
        data.frame(
            ID = 100L,
            NAME = ".",
            OF_BUILDING = 1L,
            ROOM_ID = 1L,
            RELA_ROOM_ID = 2L,
            VENT_SCHEDULE_ID = 20L,
            VENT_SET_MAX = 22,
            VENT_TYPE = 1L,
            START_POINT_ID = 0L,
            END_POINT_ID = 0L,
            EXT_PROPERTY = 0L
        )
    )

    expect_warning(
        expect_null(ventilation__convert(dest, ep)),
        "Skipped 1 ROOM_RELATION row"
    )
})

test_that("can convert ROOM_RELATION from a real DeST model", {
    skip_on_cran()

    ep <- eplusr::empty_idf(23.1)
    src <- ensure_dest_sqlite_file()
    on.exit(DBI::dbDisconnect(src), add = TRUE)

    path_tmp <- tempfile(fileext = ".sql")
    dest <- DBI::dbConnect(RSQLite::SQLite(), path_tmp)
    on.exit(
        {
            DBI::dbDisconnect(dest)
            unlink(path_tmp)
        },
        add = TRUE
    )
    RSQLite::sqliteCopyDatabase(src, dest)
    conv__update_names(dest)

    expect_warning(
        ventilation <- ventilation__convert(dest, ep),
        "documented outdoor-temperature-band rule"
    )
    tab <- attr(ventilation, "table")

    expect_equal(
        unique(ventilation$object$class_name),
        "ZoneVentilation:DesignFlowRate"
    )
    # This source fixture has 28 outdoor relations, of which five belong to
    # non-AC room groups. All 28 minima survive, with 23 range supplements.
    expect_equal(
        DBI::dbGetQuery(dest, "SELECT COUNT(*) AS N FROM ROOM_RELATION")$N,
        28L
    )
    expect_equal(sum(tab$IS_AC_ROOM == 0L), 5L)
    expect_equal(nrow(ventilation$object), 51L)
    expect_true(all(tab$IS_OUTDOOR_RELATION))
    expect_true(all(tab$VENT_TYPE == 1L))
    expect_equal(unique(tab$SCHEDULE_NAME), "通风全0.5")
    expect_equal(
        unique(tab$MAX_SCHEDULE_NAME),
        "房间与外界最大通风能力"
    )
    expect_true(all(tab$RANGE_CONTROL_CONVERTED))
    expect_equal(
        unique(tab$INCREMENT_AIR_CHANGES_PER_HOUR[tab$IS_AC_ROOM != 0]),
        9.5
    )
    expect_equal(
        unique(tab$INCREMENT_AIR_CHANGES_PER_HOUR[tab$IS_AC_ROOM == 0]),
        0
    )
    expect_setequal(
        unique(tab$RANGE_CONTROL_FIDELITY),
        c(
            "documented_rule_not_solver_equivalent",
            "source_non_ac_zone_minimum_only"
        )
    )
    expect_false(any(tab$HVAC_AVAILABILITY_GATED))
    expect_equal(unique(tab$AIR_CHANGES_PER_HOUR), 1)
    expect_false(anyNA(tab$ROOM_NAME))
    expect_false(anyNA(tab$SCHEDULE_NAME))
})
