test_that("can convert ROOM_RELATION outdoor ventilation", {
    ep <- ensure_empty_idf()
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(dest, "ROOM", data.frame(
        ID = c(1L, 2L),
        NAME = c("Room 101", "Room 102"),
        TYPE = c(1L, 2L)
    ))
    DBI::dbWriteTable(dest, "OUTSIDE", data.frame(
        OUTSIDE_ID = 10L,
        NAME = "DefaultOutside"
    ))
    DBI::dbWriteTable(dest, "SCHEDULE_YEAR", data.frame(
        SCHEDULE_ID = c(20L, 22L, 30L, 31L),
        NAME = c(
            "Ventilation 0.5 ACH", "Ventilation 10 ACH",
            "Heating Setpoint", "Cooling Setpoint"
        ),
        TYPE = c(4L, 4L, 4L, 4L),
        DATA = I(list(
            destep_test_schedule_blob(rep(0.5, 8760L)),
            destep_test_schedule_blob(rep(10, 8760L)),
            destep_test_schedule_blob(rep(18, 8760L)),
            destep_test_schedule_blob(rep(26, 8760L))
        ))
    ))
    DBI::dbWriteTable(dest, "ROOM_TYPE_DATA", data.frame(
        ID = c(1L, 2L),
        SET_T_MIN_SCHEDULE = 30L,
        SET_T_MAX_SCHEDULE = 31L
    ))
    DBI::dbWriteTable(dest, "ROOM_RELATION", data.frame(
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
    ))

    schedule <- schedule__convert(dest, ep)
    expect_warning(
        ventilation <- ventilation__convert(dest, ep),
        "documented outdoor-temperature-band rule"
    )

    expect_type(ventilation, "list")
    expect_named(ventilation, c("object", "value"))
    expect_equal(unique(ventilation$object$class_name), "ZoneVentilation:DesignFlowRate")
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
            VENT_TYPE == 1L, RANGE_CONTROL_FIDELITY
        ],
        "documented_rule_not_solver_equivalent"
    )
    expect_false(
        attr(ventilation, "table")[
            VENT_TYPE == 1L, HVAC_AVAILABILITY_GATED
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
})

test_that("normalizes a varying DeST ventilation range increment", {
    ep <- ensure_empty_idf()
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    minimum <- rep(0.5, 8760L)
    maximum <- c(rep(5.25, 4380L), rep(10, 4380L))
    DBI::dbWriteTable(dest, "ROOM", data.frame(
        ID = 1L, NAME = "Room", TYPE = 1L
    ))
    DBI::dbWriteTable(dest, "OUTSIDE", data.frame(
        OUTSIDE_ID = 10L, NAME = "Outside"
    ))
    DBI::dbWriteTable(dest, "ROOM_TYPE_DATA", data.frame(
        ID = 1L, SET_T_MIN_SCHEDULE = 30L, SET_T_MAX_SCHEDULE = 31L
    ))
    DBI::dbWriteTable(dest, "SCHEDULE_YEAR", data.frame(
        SCHEDULE_ID = c(20L, 22L, 30L, 31L),
        NAME = c("Minimum", "Maximum", "Heating", "Cooling"),
        TYPE = 4L,
        DATA = I(list(
            destep_test_schedule_blob(minimum),
            destep_test_schedule_blob(maximum),
            destep_test_schedule_blob(rep(18, 8760L)),
            destep_test_schedule_blob(rep(26, 8760L))
        ))
    ))
    DBI::dbWriteTable(dest, "ROOM_RELATION", data.frame(
        ID = 100L, NAME = ".", OF_BUILDING = 1L, ROOM_ID = 1L,
        RELA_ROOM_ID = 10L, VENT_SCHEDULE_ID = 20L,
        VENT_SET_MAX = 22L, VENT_TYPE = 1L, START_POINT_ID = 0L,
        END_POINT_ID = 0L, EXT_PROPERTY = 0L
    ))

    control <- ventilation__range_controls(dest)
    derived <- ventilation__range_schedule_rows(dest)

    expect_equal(control$INCREMENT_AIR_CHANGES_PER_HOUR, 9.5)
    expect_equal(
        unique(control$INCREMENT_FRACTION[[1L]]),
        c(0.5, 1)
    )
    expect_equal(unique(derived$DATA[[1L]]), c(0.5, 1))
})

test_that("rejects an inverted DeST ventilation range", {
    ep <- ensure_empty_idf()
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(dest, "ROOM", data.frame(
        ID = 1L, NAME = "Room", TYPE = 1L
    ))
    DBI::dbWriteTable(dest, "OUTSIDE", data.frame(
        OUTSIDE_ID = 10L, NAME = "Outside"
    ))
    DBI::dbWriteTable(dest, "ROOM_TYPE_DATA", data.frame(
        ID = 1L, SET_T_MIN_SCHEDULE = 30L, SET_T_MAX_SCHEDULE = 31L
    ))
    DBI::dbWriteTable(dest, "SCHEDULE_YEAR", data.frame(
        SCHEDULE_ID = c(20L, 22L, 30L, 31L),
        NAME = c("Minimum", "Maximum", "Heating", "Cooling"),
        TYPE = 4L,
        DATA = I(list(
            destep_test_schedule_blob(rep(10, 8760L)),
            destep_test_schedule_blob(rep(0.5, 8760L)),
            destep_test_schedule_blob(rep(18, 8760L)),
            destep_test_schedule_blob(rep(26, 8760L))
        ))
    ))
    DBI::dbWriteTable(dest, "ROOM_RELATION", data.frame(
        ID = 100L, NAME = ".", OF_BUILDING = 1L, ROOM_ID = 1L,
        RELA_ROOM_ID = 10L, VENT_SCHEDULE_ID = 20L,
        VENT_SET_MAX = 22L, VENT_TYPE = 1L, START_POINT_ID = 0L,
        END_POINT_ID = 0L, EXT_PROPERTY = 0L
    ))

    expect_error(
        ventilation__range_controls(dest),
        "Maximum ventilation schedule 22 is below minimum schedule 20"
    )
})

test_that("skips ROOM_RELATION rows that are not outdoor ventilation", {
    ep <- ensure_empty_idf()
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(dest, "ROOM", data.frame(
        ID = c(1L, 2L),
        NAME = c("Room 101", "Room 102")
    ))
    DBI::dbWriteTable(dest, "OUTSIDE", data.frame(
        OUTSIDE_ID = integer(),
        NAME = character()
    ))
    DBI::dbWriteTable(dest, "SCHEDULE_YEAR", data.frame(
        SCHEDULE_ID = 20L,
        NAME = "Ventilation 0.5 ACH"
    ))
    DBI::dbWriteTable(dest, "ROOM_RELATION", data.frame(
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
    ))

    expect_warning(
        expect_null(ventilation__convert(dest, ep)),
        "Skipped 1 ROOM_RELATION row"
    )
})

test_that("can convert ROOM_RELATION from a real DeST model", {
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
    conv__update_names(dest)

    expect_warning(
        ventilation <- ventilation__convert(dest, ep),
        "documented outdoor-temperature-band rule"
    )
    tab <- attr(ventilation, "table")

    expect_equal(unique(ventilation$object$class_name), "ZoneVentilation:DesignFlowRate")
    expect_equal(
        nrow(ventilation$object),
        2L * DBI::dbGetQuery(
            dest, "SELECT COUNT(*) AS N FROM ROOM_RELATION"
        )$N
    )
    expect_true(all(tab$IS_OUTDOOR_RELATION))
    expect_true(all(tab$VENT_TYPE == 1L))
    expect_equal(unique(tab$SCHEDULE_NAME), "通风全0.5")
    expect_equal(
        unique(tab$MAX_SCHEDULE_NAME),
        "房间与外界最大通风能力"
    )
    expect_true(all(tab$RANGE_CONTROL_CONVERTED))
    expect_equal(unique(tab$INCREMENT_AIR_CHANGES_PER_HOUR), 9.5)
    expect_equal(
        unique(tab$RANGE_CONTROL_FIDELITY),
        "documented_rule_not_solver_equivalent"
    )
    expect_false(any(tab$HVAC_AVAILABILITY_GATED))
    expect_equal(unique(tab$AIR_CHANGES_PER_HOUR), 1)
    expect_false(anyNA(tab$ROOM_NAME))
    expect_false(anyNA(tab$SCHEDULE_NAME))
})
