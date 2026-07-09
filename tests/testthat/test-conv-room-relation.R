test_that("can convert ROOM_RELATION outdoor ventilation", {
    ep <- ensure_empty_idf()
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(dest, "ROOM", data.frame(
        ID = c(1L, 2L),
        NAME = c("Room 101", "Room 102")
    ))
    DBI::dbWriteTable(dest, "OUTSIDE", data.frame(
        OUTSIDE_ID = 10L,
        NAME = "DefaultOutside"
    ))
    DBI::dbWriteTable(dest, "SCHEDULE_YEAR", data.frame(
        SCHEDULE_ID = 20L,
        NAME = "Ventilation 0.5 ACH"
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

    ventilation <- destep_conv_room_ventilation(dest, ep)

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
        rep("Ventilation 0.5 ACH", 2L)
    )
    expect_equal(
        ventilation$value$value_num[
            ventilation$value$field_name == "Air Changes per Hour"
        ],
        c(1, 1)
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
        expect_null(destep_conv_room_ventilation(dest, ep)),
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
    destep_update_name(dest)

    ventilation <- destep_conv_room_ventilation(dest, ep)
    tab <- attr(ventilation, "table")

    expect_equal(unique(ventilation$object$class_name), "ZoneVentilation:DesignFlowRate")
    expect_equal(
        nrow(ventilation$object),
        DBI::dbGetQuery(dest, "SELECT COUNT(*) AS N FROM ROOM_RELATION")$N
    )
    expect_true(all(tab$IS_OUTDOOR_RELATION))
    expect_true(all(tab$VENT_TYPE == 1L))
    expect_equal(unique(tab$SCHEDULE_NAME), "通风全0.5")
    expect_equal(unique(tab$AIR_CHANGES_PER_HOUR), 1)
    expect_false(anyNA(tab$ROOM_NAME))
    expect_false(anyNA(tab$SCHEDULE_NAME))
})
