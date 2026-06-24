test_that("can convert internal gains", {
    ep <- ensure_empty_idf()
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(dest, "ROOM", data.frame(
        ID = 1L,
        NAME = "Room 101"
    ))
    DBI::dbWriteTable(dest, "SCHEDULE_YEAR", data.frame(
        SCHEDULE_ID = 10L,
        NAME = "Always On"
    ))
    DBI::dbWriteTable(dest, "DIST_MODE", data.frame(
        DIST_MODE_ID = c(2L, 3L, 4L),
        DIST_AIR = c(0.5, 0.3, 0.7)
    ))
    DBI::dbWriteTable(dest, "OCCUPANT_GAINS", data.frame(
        GAIN_ID = 101L,
        NAME = "People 101",
        OF_ROOM = 1L,
        SCHEDULE = 10L,
        PER_AREA = 1L,
        MAXNUMBER = 0.2,
        HEAT_PER_PERSON = 40,
        DAMP_PER_PERSON = 0.1,
        DIST_MODE = 2L
    ))
    DBI::dbWriteTable(dest, "LIGHT_GAINS", data.frame(
        GAIN_ID = 102L,
        NAME = "Lights 101",
        OF_ROOM = 1L,
        SCHEDULE = 10L,
        PER_AREA = 1L,
        MAXPOWER = 10,
        HEAT_RATE = 0.9,
        DIST_MODE = 3L
    ))
    DBI::dbWriteTable(dest, "EQUIPMENT_GAINS", data.frame(
        GAIN_ID = 103L,
        NAME = "Equipment 101",
        OF_ROOM = 1L,
        SCHEDULE = 10L,
        PER_AREA = 1L,
        MAXPOWER = 40,
        DIST_MODE = 4L
    ))

    gains <- destep_conv_internal_gains(dest, ep)

    expect_type(gains, "list")
    expect_named(gains, c("object", "value"))
    expect_equal(
        unique(gains$object$class_name),
        c("Schedule:Constant", "People", "Lights", "ElectricEquipment")
    )
    expect_equal(
        gains$value$value_chr[
            gains$value$class_name == "People" &
                gains$value$field_name == "Activity Level Schedule Name"
        ],
        "Activity Level 109.44 W"
    )
})

test_that("can convert internal gains from a real DeST model", {
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

    gains <- destep_conv_internal_gains(dest, ep)

    expect_equal(sum(gains$object$class_name == "People"), 36L)
    expect_equal(sum(gains$object$class_name == "Lights"), 36L)
    expect_equal(sum(gains$object$class_name == "ElectricEquipment"), 36L)
    expect_true(all(c("OCCUPANT_GAINS", "LIGHT_GAINS", "EQUIPMENT_GAINS") %in% attr(gains, "table")$SOURCE_TABLE))
})
