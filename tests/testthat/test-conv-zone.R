test_that("can convert 'Zone'", {
    ep <- ensure_empty_idf()
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(dest, "STOREY", data.frame(
        ID = c(1L, 2L),
        NAME = c("Level 1", "Level 2"),
        MULTIPLE = c(1L, 3L),
        HEIGHT = c(3.2, 4.1)
    ))
    DBI::dbWriteTable(dest, "ROOM", data.frame(
        ID = c(10L, 11L, 20L),
        NAME = c("Room 101", "Room 102", "Room 201"),
        VOLUME = c(96, 80, 123),
        AREA = c(30, 25, 30),
        OF_STOREY = c(1L, 1L, 2L)
    ))

    # can convert 'Zone'
    expect_type(zone <- destep_conv_zone(dest, ep), "list")
    expect_named(zone, c("object", "value"))
    expect_equal(unique(zone$object$class_name), c("Zone", "ZoneList", "ZoneGroup"))
    expect_s3_class(attr(zone, "table"), "data.table")
    expect_equal(attr(zone, "table")$HEIGHT, c(3.2, 3.2, 4.1))
    expect_equal(
        zone$value$value_num[
            zone$value$class_name == "Zone" &
                zone$value$field_name == "Ceiling Height"
        ],
        c(3.2, 3.2, 4.1)
    )
})

test_that("can convert real DeST rooms with storey heights", {
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

    zone <- destep_conv_zone(dest, ep)
    tab <- attr(zone, "table")

    expect_gt(nrow(tab), 0L)
    expect_false(anyNA(tab$HEIGHT))
    expect_true(all(tab$HEIGHT > 0))
    expect_equal(
        zone$value$value_num[
            zone$value$class_name == "Zone" &
                zone$value$field_name == "Ceiling Height"
        ],
        tab$HEIGHT
    )
})
