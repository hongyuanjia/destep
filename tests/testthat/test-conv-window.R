test_that("can convert 'WINDOW'", {
    ep <- ensure_empty_idf()
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(dest, "DEFAULT_SETTING", data.frame(
        TABLE_NAME = character(),
        FIELD_NAME = character(),
        TYPE = integer(),
        LONG = integer()
    ))
    DBI::dbWriteTable(dest, "SURFACE", data.frame(
        SURFACE_ID = c(10L, 20L),
        NAME = c("Outside Face", "Room Wall"),
        TYPE = c(1L, 0L)
    ))
    DBI::dbWriteTable(dest, "MAIN_ENCLOSURE", data.frame(
        ID = 100L,
        SIDE1 = 10L,
        SIDE2 = 20L
    ))
    DBI::dbWriteTable(dest, "SYS_WINDOW", data.frame(
        WINDOW_ID = 4L,
        CNAME = "Double Window"
    ))
    DBI::dbWriteTable(dest, "WINDOW", data.frame(
        ID = 200L,
        NAME = "Window A",
        TYPE = 35L,
        OF_ENCLOSURE = 100L,
        MIDDLE_PLANE = 300L,
        WINDOW_CONSTRUCTION = 4L
    ))
    DBI::dbWriteTable(dest, "PLANE", data.frame(
        PLANE_ID = 300L,
        GEOMETRY = 400L
    ))
    DBI::dbWriteTable(dest, "GEOMETRY", data.frame(
        GEOMETRY_ID = 400L,
        BOUNDARY_LOOP_ID = 500L
    ))
    DBI::dbWriteTable(dest, "LOOP_POINT", data.frame(
        LOOP_ID = 500L,
        POINT_NO = 0:3,
        POINT = 1:4
    ))
    DBI::dbWriteTable(dest, "POINT", data.frame(
        POINT_ID = 1:4,
        X = c(1, 2, 2, 1),
        Y = c(0, 0, 0, 0),
        Z = c(1, 1, 3, 3)
    ))

    expect_type(window <- destep_conv_window(dest, ep), "list")
    expect_named(window, c("object", "value"))
    expect_equal(unique(window$object$class_name), "FenestrationSurface:Detailed")
    expect_s3_class(attr(window, "table"), "data.table")
    expect_equal(unique(attr(window, "table")$SURFACE_NAME), "Room Wall")
    expect_equal(unique(attr(window, "table")$CONSTRUCTION), "Double Window")
    expect_equal(attr(window, "table")$POINT_Z, c(3, 3, 1, 1))
})

test_that("skips window conversion without WINDOW records", {
    ep <- ensure_empty_idf()
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    expect_null(destep_conv_window(dest, ep))
})

test_that("can convert windows from a real DeST model", {
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

    window <- destep_conv_window(dest, ep)
    tab <- attr(window, "table")

    expect_equal(unique(window$object$class_name), "FenestrationSurface:Detailed")
    expect_equal(nrow(window$object), DBI::dbGetQuery(dest, "SELECT COUNT(*) AS N FROM WINDOW")$N)
    expect_false(anyNA(tab$SURFACE_NAME))
    expect_false(anyNA(tab$CONSTRUCTION))
    expect_true(all(tab$POINT_NO %in% 0:3))
})
