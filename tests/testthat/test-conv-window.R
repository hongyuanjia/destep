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
        TYPE = c(1L, 0L),
        AZIMUTH = c(180, 0),
        TILT = c(90, 90)
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

    # A valid aggregate window type replaces the detailed SYS_WINDOW reference
    # while leaving the fenestration geometry and host assignment unchanged.
    DBI::dbWriteTable(dest, "WINDOW_TYPE_DATA", data.frame(
        ID = 35L,
        NAME = "High Performance Window",
        K = 2.0,
        SC = 0.4022989,
        LIGHT_TRANS_RATIO = 0.58
    ))
    typed <- destep_conv_window(dest, ep)
    expect_equal(
        unique(attr(typed, "table")$CONSTRUCTION),
        "High Performance Window Simple Glazing Construction"
    )

    # The same DeST middle-plane polygon represents both sides of an interzone
    # window, so conversion must create reciprocal EnergyPlus objects.
    DBI::dbExecute(dest, "UPDATE SURFACE SET TYPE = 0")
    DBI::dbExecute(dest, "UPDATE SURFACE SET NAME = 'Room A Wall' WHERE SURFACE_ID = 10")
    DBI::dbExecute(dest, "UPDATE SURFACE SET NAME = 'Room B Wall' WHERE SURFACE_ID = 20")
    pair <- destep_conv_window(dest, ep)
    pair_table <- attr(pair, "table")
    pair_object <- unique(pair_table[, .(
        OUTPUT_PART_ID, NAME, BOUNDARY_OBJECT, SURFACE_NAME
    )])

    expect_equal(nrow(pair$object), 2L)
    expect_setequal(pair_object$NAME, c("Window A [1]", "Window A [2]"))
    expect_equal(
        pair_object$BOUNDARY_OBJECT[match(
            c("Window A [1]", "Window A [2]"), pair_object$NAME
        )],
        c("Window A [2]", "Window A [1]")
    )
    expect_setequal(pair_object$SURFACE_NAME, c("Room A Wall", "Room B Wall"))
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

    surface <- attr(destep_conv_surface(dest, ep), "table")
    window <- destep_conv_window(dest, ep)
    tab <- attr(window, "table")

    expect_equal(unique(window$object$class_name), "FenestrationSurface:Detailed")
    expected <- DBI::dbGetQuery(dest, "
        SELECT SUM(CASE
            WHEN S1.TYPE NOT IN (1, 2) AND S2.TYPE NOT IN (1, 2) THEN 2
            ELSE 1
        END) AS N
        FROM WINDOW W
        INNER JOIN MAIN_ENCLOSURE E ON W.OF_ENCLOSURE = E.ID
        LEFT JOIN SURFACE S1 ON E.SIDE1 = S1.SURFACE_ID
        LEFT JOIN SURFACE S2 ON E.SIDE2 = S2.SURFACE_ID
    ")$N
    expect_equal(data.table::uniqueN(tab$OUTPUT_ID), expected)
    expect_false(anyNA(tab$SURFACE_NAME))
    expect_false(anyNA(tab$CONSTRUCTION))
    expect_true(all(tab$POINT_NO %in% 0:3))

    window_normal <- tab[
        , as.list(destep_surface_normal(.SD)), by = .(OUTPUT_PART_ID, SURFACE_NAME)
    ]
    surface_normal <- surface[, as.list(destep_surface_normal(.SD)), by = .(OUTPUT_ID, NAME)]
    parent <- match(window_normal$SURFACE_NAME, surface_normal$NAME)
    expect_false(anyNA(parent))
    expect_true(all(
        window_normal$V1 * surface_normal$V1[parent] +
            window_normal$V2 * surface_normal$V2[parent] +
            window_normal$V3 * surface_normal$V3[parent] > 1.0 - 1e-6
    ))
})
