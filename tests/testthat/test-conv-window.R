# Calculate one polygon area independently for aggregate window invariants.
window__area <- function(value) {
    following <- seq_len(nrow(value)) %% nrow(value) + 1L
    normal <- c(
        sum((value$POINT_Y - value$POINT_Y[following]) *
            (value$POINT_Z + value$POINT_Z[following])),
        sum((value$POINT_Z - value$POINT_Z[following]) *
            (value$POINT_X + value$POINT_X[following])),
        sum((value$POINT_X - value$POINT_X[following]) *
            (value$POINT_Y + value$POINT_Y[following]))
    )
    sqrt(sum(normal ^ 2)) / 2.0
}

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

    expect_type(window <- window__convert(dest, ep), "list")
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
    typed <- window__convert(dest, ep)
    expect_equal(
        unique(attr(typed, "table")$CONSTRUCTION),
        "High Performance Window Simple Glazing Construction"
    )

    # The same DeST middle-plane polygon represents both sides of an interzone
    # window, so conversion must create reciprocal EnergyPlus objects.
    DBI::dbExecute(dest, "UPDATE SURFACE SET TYPE = 0")
    DBI::dbExecute(dest, "UPDATE SURFACE SET NAME = 'Room A Wall' WHERE SURFACE_ID = 10")
    DBI::dbExecute(dest, "UPDATE SURFACE SET NAME = 'Room B Wall' WHERE SURFACE_ID = 20")
    pair <- window__convert(dest, ep)
    pair_table <- attr(pair, "table")
    pair_object <- unique(pair_table[, .(
        OUTPUT_PART_ID, NAME, BOUNDARY_OBJECT, SURFACE_NAME, SIDE, CONSTRUCTION
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
    expect_equal(
        unique(pair_object[SIDE == 1L]$CONSTRUCTION),
        "High Performance Window Simple Glazing Construction [Reverse]"
    )
    expect_equal(
        unique(pair_object[SIDE == 2L]$CONSTRUCTION),
        "High Performance Window Simple Glazing Construction"
    )
})

test_that("skips window conversion without WINDOW records", {
    ep <- ensure_empty_idf()
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    expect_null(window__convert(dest, ep))
})

test_that("rectangular windows stay intact on an intact host", {
    host <- data.table::data.table(
        ID = 10L, PART = 1L, NAME = "Wall", POINT_NO = 0:3,
        POINT_X = c(0, 4, 4, 0), POINT_Y = 0,
        POINT_Z = c(0, 0, 4, 4)
    )
    window <- data.table::data.table(
        OUTPUT_ID = "200-1", ID = 200L, SURFACE_ID = 10L,
        ORIGINAL_NAME = "Window", NAME = "Window", SIDE = 1L,
        INTERZONE = FALSE, BOUNDARY_OBJECT = NA_character_, POINT_NO = 0:3,
        POINT_X = c(1, 3, 3, 1), POINT_Y = 0,
        POINT_Z = c(1, 1, 3, 3)
    )

    split <- window__split_by_surface(window, host)

    expect_equal(data.table::uniqueN(split$OUTPUT_PART_ID), 1L)
    expect_equal(nrow(split), 4L)
})

test_that("window clipping rejects invalid host-plane geometry", {
    host <- data.table::data.table(
        POINT_NO = 0:3,
        POINT_X = c(0, 4, 4, 0), POINT_Y = c(0, 0, 0.005, 0),
        POINT_Z = c(0, 0, 4, 4)
    )
    window <- data.table::data.table(
        POINT_NO = 0:3,
        POINT_X = c(1, 3, 3, 1), POINT_Y = 0,
        POINT_Z = c(1, 1, 3, 3)
    )
    expect_error(
        window__clip_polygon(window, host),
        "host must be planar and convex"
    )

    host[, POINT_Y := 0]
    window[, POINT_Y := 0.001]
    expect_error(
        window__clip_polygon(window, host),
        "not coplanar"
    )
})

test_that("interzone window pieces use the same canonical partition", {
    # The two room sides have opposite winding. Partitioning them independently
    # must not choose opposite quadrilateral diagonals for reciprocal pieces.
    side1_host <- data.table::data.table(
        ID = 10L, PART = 1L, NAME = "Wall A", POINT_NO = 0:3,
        POINT_X = c(0, 4, 4, 0), POINT_Y = 0,
        POINT_Z = c(0, 0, 4, 4)
    )
    side2_host <- data.table::copy(side1_host[4:1])
    side2_host[, `:=`(ID = 20L, NAME = "Wall B", POINT_NO = 0:3)]

    side1 <- data.table::data.table(
        OUTPUT_ID = "200-1", ID = 200L, SURFACE_ID = 10L,
        ORIGINAL_NAME = "Window", NAME = "Window", SIDE = 1L,
        INTERZONE = TRUE, BOUNDARY_OBJECT = NA_character_, POINT_NO = 0:3,
        POINT_X = c(1, 3, 2.5, 1), POINT_Y = 0,
        POINT_Z = c(1, 1, 3, 3)
    )
    side2 <- data.table::copy(side1[4:1])
    side2[, `:=`(
        OUTPUT_ID = "200-2", SURFACE_ID = 20L, SIDE = 2L,
        POINT_NO = 0:3
    )]
    split <- window__split_by_surface(
        data.table::rbindlist(list(side1, side2)),
        data.table::rbindlist(list(side1_host, side2_host))
    )
    piece <- split[, .(
        VERTICES = paste(sort(sprintf(
            "%.12f|%.12f|%.12f", POINT_X, POINT_Y, POINT_Z
        )), collapse = ";")
    ), by = .(SIDE, PIECE)]
    side1_piece <- piece[SIDE == 1L][order(PIECE)]$VERTICES
    side2_piece <- piece[SIDE == 2L][order(PIECE)]$VERTICES

    expect_equal(length(side1_piece), 2L)
    expect_equal(side1_piece, side2_piece)
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

    surface <- attr(surface__convert(dest, ep), "table")
    window <- window__convert(dest, ep, surface)
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
        , as.list(geom__unit_normal(.SD)), by = .(OUTPUT_PART_ID, SURFACE_NAME)
    ]
    surface_normal <- surface[, as.list(geom__unit_normal(.SD)), by = .(OUTPUT_ID, NAME)]
    parent <- match(window_normal$SURFACE_NAME, surface_normal$NAME)
    expect_false(anyNA(parent))
    expect_true(all(
        window_normal$V1 * surface_normal$V1[parent] +
            window_normal$V2 * surface_normal$V2[parent] +
            window_normal$V3 * surface_normal$V3[parent] > 1.0 - 1e-6
    ))

    raw <- data.table::as.data.table(DBI::dbGetQuery(dest, "
        SELECT W.ID, L.POINT_NO, ROUND(P.X, 3) AS POINT_X,
            ROUND(P.Y, 3) AS POINT_Y, ROUND(P.Z, 3) AS POINT_Z
        FROM WINDOW W
        INNER JOIN PLANE PL ON W.MIDDLE_PLANE = PL.PLANE_ID
        INNER JOIN GEOMETRY G ON PL.GEOMETRY = G.GEOMETRY_ID
        INNER JOIN LOOP_POINT L ON G.BOUNDARY_LOOP_ID = L.LOOP_ID
        INNER JOIN POINT P ON L.POINT = P.POINT_ID
        ORDER BY W.ID, L.POINT_NO
    "))
    source_area <- raw[, .(SOURCE_AREA = window__area(.SD)), by = "ID"]
    converted_area <- tab[, .(PIECE_AREA = window__area(.SD)),
        by = .(ID, OUTPUT_ID, OUTPUT_PART_ID)][,
        .(CONVERTED_AREA = sum(PIECE_AREA)), by = .(ID, OUTPUT_ID)]
    area <- source_area[converted_area, on = "ID"]
    expect_lt(max(abs(area$CONVERTED_AREA - area$SOURCE_AREA)), 1e-6)
})
