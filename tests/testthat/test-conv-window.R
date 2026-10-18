# Calculate one polygon area independently for aggregate window invariants.
window__area <- function(value) {
    following <- seq_len(nrow(value)) %% nrow(value) + 1L
    normal <- c(
        sum(
            (value$POINT_Y - value$POINT_Y[following]) *
                (value$POINT_Z + value$POINT_Z[following])
        ),
        sum(
            (value$POINT_Z - value$POINT_Z[following]) *
                (value$POINT_X + value$POINT_X[following])
        ),
        sum(
            (value$POINT_X - value$POINT_X[following]) *
                (value$POINT_Y + value$POINT_Y[following])
        )
    )
    sqrt(sum(normal^2)) / 2.0
}

test_that("can convert 'WINDOW'", {
    ep <- eplusr::empty_idf(23.1)
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(
        dest,
        "DEFAULT_SETTING",
        data.frame(
            TABLE_NAME = character(),
            FIELD_NAME = character(),
            TYPE = integer(),
            LONG = integer()
        )
    )
    DBI::dbWriteTable(
        dest,
        "SURFACE",
        data.table::data.table(
            SURFACE_ID = c(10L, 20L, 11L, 21L),
            NAME = c(
                "Outside Host",
                "Room Wall",
                "Outside Window Face",
                "Room Window Face"
            ),
            TYPE = c(1L, 0L, 1L, 0L),
            AZIMUTH = c(180, 0, 180, 0),
            TILT = c(90, 90, 90, 90),
            VENTILATION_COEF = c(21.6, 1.8, 17.8, 4.5)
        )
    )
    DBI::dbWriteTable(
        dest,
        "MAIN_ENCLOSURE",
        data.frame(
            ID = 100L,
            SIDE1 = 10L,
            SIDE2 = 20L
        )
    )
    DBI::dbWriteTable(
        dest,
        "SYS_WINDOW",
        data.frame(
            WINDOW_ID = 4L,
            CNAME = "Double Window"
        )
    )
    DBI::dbWriteTable(
        dest,
        "WINDOW",
        data.frame(
            ID = 200L,
            NAME = "Window A",
            SIDE1 = 11L,
            SIDE2 = 21L,
            TYPE = 35L,
            OF_ENCLOSURE = 100L,
            MIDDLE_PLANE = 300L,
            WINDOW_CONSTRUCTION = 4L
        )
    )
    DBI::dbWriteTable(
        dest,
        "PLANE",
        data.frame(
            PLANE_ID = 300L,
            GEOMETRY = 400L
        )
    )
    DBI::dbWriteTable(
        dest,
        "GEOMETRY",
        data.frame(
            GEOMETRY_ID = 400L,
            BOUNDARY_LOOP_ID = 500L
        )
    )
    DBI::dbWriteTable(
        dest,
        "LOOP_POINT",
        data.frame(
            LOOP_ID = 500L,
            POINT_NO = 0:3,
            POINT = 1:4
        )
    )
    DBI::dbWriteTable(
        dest,
        "POINT",
        data.frame(
            POINT_ID = 1:4,
            X = c(1, 2, 2, 1),
            Y = c(0, 0, 0, 0),
            Z = c(1, 1, 3, 3)
        )
    )

    expect_error(
        window__convert(dest, ep),
        "WINDOW.ID=200 TYPE=35 \\(missing WINDOW_TYPE_DATA table or required fields\\)"
    )
    DBI::dbWriteTable(
        dest,
        "WINDOW_TYPE_DATA",
        data.frame(
            ID = 35L,
            NAME = "High Performance Window",
            K = 2.0,
            SC = 0.4022989,
            LIGHT_TRANS_RATIO = 0.58
        )
    )

    expect_type(window <- window__convert(dest, ep), "list")
    # Native convection removes only film overrides, preserving the window
    # geometry and the source data used by optical and construction conversion.
    automatic <- window__convert(dest, ep, surface_convection = "energyplus")
    expect_false(
        "SurfaceProperty:ConvectionCoefficients" %in%
            automatic$object$class_name
    )
    expect_equal(attr(automatic, "table"), attr(window, "table"))
    expect_equal(
        automatic$value,
        window$value[class_name == "FenestrationSurface:Detailed"]
    )
    expect_named(window, c("object", "value"))
    expect_setequal(
        unique(window$object$class_name),
        c(
            "FenestrationSurface:Detailed",
            "SurfaceProperty:ConvectionCoefficients"
        )
    )
    expect_s3_class(attr(window, "table"), "data.table")
    expect_equal(unique(attr(window, "table")$SURFACE_NAME), "Room Wall")
    expect_equal(
        unique(attr(window, "table")$CONSTRUCTION),
        "High Performance Window Simple Glazing Construction"
    )
    expect_equal(attr(window, "table")$POINT_X, c(2, 2, 1, 1))
    expect_equal(attr(window, "table")$POINT_Z, c(3, 1, 1, 3))
    convection <- window$value[
        class_name == "SurfaceProperty:ConvectionCoefficients"
    ]
    expect_equal(
        convection[field_name == "Convection Coefficient 1", value_num],
        4.5
    )
    expect_equal(
        convection[field_name == "Convection Coefficient 2", value_num],
        17.8
    )

    # A valid aggregate window type replaces the detailed SYS_WINDOW reference
    # while leaving the fenestration geometry and host assignment unchanged.
    typed <- window__convert(dest, ep)
    expect_equal(
        unique(attr(typed, "table")$CONSTRUCTION),
        "High Performance Window Simple Glazing Construction"
    )

    # The same DeST middle-plane polygon represents both sides of an interzone
    # window, so conversion must create reciprocal EnergyPlus objects.
    DBI::dbExecute(dest, "UPDATE SURFACE SET TYPE = 0")
    DBI::dbExecute(
        dest,
        "UPDATE SURFACE SET NAME = 'Room A Wall' WHERE SURFACE_ID = 10"
    )
    DBI::dbExecute(
        dest,
        "UPDATE SURFACE SET NAME = 'Room B Wall' WHERE SURFACE_ID = 20"
    )
    pair <- window__convert(dest, ep)
    pair_table <- attr(pair, "table")
    pair_object <- unique(pair_table[, .(
        OUTPUT_PART_ID,
        NAME,
        BOUNDARY_OBJECT,
        SURFACE_NAME,
        SIDE,
        CONSTRUCTION
    )])

    expect_equal(
        pair$object[, .N, by = "class_name"][
            class_name == "FenestrationSurface:Detailed",
            N
        ],
        2L
    )
    expect_setequal(pair_object$NAME, c("Window A [1]", "Window A [2]"))
    expect_equal(
        pair_object$BOUNDARY_OBJECT[match(
            c("Window A [1]", "Window A [2]"),
            pair_object$NAME
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
    pair_convection <- pair$value[
        class_name == "SurfaceProperty:ConvectionCoefficients"
    ]
    expect_setequal(
        pair_convection[
            field_name == "Convection Coefficient 1",
            value_num
        ],
        c(17.8, 4.5)
    )
    expect_false(any(
        pair_convection$field_name == "Convection Coefficient 2 Location"
    ))
})

test_that("skips window conversion without WINDOW records", {
    ep <- eplusr::empty_idf(23.1)
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    expect_null(window__convert(dest, ep))
})

test_that("rectangular windows stay intact on an intact host", {
    host <- data.table::data.table(
        ID = 10L,
        PART = 1L,
        NAME = "Wall",
        POINT_NO = 0:3,
        POINT_X = c(0, 4, 4, 0),
        POINT_Y = 0,
        POINT_Z = c(0, 0, 4, 4)
    )
    window <- data.table::data.table(
        OUTPUT_ID = "200-1",
        ID = 200L,
        SURFACE_ID = 10L,
        ORIGINAL_NAME = "Window",
        NAME = "Window",
        SIDE = 1L,
        INTERZONE = FALSE,
        BOUNDARY_OBJECT = NA_character_,
        POINT_NO = 0:3,
        POINT_X = c(1, 3, 3, 1),
        POINT_Y = 0,
        POINT_Z = c(1, 1, 3, 3)
    )

    split <- subsurface__split_by_surface(window, host)

    expect_equal(data.table::uniqueN(split$OUTPUT_PART_ID), 1L)
    expect_equal(nrow(split), 4L)
})

test_that("window clipping rejects invalid host-plane geometry", {
    host <- data.table::data.table(
        POINT_NO = 0:3,
        POINT_X = c(0, 4, 4, 0),
        POINT_Y = c(0, 0, 0.005, 0),
        POINT_Z = c(0, 0, 4, 4)
    )
    window <- data.table::data.table(
        POINT_NO = 0:3,
        POINT_X = c(1, 3, 3, 1),
        POINT_Y = 0,
        POINT_Z = c(1, 1, 3, 3)
    )
    expect_error(
        subsurface__clip_polygon(window, host),
        "host must be planar and convex"
    )

    host[, POINT_Y := 0]
    window[, POINT_Y := 0.001]
    expect_error(
        subsurface__clip_polygon(window, host),
        "not coplanar"
    )
})

test_that("interzone window pieces use the same canonical partition", {
    # The two room sides have opposite winding. Partitioning them independently
    # must not choose opposite quadrilateral diagonals for reciprocal pieces.
    side1_host <- data.table::data.table(
        ID = 10L,
        PART = 1L,
        NAME = "Wall A",
        POINT_NO = 0:3,
        POINT_X = c(0, 4, 4, 0),
        POINT_Y = 0,
        POINT_Z = c(0, 0, 4, 4)
    )
    side2_host <- data.table::copy(side1_host[4:1])
    side2_host[, `:=`(ID = 20L, NAME = "Wall B", POINT_NO = 0:3)]

    side1 <- data.table::data.table(
        OUTPUT_ID = "200-1",
        ID = 200L,
        SURFACE_ID = 10L,
        ORIGINAL_NAME = "Window",
        NAME = "Window",
        SIDE = 1L,
        INTERZONE = TRUE,
        BOUNDARY_OBJECT = NA_character_,
        POINT_NO = 0:3,
        POINT_X = c(1, 3, 2.5, 1),
        POINT_Y = 0,
        POINT_Z = c(1, 1, 3, 3)
    )
    side2 <- data.table::copy(side1[4:1])
    side2[, `:=`(
        OUTPUT_ID = "200-2",
        SURFACE_ID = 20L,
        SIDE = 2L,
        POINT_NO = 0:3
    )]
    split <- subsurface__split_by_surface(
        data.table::rbindlist(list(side1, side2)),
        data.table::rbindlist(list(side1_host, side2_host))
    )
    piece <- split[,
        .(
            VERTICES = paste(
                sort(sprintf(
                    "%.12f|%.12f|%.12f",
                    POINT_X,
                    POINT_Y,
                    POINT_Z
                )),
                collapse = ";"
            )
        ),
        by = .(SIDE, PIECE)
    ]
    side1_piece <- piece[SIDE == 1L][order(PIECE)]$VERTICES
    side2_piece <- piece[SIDE == 2L][order(PIECE)]$VERTICES

    expect_equal(length(side1_piece), 2L)
    expect_equal(side1_piece, side2_piece)
})

test_that("can convert windows from a real DeST model", {
    skip_on_cran()

    ep <- eplusr::empty_idf(23.1)
    src <- destep_test__unmultiplied_fixture()
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

    surface <- attr(surface__convert(dest, ep), "table")
    expect_warning(
        window <- window__convert(dest, ep, surface),
        class = "destep_unsupported_solar_distribution"
    )
    tab <- attr(window, "table")

    expect_setequal(
        unique(window$object$class_name),
        c(
            "FenestrationSurface:Detailed",
            "SurfaceProperty:ConvectionCoefficients"
        )
    )
    expected <- DBI::dbGetQuery(
        dest,
        "
        SELECT SUM(CASE
            WHEN S1.TYPE NOT IN (1, 2) AND S2.TYPE NOT IN (1, 2) THEN 2
            ELSE 1
        END) AS N
        FROM WINDOW W
        INNER JOIN MAIN_ENCLOSURE E ON W.OF_ENCLOSURE = E.ID
        LEFT JOIN SURFACE S1 ON E.SIDE1 = S1.SURFACE_ID
        LEFT JOIN SURFACE S2 ON E.SIDE2 = S2.SURFACE_ID
    "
    )$N
    expect_equal(data.table::uniqueN(tab$OUTPUT_ID), expected)
    expect_false(anyNA(tab$SURFACE_NAME))
    expect_false(anyNA(tab$CONSTRUCTION))
    expect_true(all(tab$POINT_NO %in% 0:3))

    window_normal <- tab[,
        as.list(geom__unit_normal(.SD)),
        by = .(OUTPUT_PART_ID, SURFACE_NAME)
    ]
    surface_normal <- surface[,
        as.list(geom__unit_normal(.SD)),
        by = .(OUTPUT_ID, NAME)
    ]
    parent <- match(window_normal$SURFACE_NAME, surface_normal$NAME)
    expect_false(anyNA(parent))
    expect_true(all(
        window_normal$V1 *
            surface_normal$V1[parent] +
            window_normal$V2 * surface_normal$V2[parent] +
            window_normal$V3 * surface_normal$V3[parent] >
            1.0 - 1e-6
    ))

    raw <- data.table::as.data.table(DBI::dbGetQuery(
        dest,
        "
        SELECT W.ID, L.POINT_NO, ROUND(P.X, 3) AS POINT_X,
            ROUND(P.Y, 3) AS POINT_Y, ROUND(P.Z, 3) AS POINT_Z
        FROM WINDOW W
        INNER JOIN PLANE PL ON W.MIDDLE_PLANE = PL.PLANE_ID
        INNER JOIN GEOMETRY G ON PL.GEOMETRY = G.GEOMETRY_ID
        INNER JOIN LOOP_POINT L ON G.BOUNDARY_LOOP_ID = L.LOOP_ID
        INNER JOIN POINT P ON L.POINT = P.POINT_ID
        ORDER BY W.ID, L.POINT_NO
    "
    ))
    source_area <- raw[, .(SOURCE_AREA = window__area(.SD)), by = "ID"]
    converted_area <- tab[,
        .(PIECE_AREA = window__area(.SD)),
        by = .(ID, OUTPUT_ID, OUTPUT_PART_ID)
    ][,
        .(CONVERTED_AREA = sum(PIECE_AREA)),
        by = .(ID, OUTPUT_ID)
    ]
    area <- source_area[converted_area, on = "ID"]
    expect_lt(max(abs(area$CONVERTED_AREA - area$SOURCE_AREA)), 1e-6)
})

# Construct a source relation with three windows of the same type but distinct
# material-area factors and ordered emissivities; geometry is deliberately absent.
window_test__performance_db <- function() {
    db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbWriteTable(
        db,
        "WINDOW",
        data.frame(
            ID = 1:3,
            TYPE = 46L,
            WINDOW_CONSTRUCTION = 4L,
            K = c(1, 2, 1),
            SIDE1 = 1:3,
            SIDE2 = 4:6
        )
    )
    DBI::dbWriteTable(
        db,
        "WINDOW_TYPE_DATA",
        data.frame(
            ID = 46L,
            NAME = "Native two-layer",
            K = 1.9,
            SC = .883,
            LIGHT_TRANS_RATIO = .703,
            LIGHT_REFLECT_RATIO = .128,
            LAYER_NUM = 2L
        )
    )
    DBI::dbWriteTable(
        db,
        "SURFACE",
        data.frame(SURFACE_ID = 1:6, BLACKNESS = c(.84, .84, .2, .84, .84, .84))
    )
    db
}

test_that("native normal and fixed-60-degree optics agree with independently retained evidence", {
    # These constants were recovered and independently checked against original
    # PE instructions and native diffuse QT before the converter implementation.
    optical <- window__n2_optics(.883)
    expect_equal(
        unname(optical[c("tau", "rho", "diffuse_tau", "diffuse_alpha")]),
        c(
            .7515969409937324,
            .1380098240762757,
            .6100395965174817,
            .13129574489050247
        ),
        tolerance = 1e-12
    )
    expect_gt(optical[["nominal_tau"]], optical[["tau"]])
})

test_that("source angular branch and grazing endpoint preserve independent N2 evidence", {
    # Retained Python reconstruction from the native PE checks uses an explicit
    # per-layer absorption flux, independently of this R backward recurrence.
    expected <- rbind(
        c(.74674010936077295, .13723058000687838),
        c(.74200723722993178, .14159751114135555),
        c(.74097577549888072, .14223144649944885),
        c(.61003959651748174, .25866465859201571),
        c(.0056647250982294235, .93352465317546918),
        c(0, 1)
    )
    parameter <- window__n2_optics(.883)[["parameter"]]
    actual <- t(vapply(
        c(29, 30, 31, 60, 89, 90) * pi / 180,
        function(angle) unname(window__n2_angle(parameter, angle)),
        numeric(2L)
    ))
    expect_equal(actual, expected, tolerance = 1e-12)
    # A high, unreachable nominal SC retains the bounded source search rather
    # than rescaling the angle curve to hit .87*SC or an annual load target.
    expect_lt(window__n2_optics(1.2)[["tau"]], .87 * 1.2)
    for (sc in c(0, .1, .480459770114943, .883, 1.2)) {
        parameter <- window__n2_optics(sc)[["parameter"]]
        curve <- vapply(
            0:90 * pi / 180,
            function(angle) {
                window__n2_angle(parameter, angle)
            },
            numeric(2L)
        )
        expect_true(all(is.finite(curve)))
        expect_true(all(curve >= 0 & curve <= 1))
        expect_true(all(colSums(curve) <= 1 + 1e-12))
    }
})

test_that("only emitted N2 source angle tables require the newer table schema", {
    db <- window_test__performance_db()
    on.exit(DBI::dbDisconnect(db))
    expect_identical(
        window__angle_generation_version(db, "26.1"),
        numeric_version("9.2.0")
    )
    expect_error(
        window__angle_generation_version(db, "9.1"),
        class = "destep_unsupported_window_angle_target"
    )
    glazing <- const__window_type_objects(const__window_type_performance(
        db
    ))$glazing
    expect_error(
        window__equivalent_objects(db, eplusr::empty_idf("9.1"), glazing),
        class = "destep_unsupported_window_angle_target"
    )
    # The unsupported-layer fallback is unchanged; an unused N2 catalogue row
    # must not raise the generation version of actual N1 windows.
    DBI::dbExecute(db, "UPDATE WINDOW_TYPE_DATA SET LAYER_NUM=1")
    expect_null(window__angle_generation_version(db, "9.0.1"))
    DBI::dbExecute(db, "DELETE FROM WINDOW")
    expect_null(window__angle_generation_version(db, "9.0.1"))
})

test_that("source angle table order and references survive EnergyPlus serialization", {
    db <- window_test__performance_db()
    on.exit(DBI::dbDisconnect(db))
    ep <- eplusr::empty_idf("9.2")
    ep$add(
        Building = list(name = "Angle fixture"),
        GlobalGeometryRules = list(
            starting_vertex_position = "UpperLeftCorner",
            vertex_entry_direction = "Counterclockwise",
            coordinate_system = "Relative"
        )
    )
    converted <- window__equivalent_objects(
        db,
        ep,
        const__window_type_objects(const__window_type_performance(db))$glazing
    )
    table <- conv__combine_outputs(list(converted))
    # Add all related classes together, so the IDD validates every named curve
    # and independent axis reference in the actual material graph.
    private <- eplusr::get_priv_env(ep)
    add <- eplusr::add_idf_object(
        private$idd_env(),
        private$idf_env(),
        table$object,
        table$value,
        default = TRUE,
        unique = FALSE,
        empty = TRUE,
        level = "draft"
    )
    private$update_idf_env(add)
    expect_true(ep$is_valid())
    fragment <- tempfile(fileext = ".idf")
    on.exit(unlink(fragment))
    ep$save(fragment, overwrite = TRUE)
    reread <- eplusr::read_idf(fragment)
    expect_true(reread$is_valid())
    objects <- reread$to_table()
    expect_true(all(
        objects$value[objects$field == "Optical Data Type"] ==
            "SpectralAndAngle"
    ))
    values <- reread$Table_Lookup[[1L]]$to_table()
    # Two wavelengths repeat within each angle; transposing these dimensions
    # used to produce valid IDF syntax but the wrong optical response.
    tau <- as.numeric(values$value[values$index >= 11L])
    expected <- vapply(
        0:90 * pi / 180,
        function(angle) {
            window__n2_angle(window__n2_optics(.883)[["parameter"]], angle)[[
                "tau"
            ]]
        },
        numeric(1L)
    )
    expect_equal(tau, rep(expected, each = 2L), tolerance = 1e-12)
    expect_equal(length(tau), 182L)
    front <- reread$Table_Lookup[[2L]]$to_table()
    back <- reread$Table_Lookup[[3L]]$to_table()
    front <- as.numeric(front$value[front$index >= 11L])
    back <- as.numeric(back$value[back$index >= 11L])
    expect_equal(front, back)
    expect_true(all(tau + front <= 1 + 1e-12))
    angle <- reread$Table_IndependentVariable[[1L]]$to_table()
    wave <- reread$Table_IndependentVariable[[2L]]$to_table()
    expect_equal(as.numeric(angle$value[angle$index >= 11L]), 0:90)
    expect_equal(as.numeric(wave$value[wave$index >= 11L]), c(.1, 4))
    # Runtime 9.2/23.1 use output-unit validation for independent axes. Their
    # WindowManager still passes the first coordinate as degrees, independently
    # of this metadata; both axes must use the compatible Dimensionless value.
    expect_identical(angle$value[angle$field == "Unit Type"], "Dimensionless")
    expect_identical(wave$value[wave$field == "Unit Type"], "Dimensionless")
})

test_that("row K and ordered emissivities prevent incorrect same-type merging", {
    db <- window_test__performance_db()
    on.exit(DBI::dbDisconnect(db))
    source <- const__window_type_performance(db)
    source <- source[order(source$WINDOW_ID)]
    expect_true(all(source$DETAILED_EQUIVALENT))
    expect_equal(
        source$GLASS_RESISTANCE,
        c(.36845478947368415, .18422739473684208, .36845478947368415)
    )
    expect_equal(length(unique(source$TYPE_CONSTRUCTION_NAME)), 3L)
    object <- const__window_type_objects(source)
    expect_equal(nrow(object$glazing), 3L)
    expect_equal(nrow(object$construction), 6L)
    ep <- eplusr::empty_idf("9.2.0")
    converted <- window__equivalent_objects(db, ep, object$glazing)
    expect_equal(nrow(converted$object), 24L)
    fields <- converted$value
    front <- fields[
        fields$field_name == "Front Side Infrared Hemispherical Emissivity",
        "value_num"
    ][[1L]]
    back <- fields[
        fields$field_name == "Back Side Infrared Hemispherical Emissivity",
        "value_num"
    ][[1L]]
    expect_equal(front, c(.84, .84, .84, .84, .2, .84))
    expect_equal(back, c(.84, .84, .84, .84, .84, .2))
    conductivity <- fields[fields$field_name == "Conductivity", "value_num"][[
        1L
    ]]
    expect_equal(
        conductivity,
        rep(
            c(.016284223116140367, .032568446232280734, .016284223116140367),
            each = 2L
        )
    )
    expect_false(any(
        converted$object$class_name == "WindowMaterial:SimpleGlazingSystem"
    ))
    DBI::dbExecute(db, "UPDATE SURFACE SET BLACKNESS=.84 WHERE SURFACE_ID=3")
    shared <- const__window_type_performance(db)
    expect_equal(length(unique(shared$TYPE_CONSTRUCTION_NAME)), 2L)
})

test_that("invalid saved row factors and emissivities are refused", {
    db <- window_test__performance_db()
    on.exit(DBI::dbDisconnect(db))
    for (invalid in c("0", "-1", "NULL")) {
        DBI::dbExecute(
            db,
            paste0("UPDATE WINDOW SET K=", invalid, " WHERE ID=2")
        )
        expect_error(
            const__window_type_performance(db),
            class = "destep_invalid_window_factor"
        )
    }
    DBI::dbExecute(db, "UPDATE WINDOW SET K=2 WHERE ID=2")
    DBI::dbExecute(db, "UPDATE SURFACE SET BLACKNESS=1.2 WHERE SURFACE_ID=3")
    expect_error(
        const__window_type_performance(db),
        class = "destep_invalid_window_emissivity"
    )
})

test_that("other layer counts remain an explicit aggregate approximation", {
    db <- window_test__performance_db()
    on.exit(DBI::dbDisconnect(db))
    DBI::dbExecute(db, "UPDATE WINDOW_TYPE_DATA SET LAYER_NUM=3")
    source <- const__window_type_performance(db)
    expect_false(any(source$DETAILED_EQUIVALENT))
    expect_equal(length(unique(source$TYPE_CONSTRUCTION_NAME)), 2L)
    expect_equal(
        source$GLASS_RESISTANCE[source$WINDOW_ID == 2L],
        (1 / 1.9 - 1 / 8.7 - 1 / 23.3) / 2
    )
    expect_true(all(is.finite(
        const__window_type_objects(source)$glazing$TARGET_U_FACTOR
    )))
})

test_that("saved visible fields do not control the verified positive N2 optics", {
    db <- window_test__performance_db()
    on.exit(DBI::dbDisconnect(db))
    DBI::dbExecute(
        db,
        paste(
            "UPDATE WINDOW_TYPE_DATA SET SC=.480459770114943,",
            "LIGHT_TRANS_RATIO=.78, LIGHT_REFLECT_RATIO=.8"
        )
    )
    source <- const__window_type_performance(db)
    expect_true(all(source$DETAILED_EQUIVALENT))
    expect_equal(source$SOURCE_LIGHT_TRANS_RATIO, rep(.78, 3L))
    expect_equal(source$SOURCE_LIGHT_REFLECT_RATIO, rep(.8, 3L))
    expect_equal(
        source$NORMAL_VISIBLE_TAU,
        rep(.418044085369104, 3L),
        tolerance = 1e-12
    )
    expect_equal(
        source$NORMAL_VISIBLE_RHO,
        rep(.471562679700904, 3L),
        tolerance = 1e-12
    )
    ep <- eplusr::empty_idf("9.2.0")
    fields <- window__equivalent_objects(
        db,
        ep,
        const__window_type_objects(source)$glazing
    )$value
    vt <- fields[
        fields$field_name == "Visible Transmittance at Normal Incidence",
        "value_num"
    ][[1L]]
    expect_equal(vt, rep(.418044085369104, 6L), tolerance = 1e-12)
    vr <- fields[
        grepl(
            "Side Visible Reflectance at Normal Incidence$",
            fields$field_name
        ),
        "value_num"
    ][[1L]]
    expect_equal(vr, rep(.471562679700904, 12L), tolerance = 1e-12)
    expect_true(all(grepl("not_used", source$SOURCE_VISIBLE_USAGE)))
    # Even nonphysical saved placeholders are metadata on this source branch.
    DBI::dbExecute(
        db,
        "UPDATE WINDOW_TYPE_DATA SET LIGHT_TRANS_RATIO=-2, LIGHT_REFLECT_RATIO=10"
    )
    changed <- const__window_type_performance(db)
    expect_equal(changed$NORMAL_VISIBLE_TAU, source$NORMAL_VISIBLE_TAU)
    expect_equal(changed$NORMAL_VISIBLE_RHO, source$NORMAL_VISIBLE_RHO)
    expect_equal(changed$GLASS_RESISTANCE, source$GLASS_RESISTANCE)
    expect_equal(changed$SOURCE_LIGHT_TRANS_RATIO, rep(-2, 3L))
    expect_equal(changed$SOURCE_LIGHT_REFLECT_RATIO, rep(10, 3L))
})

test_that("absent saved visible fields cannot block a complete positive N2 type", {
    db <- window_test__performance_db()
    on.exit(DBI::dbDisconnect(db))
    DBI::dbExecute(
        db,
        "ALTER TABLE WINDOW_TYPE_DATA DROP COLUMN LIGHT_TRANS_RATIO"
    )
    DBI::dbExecute(
        db,
        "ALTER TABLE WINDOW_TYPE_DATA DROP COLUMN LIGHT_REFLECT_RATIO"
    )
    source <- const__window_type_performance(db)
    expect_true(all(source$DETAILED_EQUIVALENT))
    expect_true(all(is.na(source$SOURCE_LIGHT_TRANS_RATIO)))
    expect_true(all(is.na(source$SOURCE_LIGHT_REFLECT_RATIO)))
    expect_equal(source$NORMAL_VISIBLE_TAU, source$NORMAL_SOLAR_TAU)
    expect_equal(source$NORMAL_VISIBLE_RHO, source$NORMAL_SOLAR_RHO)
})

test_that("positive N2 aggregate fallback retains SC-derived visible transmission", {
    db <- window_test__performance_db()
    on.exit(DBI::dbDisconnect(db))
    DBI::dbExecute(db, "UPDATE SURFACE SET BLACKNESS=0")
    source <- const__window_type_performance(db)
    expect_false(any(source$DETAILED_EQUIVALENT))
    # Losing a target emissivity representation does not change the native
    # thermal branch. Its exact film constant still applies to the fallback.
    expect_equal(
        source$GLASS_RESISTANCE,
        (1 / 1.9 - .157861) / c(1, 2, 1),
        tolerance = 1e-12
    )
    expect_equal(
        source$LIGHT_TRANS_RATIO,
        rep(window__n2_optics(.883)[["tau"]], 3L)
    )
    expect_equal(source$SOURCE_LIGHT_TRANS_RATIO, rep(.703, 3L))
    expect_true(all(is.na(source$NORMAL_SOLAR_TAU)))
    # Only the verified two-layer branch substitutes the saved visible input.
    DBI::dbExecute(db, "UPDATE WINDOW_TYPE_DATA SET LAYER_NUM=3")
    other <- const__window_type_performance(db)
    expect_equal(other$LIGHT_TRANS_RATIO, rep(.703, 3L))
    expect_true(all(is.na(other$NORMAL_VISIBLE_TAU)))
    expect_true(all(
        other$SOURCE_VISIBLE_USAGE == "unverified_native_visible_path"
    ))
    DBI::dbExecute(db, "UPDATE WINDOW_TYPE_DATA SET ID=-46, LAYER_NUM=2")
    DBI::dbExecute(db, "UPDATE WINDOW SET TYPE=-46")
    negative <- const__window_type_performance(db)
    expect_false(any(negative$DETAILED_EQUIVALENT))
    expect_equal(negative$LIGHT_TRANS_RATIO, rep(.703, 3L))
    expect_true(all(is.na(negative$NORMAL_VISIBLE_TAU)))
})

test_that("missing active window types cannot create an unverified stack fallback", {
    db <- window_test__performance_db()
    on.exit(DBI::dbDisconnect(db))
    DBI::dbExecute(db, "DROP TABLE WINDOW_TYPE_DATA")
    expect_error(
        const__window_type_performance(db),
        "missing WINDOW_TYPE_DATA table or required fields"
    )
    # A different saved construction cannot silently replace active K/SC data.
    DBI::dbExecute(db, "UPDATE WINDOW SET WINDOW_CONSTRUCTION=100")
    expect_error(
        const__window_type_performance(db),
        "missing WINDOW_TYPE_DATA table or required fields"
    )
})

test_that("solar modes retain source weights and unresolved window references", {
    db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(db))
    DBI::dbWriteTable(
        db,
        "WINDOW",
        data.frame(ID = 1:3, SUN_TRANS_DIST_MODE = c(1L, 7L, NA_integer_))
    )
    DBI::dbWriteTable(
        db,
        "DIST_MODE",
        data.frame(
            DIST_MODE_ID = 1L,
            DIST_AIR = 0,
            DIST_ROOF = .167,
            DIST_FLOOR = .642,
            DIST_AROUND = .156
        )
    )
    audit <- solar__source_audit(db)
    expect_equal(audit$bindings$WINDOW_ID, 1:3)
    expect_equal(audit$bindings$SURFACE_WEIGHT_SUM[1L], .965)
    expect_equal(audit$bindings$DIST_FLOOR[1L], .642)
    expect_equal(
        audit$bindings$SOURCE_STATUS,
        c("saved_mode_retained_for_audit", "unresolved_mode", "unresolved_mode")
    )
    expect_true(all(is.na(audit$bindings$DIST_FLOOR[2:3])))
    warning <- NULL
    withCallingHandlers(
        window__warn_transmitted_solar_distribution(db),
        warning = function(condition) {
            warning <<- condition
            invokeRestart("muffleWarning")
        }
    )
    expect_s3_class(warning, "destep_unsupported_solar_distribution")
    expect_equal(warning$distributions, audit$bindings)
    comments <- solar__comments(audit)
    expect_true(any(grepl(
        "weights=0/0.167/0.642/0.156",
        comments,
        fixed = TRUE
    )))
    expect_true(any(grepl(
        "mode=7 status=unresolved_mode",
        comments,
        fixed = TRUE
    )))
    DBI::dbExecute(db, "UPDATE DIST_MODE SET DIST_FLOOR=-.1")
    expect_equal(
        solar__source_audit(db)$bindings$SOURCE_STATUS[1L],
        "invalid_source_weights"
    )
})

test_that("missing mode schema and no windows have stable transparent diagnostics", {
    db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(db))
    DBI::dbWriteTable(db, "WINDOW", data.frame(ID = 1L))
    audit <- solar__source_audit(db)
    expect_equal(audit$bindings$SOURCE_STATUS, "missing_binding_field")
    expect_true(is.na(audit$bindings$MODE_ID))
    DBI::dbExecute(db, "DELETE FROM WINDOW")
    expect_equal(nrow(solar__source_audit(db)$bindings), 0L)
    expect_length(solar__comments(solar__source_audit(db)), 0L)
})

test_that("duplicate mode identities cannot disappear in an audit join", {
    db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(db))
    DBI::dbWriteTable(
        db,
        "WINDOW",
        data.frame(ID = 1L, SUN_TRANS_DIST_MODE = 1L)
    )
    DBI::dbWriteTable(
        db,
        "DIST_MODE",
        data.frame(
            DIST_MODE_ID = c(1L, 1L),
            DIST_AIR = 0,
            DIST_ROOF = 0,
            DIST_FLOOR = 1,
            DIST_AROUND = 0
        )
    )
    expect_error(
        solar__source_audit(db),
        class = "destep_invalid_solar_distribution"
    )
})

test_that("receiver metadata includes native floor and roof room-face types", {
    db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(db))
    DBI::dbWriteTable(
        db,
        "WINDOW",
        data.frame(
            ID = 872L,
            SUN_TRANS_DIST_MODE = 1L,
            SIDE1 = 873L,
            SIDE2 = 875L
        )
    )
    DBI::dbWriteTable(db, "ROOM", data.frame(ID = 199L, OF_ROOM_GROUP = 200L))
    DBI::dbWriteTable(
        db,
        "SURFACE",
        data.frame(
            SURFACE_ID = c(109L, 248L, 317L, 875L),
            OF_ROOM = 199L,
            TYPE = c(0L, 4L, 5L, 0L),
            AREA = c(21.6, 48, 48, 6),
            AZIMUTH = c(0, 999, -999, 270)
        )
    )
    DBI::dbWriteTable(
        db,
        "MAIN_ENCLOSURE",
        data.frame(SIDE1 = c(107L, 251L, 314L), SIDE2 = c(109L, 248L, 317L))
    )
    receivers <- solar__source_audit(db)$receivers
    expect_equal(nrow(receivers), 4L)
    expect_setequal(receivers$FAMILY, c("around", "floor", "roof"))
    expect_equal(sum(receivers$RAW_AREA), 123.6)
    expect_true(all(receivers$SCOPE == "single_room_group_saved_receivers"))
})
