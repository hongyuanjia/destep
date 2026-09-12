test_that("converts exterior and interzone DeST doors", {
    ep <- eplusr::empty_idf(23.1)
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(
        dest,
        "DEFAULT_SETTING",
        data.table::data.table(
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
                "Outside Door Face",
                "Room Door Face"
            ),
            TYPE = c(1L, 0L, 1L, 0L),
            AZIMUTH = c(180, 0, 180, 0),
            TILT = c(90, 90, 90, 90),
            ABSORB_COEF = c(0.7, 0.7, 0.0, 0.0),
            BLACKNESS = c(0.9, 0.9, 0.0, 0.0),
            VENTILATION_COEF = c(21.6, 1.8, 0.0, 0.0)
        )
    )
    DBI::dbWriteTable(
        dest,
        "MAIN_ENCLOSURE",
        data.table::data.table(
            ID = 100L,
            SIDE1 = 10L,
            SIDE2 = 20L
        )
    )
    DBI::dbWriteTable(
        dest,
        "SYS_DOOR",
        data.table::data.table(
            DOOR_ID = 4L,
            CNAME = "Opaque Door"
        )
    )
    DBI::dbWriteTable(
        dest,
        "DOOR",
        data.table::data.table(
            ID = 200L,
            NAME = "Door A",
            SIDE1 = 11L,
            SIDE2 = 21L,
            OF_ENCLOSURE = 100L,
            MIDDLE_PLANE = 300L,
            DOOR_CONSTRUCTION = 4L
        )
    )
    DBI::dbWriteTable(
        dest,
        "PLANE",
        data.table::data.table(
            PLANE_ID = 300L,
            GEOMETRY = 400L
        )
    )
    DBI::dbWriteTable(
        dest,
        "GEOMETRY",
        data.table::data.table(
            GEOMETRY_ID = 400L,
            BOUNDARY_LOOP_ID = 500L
        )
    )
    DBI::dbWriteTable(
        dest,
        "LOOP_POINT",
        data.table::data.table(
            LOOP_ID = 500L,
            POINT_NO = 0:3,
            POINT = 1:4
        )
    )
    DBI::dbWriteTable(
        dest,
        "POINT",
        data.table::data.table(
            POINT_ID = 1:4,
            X = c(1, 2, 2, 1),
            Y = c(0, 0, 0, 0),
            Z = c(1, 1, 3, 3)
        )
    )

    converted <- door__convert(dest, ep)
    table <- attr(converted, "table")

    expect_setequal(
        unique(converted$object$class_name),
        c(
            "FenestrationSurface:Detailed",
            "SurfaceProperty:ConvectionCoefficients"
        )
    )
    expect_equal(
        converted$value[field_name == "Surface Type", value_chr],
        "Door"
    )
    expect_equal(unique(table$SURFACE_NAME), "Room Wall")
    expect_equal(unique(table$CONSTRUCTION), "Opaque Door")
    expect_equal(table$POINT_X, c(2, 2, 1, 1))
    expect_equal(table$POINT_Z, c(3, 1, 1, 3))
    expect_equal(
        table[, geom__polygon_area(.SD), by = OUTPUT_PART_ID]$V1,
        2.0
    )
    convection <- converted$value[
        class_name == "SurfaceProperty:ConvectionCoefficients"
    ]
    expect_equal(
        convection[field_name == "Convection Coefficient 1", value_num],
        1.8
    )
    expect_equal(
        convection[field_name == "Convection Coefficient 2", value_num],
        21.6
    )

    # Door constructions inherit the effective properties of the owning
    # enclosure instead of the placeholder values on raw door-face records.
    DBI::dbExecute(
        dest,
        "UPDATE SURFACE SET ABSORB_COEF = 0.1 WHERE SURFACE_ID = 10"
    )
    DBI::dbExecute(
        dest,
        "UPDATE SURFACE SET ABSORB_COEF = 0.6 WHERE SURFACE_ID = 20"
    )
    properties <- attr(door__convert(dest, ep), "table")
    expect_equal(unique(properties$INSIDE_SOLAR_ABSORPTANCE), 0.6)
    expect_equal(unique(properties$OUTSIDE_SOLAR_ABSORPTANCE), 0.1)
    expect_match(
        unique(properties$CONSTRUCTION),
        "Opaque Door [DeST i-a0.6-e0.9 o-a0.1-e0.9]",
        fixed = TRUE
    )
    DBI::dbExecute(
        dest,
        "UPDATE SURFACE SET ABSORB_COEF = 0.7 WHERE SURFACE_ID IN (10, 20)"
    )

    # A shared door must produce reciprocal subsurfaces with opposite layer
    # directions and explicit outside-boundary references.
    DBI::dbExecute(dest, "UPDATE SURFACE SET TYPE = 0")
    DBI::dbExecute(
        dest,
        "UPDATE SURFACE SET NAME = 'Room A Wall' WHERE SURFACE_ID = 10"
    )
    DBI::dbExecute(
        dest,
        "UPDATE SURFACE SET NAME = 'Room B Wall' WHERE SURFACE_ID = 20"
    )
    pair <- door__convert(dest, ep)
    pair_table <- unique(attr(pair, "table")[, .(
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
    expect_setequal(pair_table$NAME, c("Door A [1]", "Door A [2]"))
    expect_equal(
        pair_table$BOUNDARY_OBJECT[match(
            c("Door A [1]", "Door A [2]"),
            pair_table$NAME
        )],
        c("Door A [2]", "Door A [1]")
    )
    expect_setequal(
        pair_table$SURFACE_NAME,
        c("Room A Wall", "Room B Wall")
    )
    expect_equal(
        unique(pair_table[SIDE == 1L]$CONSTRUCTION),
        "Opaque Door [Reverse]"
    )
    expect_equal(
        unique(pair_table[SIDE == 2L]$CONSTRUCTION),
        "Opaque Door"
    )
})

test_that("skips door conversion without DOOR records", {
    ep <- eplusr::empty_idf(23.1)
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    expect_null(door__convert(dest, ep))
})
