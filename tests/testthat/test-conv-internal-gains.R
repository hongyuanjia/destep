test_that("can convert internal gains", {
    ep <- eplusr::empty_idf(23.1)
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(dest, "ROOM", data.frame(
        ID = 1L,
        NAME = "Room 101",
        TYPE = 1L
    ))
    DBI::dbWriteTable(dest, "SCHEDULE_YEAR", data.frame(
        SCHEDULE_ID = 10L,
        NAME = "Always On"
    ))
    DBI::dbWriteTable(dest, "DIST_MODE", data.frame(
        DIST_MODE_ID = c(2L, 3L, 4L),
        DIST_AIR = c(0.5, 0.3, 0.7)
    ))
    DBI::dbWriteTable(dest, "ROOM_TYPE_DATA", data.frame(
        ID = 1L,
        NAME = "Dense Office",
        O_SCHEDULE = 10L,
        O_MAXNUMBER = 0.15,
        O_MINNUMBER = 0.05,
        O_HEAT_PER_PERSON = 61,
        O_DAMP_PER_PERSON = 109,
        O_MIN_REQUIRE_FRESH_AIR = 30,
        O_PER_AREA = 1L,
        O_DIST_MODE = 2L,
        L_SCHEDULE = 10L,
        L_MAXPOWER = 12,
        L_MINPOWER = 2,
        L_HEAT_RATE = 0.9,
        L_PER_AREA = 1L,
        L_DIST_MODE = 3L,
        E_SCHEDULE = 10L,
        E_MAXPOWER = 20,
        E_MINPOWER = 0,
        E_MAX_HUM = 0,
        E_MIN_HUM = 0,
        E_PER_AREA = 1L,
        E_DIST_MODE = 4L
    ))
    # Keep conflicting drawing-marker values in the fixture so the assertions
    # below prove that ROOM_TYPE_DATA, rather than these rows, controls Calload.
    DBI::dbWriteTable(dest, "OCCUPANT_GAINS", data.frame(
        GAIN_ID = 101L, NAME = "Marker People", OF_ROOM = 1L,
        SCHEDULE = 10L, PER_AREA = 1L, MAXNUMBER = 0.2,
        MINNUMBER = 0, HEAT_PER_PERSON = 40, DAMP_PER_PERSON = 0.1,
        MIN_REQUIRE_FRESH_AIR = 25, DIST_MODE = 2L
    ))
    DBI::dbWriteTable(dest, "LIGHT_GAINS", data.frame(
        GAIN_ID = 102L, NAME = "Marker Lights", OF_ROOM = 1L,
        SCHEDULE = 10L, PER_AREA = 1L, MAXPOWER = 10, MINPOWER = 1,
        HEAT_RATE = 0.9, DIST_MODE = 3L
    ))
    DBI::dbWriteTable(dest, "EQUIPMENT_GAINS", data.frame(
        GAIN_ID = 103L, NAME = "Marker Equipment", OF_ROOM = 1L,
        SCHEDULE = 10L, PER_AREA = 1L, MAXPOWER = 40, MINPOWER = 0,
        MAX_HUM = 0, MIN_HUM = 0, DIST_MODE = 4L
    ))

    gains <- internal_gains__convert(dest, ep)

    expect_type(gains, "list")
    expect_named(gains, c("object", "value"))
    expect_equal(
        unique(gains$object$class_name),
        c("Schedule:Constant", "People", "Lights", "ElectricEquipment")
    )
    expect_equal(
        unique(gains$value$value_chr[
            gains$value$class_name == "People" &
                gains$value$field_name == "Activity Level Schedule Name"
        ]),
        "Activity Level 136.69 W"
    )
    activity_object <- gains$value[
        class_name == "Schedule:Constant" &
            field_name == "Name" &
            value_chr == "Activity Level 136.69 W",
        rleid
    ]
    expect_equal(
        gains$value[
            rleid == activity_object & field_name == "Hourly Value",
            value_num
        ],
        61 + 109 * 2.5 / 3.6
    )
    expect_equal(
        unique(gains$value$value_num[
            gains$value$class_name == "People" &
                gains$value$field_name == "Sensible Heat Fraction"
        ]),
        61 / (61 + 109 * 2.5 / 3.6)
    )
    expect_equal(sum(gains$object$class_name == "People"), 2L)
    expect_equal(
        gains$value$value_chr[
            gains$value$class_name == "People" &
                gains$value$field_name == "Number of People Schedule Name"
        ],
        c("Always On", "Always On - DeST Minimum People")
    )
    expect_equal(
        gains$value$value_num[
            gains$value$class_name == "People" &
                gains$value$field_name == "People per Floor Area"
        ],
        c(0.10, 0.05)
    )
    expect_equal(sum(gains$object$class_name == "Lights"), 2L)
    expect_equal(
        gains$value$value_chr[
            gains$value$class_name == "Lights" &
                gains$value$field_name == "Schedule Name"
        ],
        c("Always On", "Always On - DeST Minimum Lights")
    )
    expect_equal(
        gains$value$value_num[
            gains$value$class_name == "Lights" &
                grepl("Watts per .*Floor Area", gains$value$field_name)
        ],
        c(10, 2)
    )
})

test_that("internal gains resolve target zone-reference fields", {
    skip_if_not("9.0.1" %in% eplusr::avail_eplus())

    old <- eplusr::empty_idf("9.0.1")
    current <- eplusr::empty_idf(23.1)
    classes <- c("People", "Lights", "ElectricEquipment")

    expect_identical(
        vapply(
            classes,
            function(class) internal_gains__zone_field_name(old, class),
            character(1L)
        ),
        stats::setNames(rep("Zone or ZoneList Name", 3L), classes)
    )
    expect_identical(
        vapply(
            classes,
            function(class) internal_gains__zone_field_name(current, class),
            character(1L)
        ),
        stats::setNames(
            rep("Zone or ZoneList or Space or SpaceList Name", 3L),
            classes
        )
    )
})

test_that("rejects internal gain minimum values above their maximum", {
    people <- data.frame(
        NAME = "Invalid People", SCHEDULE_NAME = "Always On",
        METHOD = "People", NUMBER_OF_PEOPLE = 1,
        MIN_NUMBER_OF_PEOPLE = 2
    )
    lights <- data.frame(
        NAME = "Invalid Lights", SCHEDULE_NAME = "Always On",
        METHOD = "LightingLevel", LIGHTING_LEVEL = 5,
        MIN_LIGHTING_LEVEL = 6
    )
    equipment <- data.frame(
        NAME = "Invalid Equipment", SCHEDULE_NAME = "Always On",
        METHOD = "EquipmentLevel", DESIGN_LEVEL = 10,
        MIN_DESIGN_LEVEL = 11
    )

    expect_error(
        internal_gains__people_values(people, 1L, "Minimum"),
        "Invalid People.*minimum.*exceeds maximum"
    )
    expect_error(
        internal_gains__light_values(lights, 1L, "watts_per_floor_area", "Minimum"),
        "Invalid Lights.*minimum.*exceeds maximum"
    )
    expect_error(
        internal_gains__equipment_values(
            equipment, 1L, "watts_per_floor_area", "Minimum"
        ),
        "Invalid Equipment.*minimum.*exceeds maximum"
    )
})

test_that("nonzero equipment moisture is rejected until it can be mapped", {
    ep <- eplusr::empty_idf(23.1)
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(dest, "ROOM", data.frame(
        ID = 1L,
        NAME = "Room 101",
        TYPE = 1L
    ))
    DBI::dbWriteTable(dest, "SCHEDULE_YEAR", data.frame(
        SCHEDULE_ID = 10L,
        NAME = "Always On"
    ))
    DBI::dbWriteTable(dest, "DIST_MODE", data.frame(
        DIST_MODE_ID = 4L,
        DIST_AIR = 0.7
    ))
    DBI::dbWriteTable(dest, "ROOM_TYPE_DATA", data.frame(
        ID = 1L,
        E_SCHEDULE = 10L,
        E_PER_AREA = 0L,
        E_MAXPOWER = 40,
        E_MINPOWER = 0,
        E_MAX_HUM = 0.2,
        E_MIN_HUM = 0,
        E_DIST_MODE = 4L
    ))

    expect_error(
        internal_gains__convert_electric_equipment(dest, ep),
        "Cannot convert nonzero ROOM_TYPE_DATA equipment moisture generation"
    )
})

test_that("can convert internal gains from a real DeST model", {
    skip_on_cran()

    ep <- eplusr::empty_idf(23.1)
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

    gains <- internal_gains__convert(dest, ep)
    expected <- DBI::dbGetQuery(
        dest,
        "
        SELECT
            SUM(
                CASE WHEN T.O_MAXNUMBER > 0 OR T.O_MINNUMBER > 0
                    THEN 1 + (T.O_MINNUMBER > 0) ELSE 0 END
            ) AS PEOPLE,
            SUM(
                CASE WHEN T.L_MAXPOWER > 0 OR T.L_MINPOWER > 0
                    THEN 1 + (T.L_MINPOWER > 0) ELSE 0 END
            ) AS LIGHTS,
            SUM(
                CASE WHEN T.E_MAXPOWER > 0 OR T.E_MINPOWER > 0
                    THEN 1 + (T.E_MINPOWER > 0) ELSE 0 END
            ) AS EQUIPMENT
        FROM ROOM R
        INNER JOIN ROOM_TYPE_DATA T
        ON R.TYPE = T.ID
        "
    )

    expect_equal(sum(gains$object$class_name == "People"), expected$PEOPLE[[1L]])
    expect_equal(sum(gains$object$class_name == "Lights"), expected$LIGHTS[[1L]])
    expect_equal(
        sum(gains$object$class_name == "ElectricEquipment"),
        expected$EQUIPMENT[[1L]]
    )
    expect_equal(unique(attr(gains, "table")$SOURCE_TABLE), "ROOM_TYPE_DATA")
})
