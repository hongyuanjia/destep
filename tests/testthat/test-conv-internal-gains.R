test_that("can convert internal gains", {
    ep <- eplusr::empty_idf(23.1)
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(
        dest,
        "ROOM",
        data.frame(
            ID = 1L,
            NAME = "Room 101",
            AREA = 10,
            TYPE = 1L
        )
    )
    DBI::dbWriteTable(
        dest,
        "SCHEDULE_YEAR",
        data.frame(
            SCHEDULE_ID = 10L,
            NAME = "Always On"
        )
    )
    DBI::dbWriteTable(
        dest,
        "DIST_MODE",
        data.frame(
            DIST_MODE_ID = c(2L, 3L, 4L),
            DIST_AIR = c(0.5, 0.3, 0.7),
            DIST_AROUND = c(0.25, 0.28, 0.1),
            DIST_FLOOR = c(0.05, 0.35, 0.1),
            DIST_ROOF = c(0.2, 0.07, 0.1)
        )
    )
    DBI::dbWriteTable(
        dest,
        "ROOM_TYPE_DATA",
        data.frame(
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
        )
    )
    # Keep conflicting drawing-marker values in the fixture so the assertions
    # below prove that ROOM_TYPE_DATA, rather than these rows, controls Calload.
    DBI::dbWriteTable(
        dest,
        "OCCUPANT_GAINS",
        data.frame(
            GAIN_ID = 101L,
            NAME = "Marker People",
            OF_ROOM = 1L,
            SCHEDULE = 10L,
            PER_AREA = 1L,
            MAXNUMBER = 0.2,
            MINNUMBER = 0,
            HEAT_PER_PERSON = 40,
            DAMP_PER_PERSON = 0.1,
            MIN_REQUIRE_FRESH_AIR = 25,
            DIST_MODE = 2L
        )
    )
    DBI::dbWriteTable(
        dest,
        "LIGHT_GAINS",
        data.frame(
            GAIN_ID = 102L,
            NAME = "Marker Lights",
            OF_ROOM = 1L,
            SCHEDULE = 10L,
            PER_AREA = 1L,
            MAXPOWER = 10,
            MINPOWER = 1,
            HEAT_RATE = 0.9,
            DIST_MODE = 3L
        )
    )
    DBI::dbWriteTable(
        dest,
        "EQUIPMENT_GAINS",
        data.frame(
            GAIN_ID = 103L,
            NAME = "Marker Equipment",
            OF_ROOM = 1L,
            SCHEDULE = 10L,
            PER_AREA = 1L,
            MAXPOWER = 40,
            MINPOWER = 0,
            MAX_HUM = 0,
            MIN_HUM = 0,
            DIST_MODE = 4L
        )
    )

    gains <- internal_gains__convert(dest, ep)

    # Source metadata must come from the same minimum/variable definitions as
    # the emitted objects, preserving electric watts separately from room heat.
    sources <- attr(gains, "sources")
    expect_length(sources, 5L)
    people_sources <- Filter(function(s) s$kind == "people", sources)
    light_sources <- Filter(function(s) s$kind == "light", sources)
    expect_equal(vapply(people_sources, `[[`, numeric(1L), "design_power"), c(1, 0.5), ignore_attr = TRUE)
    expect_equal(vapply(light_sources, `[[`, numeric(1L), "design_power"), c(90, 18), ignore_attr = TRUE)
    expect_equal(light_sources[[1L]]$mode, list(air = 0.3, wall = 0.28, floor = 0.35, roof = 0.07))
    expect_equal(people_sources[[1L]]$sensible_heat, 61)
    expect_false(people_sources[[1L]]$temperature_dependent)

    expect_type(gains, "list")
    expect_named(gains, c("object", "value"))
    expect_equal(
        unique(gains$object$class_name),
        c("Schedule:Constant", "People", "Lights", "OtherEquipment", "ElectricEquipment")
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
    expect_equal(
        unique(gains$value$value_num[
            gains$value$class_name == "Lights" &
                gains$value$field_name == "Fraction Visible"
        ]),
        0
    )
    expect_equal(
        unique(gains$value$value_num[
            gains$value$class_name == "Lights" &
                gains$value$field_name == "Fraction Replaceable"
        ]),
        0
    )

    # The new mode must preserve existing People and latent inputs while
    # applying the nonzero minimum count at the correct heat-balance point.
    dynamic <- internal_gains__convert(dest, ep, "temperature_dependent")
    dynamic_sources <- Filter(function(s) s$kind == "people", attr(dynamic, "sources"))
    expect_true(all(vapply(dynamic_sources, `[[`, logical(1L), "temperature_dependent")))
    expect_equal(unique(unlist(lapply(dynamic_sources, `[[`, "companion_objects"))),
        "Room 101 People Temperature Correction")
    original_people <- gains$value[class_name == "People", .(field_name, value_chr, value_num)]
    updated_people <- dynamic$value[class_name == "People", .(field_name, value_chr, value_num)]
    expect_equal(updated_people, original_people)
    expect_equal(dynamic$value[class_name == "EnergyManagementSystem:ProgramCallingManager" &
        field_name == "EnergyPlus Model Calling Point", value_chr],
        "BeginZoneTimestepBeforeInitHeatBalance")
    expect_equal(dynamic$value[class_name == "OtherEquipment" &
        field_name == "Fraction Latent", value_num], rep(0, 3L))
    expect_equal(dynamic$value[class_name == "EnergyManagementSystem:InternalVariable" &
        field_name == "Internal Data Type", value_chr], "Zone Floor Area")
    expect_match(paste(dynamic$value[class_name == "EnergyManagementSystem:Program", value_chr], collapse = "\n"),
        "SET Count = 0.05")
    expect_true(all(grepl("temperature_dependent", unlist(dynamic$object[class_name == "People", comment]))))

    # Zero reference sensible and latent heat still permits a positive cold-
    # room feedback term; avoid the former 0/0 sensible-fraction input.
    DBI::dbExecute(dest, "UPDATE ROOM_TYPE_DATA SET O_HEAT_PER_PERSON=0, O_DAMP_PER_PERSON=0")
    zero <- internal_gains__convert_people(dest, ep, "temperature_dependent")
    expect_equal(zero$value[class_name == "People" & field_name == "Sensible Heat Fraction", value_num], c(1, 1))
    expect_error(internal_gains__convert_people(dest, ep, "guess"), "arg")
    # Existing moisture conversion remains available. Its new metadata must
    # explicitly prevent accidental use by the sensible-only projection.
    DBI::dbExecute(dest, "UPDATE ROOM_TYPE_DATA SET E_MAX_HUM=1")
    wet <- internal_gains__convert_electric_equipment(dest, ep)
    expect_match(attr(wet, "sources")[[1L]]$unsupported_reason, "Equipment moisture")
    expect_true(any(grepl("Moisture", wet$value$value_chr), na.rm = TRUE))
})

test_that("lighting heat ratio preserves electricity and scales minimum and variable heat", {
    ep <- eplusr::empty_idf(23.1)
    lights <- data.table::data.table(NAME = "Test Lights", ROOM_NAME = "Room", ROOM_AREA = 10,
        SCHEDULE_NAME = "Occupancy", METHOD = "Watts/Area", WATTS_PER_AREA = 12,
        MIN_WATTS_PER_AREA = 2, FRACTION_RADIANT = .7, HEAT_TO_ELECTRIC_RATIO = .9)
    correction <- internal_gains__light_ratio_values(lights, 1L,
        "watts_per_floor_area", "Minimum", ep)
    expect_equal(vapply(correction, `[[`, numeric(1L), "design_level"), c(-10, -2))
    expect_equal(vapply(correction, `[[`, character(1L), "schedule_name"), c("Occupancy", "Minimum"))
    expect_true(all(vapply(correction, `[[`, character(1L), "fuel_type") == "None"))
    for (fraction in c(0, .25, 1)) {
        electric <- 10 * (2 + 10 * fraction)
        correction_power <- correction[[1L]]$design_level * fraction + correction[[2L]]$design_level
        expect_equal(electric + correction_power, .9 * electric)
    }
    lights$HEAT_TO_ELECTRIC_RATIO <- 1
    expect_length(internal_gains__light_ratio_values(lights, 1L,
        "watts_per_floor_area", "Minimum", ep), 0L)
    lights$HEAT_TO_ELECTRIC_RATIO <- 0
    lights$METHOD <- "LightingLevel"
    lights$LIGHTING_LEVEL <- 100
    lights$MIN_LIGHTING_LEVEL <- 20
    zero <- internal_gains__light_ratio_values(lights, 1L,
        "watts_per_floor_area", "Minimum", ep)
    expect_equal(vapply(zero, `[[`, numeric(1L), "design_level"), c(-80, -20))
    for (invalid in c(NA_real_, NaN, Inf, -1)) {
        lights$HEAT_TO_ELECTRIC_RATIO <- invalid
        expect_error(internal_gains__light_ratio_values(lights, 1L,
            "watts_per_floor_area", "Minimum", ep), "finite and non-negative")
    }
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
    expect_identical(
        unname(internal_gains__people_field_names(old)),
        c(
            "Number of People",
            "People per Zone Floor Area",
            "Zone Floor Area per Person"
        )
    )
    expect_identical(
        unname(internal_gains__people_field_names(current)),
        c(
            "Number of People",
            "People per Floor Area",
            "Floor Area per Person"
        )
    )
    expect_error(internal_gains__people_temperature(NULL, old,
        data.table::data.table(BASE_SENSIBLE_HEAT = 53)), "9.1.0 or newer")
    expect_error(internal_gains__people_temperature(NULL, current,
        data.table::data.table(BASE_SENSIBLE_HEAT = -1)), "non-negative finite")
})

test_that("rejects internal gain minimum values above their maximum", {
    people <- data.frame(
        NAME = "Invalid People",
        SCHEDULE_NAME = "Always On",
        METHOD = "People",
        NUMBER_OF_PEOPLE = 1,
        MIN_NUMBER_OF_PEOPLE = 2
    )
    lights <- data.frame(
        NAME = "Invalid Lights",
        SCHEDULE_NAME = "Always On",
        METHOD = "LightingLevel",
        LIGHTING_LEVEL = 5,
        MIN_LIGHTING_LEVEL = 6
    )
    equipment <- data.frame(
        NAME = "Invalid Equipment",
        SCHEDULE_NAME = "Always On",
        METHOD = "EquipmentLevel",
        DESIGN_LEVEL = 10,
        MIN_DESIGN_LEVEL = 11
    )

    expect_error(
        internal_gains__people_values(people, 1L, "Minimum"),
        "Invalid People.*minimum.*exceeds maximum"
    )
    expect_error(
        internal_gains__light_values(
            lights,
            1L,
            "watts_per_floor_area",
            "Minimum"
        ),
        "Invalid Lights.*minimum.*exceeds maximum"
    )
    expect_error(
        internal_gains__equipment_values(
            equipment,
            1L,
            "watts_per_floor_area",
            "Minimum"
        ),
        "Invalid Equipment.*minimum.*exceeds maximum"
    )
})

test_that("equipment moisture preserves sensible gains and uses an unmetered source", {
    ep <- eplusr::empty_idf(23.1)
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(
        dest,
        "ROOM",
        data.frame(
            ID = 1L,
            NAME = "Room 101",
            TYPE = 1L
        )
    )
    DBI::dbWriteTable(
        dest,
        "SCHEDULE_YEAR",
        data.frame(
            SCHEDULE_ID = 10L,
            NAME = "Always On"
        )
    )
    DBI::dbWriteTable(
        dest,
        "DIST_MODE",
        data.frame(
            DIST_MODE_ID = 4L,
            DIST_AIR = 0.7
        )
    )
    DBI::dbWriteTable(
        dest,
        "ROOM_TYPE_DATA",
        data.frame(
            ID = 1L,
            E_SCHEDULE = 10L,
            E_PER_AREA = 0L,
            E_MAXPOWER = 40,
            E_MINPOWER = 0,
            E_MAX_HUM = 0.2,
            E_MIN_HUM = 0,
            E_DIST_MODE = 4L
        )
    )

    gain <- internal_gains__convert_electric_equipment(dest, ep)
    # Older targets lack the execution point needed to preserve source timing.
    expect_error(
        internal_gains__convert_electric_equipment(
            dest,
            eplusr::empty_idf("9.0.1")
        ),
        "Nonzero equipment moisture requires EnergyPlus 9.1.0 or newer"
    )
    expect_equal(sum(gain$object$class_name == "ElectricEquipment"), 1L)
    expect_equal(sum(gain$object$class_name == "OtherEquipment"), 1L)
    expect_equal(
        gain$value[
            class_name == "ElectricEquipment" &
                field_name == "Design Level",
            value_num
        ],
        40
    )
    expect_equal(
        gain$value[
            class_name == "ElectricEquipment" &
                field_name == "Fraction Latent",
            value_num
        ],
        0
    )
    expect_equal(
        gain$value[
            class_name == "OtherEquipment" &
                field_name == "Fuel Type",
            value_chr
        ],
        "None"
    )
    expect_equal(
        gain$value[
            class_name == "OtherEquipment" &
                field_name == "Fraction Latent",
            value_num
        ],
        1
    )
    expect_equal(
        gain$value[
            class_name == "OtherEquipment" &
                field_name %in% c("Fraction Radiant", "Fraction Lost"),
            value_num
        ],
        c(0, 0)
    )
    expect_equal(
        sum(
            gain$object$class_name == "EnergyManagementSystem:InternalVariable"
        ),
        0L
    )

    # A pure moisture source with a nonzero minimum must also be supported.
    DBI::dbExecute(
        dest,
        paste(
            "UPDATE ROOM_TYPE_DATA SET E_MAXPOWER=0, E_MINPOWER=0,",
            "E_MIN_HUM=0.1, E_PER_AREA=1"
        )
    )
    area_gain <- internal_gains__convert_electric_equipment(dest, ep)
    expect_equal(
        area_gain$value[
            class_name == "OtherEquipment" &
                field_name == "Design Level Calculation Method",
            value_chr
        ],
        "Watts/Area"
    )
    expect_equal(
        area_gain$value[
            class_name == "EnergyManagementSystem:InternalVariable" &
                field_name == "Internal Data Type",
            value_chr
        ],
        "Zone Floor Area"
    )

    # Unsupported negative sources must not disappear through the ACTIVE filter.
    DBI::dbExecute(
        dest,
        "UPDATE ROOM_TYPE_DATA SET E_MAX_HUM=0, E_MIN_HUM=-0.1"
    )
    expect_error(
        internal_gains__convert_electric_equipment(dest, ep),
        "Invalid ROOM_TYPE_DATA equipment moisture generation"
    )
    DBI::dbExecute(
        dest,
        "UPDATE ROOM_TYPE_DATA SET E_MAX_HUM=0.1, E_MIN_HUM=0.2"
    )
    expect_error(
        internal_gains__convert_electric_equipment(dest, ep),
        "MIN_HUM <= MAX_HUM"
    )

    # Zero-moisture equipment keeps its original older-version support.
    DBI::dbExecute(
        dest,
        "UPDATE ROOM_TYPE_DATA SET E_MAX_HUM=0, E_MIN_HUM=0, E_MAXPOWER=40"
    )
    dry_gain <- internal_gains__convert_electric_equipment(
        dest,
        eplusr::empty_idf("9.0.1")
    )
    expect_equal(unique(dry_gain$object$class_name), "ElectricEquipment")
})

test_that("equipment moisture rejects non-finite and inverted source ranges", {
    for (pair in list(c(Inf, 0), c(NaN, 0), c(0, -1), c(1, 2), c(-1, 0))) {
        equipment <- data.table::data.table(
            NAME = "Invalid moisture",
            MAX_HUM = pair[[1L]],
            MIN_HUM = pair[[2L]]
        )
        expect_error(equipment__assert_moisture(equipment), "Invalid moisture")
    }
    expect_silent(equipment__assert_moisture(data.table::data.table(
        NAME = "Absent moisture",
        MAX_HUM = NA_real_,
        MIN_HUM = NA_real_
    )))
})

test_that("can convert internal gains from a real DeST model", {
    skip_on_cran()

    ep <- eplusr::empty_idf(23.1)
    src <- ensure_dest_sqlite_file()
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

    expect_equal(
        sum(gains$object$class_name == "People"),
        expected$PEOPLE[[1L]]
    )
    expect_equal(
        sum(gains$object$class_name == "Lights"),
        expected$LIGHTS[[1L]]
    )
    expect_equal(
        sum(gains$object$class_name == "ElectricEquipment"),
        expected$EQUIPMENT[[1L]]
    )
    expect_equal(unique(attr(gains, "table")$SOURCE_TABLE), "ROOM_TYPE_DATA")
    # A real DeST schema must supply metadata for every emitted primary gain,
    # including any nonzero minimum, directly from its source distribution.
    sources <- attr(gains, "sources")
    expect_length(sources, sum(gains$object$class_name %in% c("People", "Lights", "ElectricEquipment")))
    expect_true(all(vapply(sources, function(s)
        is.finite(s$design_power) && s$design_power >= 0 &&
            identical(names(s$mode), c("air", "wall", "floor", "roof")), logical(1L))))
})
