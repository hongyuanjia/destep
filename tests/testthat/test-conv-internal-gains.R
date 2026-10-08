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

    expect_warning(
        gains <- internal_gains__convert(dest, ep),
        class = "destep_unsupported_gain_distribution"
    )

    expect_type(gains, "list")
    expect_named(gains, c("object", "value"))
    expect_equal(
        unique(gains$object$class_name),
        c(
            "Schedule:Constant",
            "People",
            "OtherEquipment",
            "EnergyManagementSystem:Sensor",
            "EnergyManagementSystem:InternalVariable",
            "EnergyManagementSystem:Actuator",
            "EnergyManagementSystem:Program",
            "EnergyManagementSystem:ProgramCallingManager",
            "Lights",
            "ElectricEquipment"
        )
    )
    expect_equal(
        unique(gains$value$value_chr[
            gains$value$class_name == "People" &
                gains$value$field_name == "Activity Level Schedule Name"
        ]),
        "People Sensible Heat 61 W"
    )
    activity_object <- gains$value[
        class_name == "Schedule:Constant" &
            field_name == "Name" &
            value_chr == "People Sensible Heat 61 W",
        rleid
    ]
    expect_equal(
        gains$value[
            rleid == activity_object & field_name == "Hourly Value",
            value_num
        ],
        61
    )
    expect_equal(
        unique(gains$value$value_num[
            gains$value$class_name == "People" &
                gains$value$field_name == "Sensible Heat Fraction"
        ]),
        1
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

    # Nominal sensible inputs do not acquire a temperature-dependent correction;
    # the only People EMS serves the independent prescribed moisture source.
    expect_true(any(
        grepl(
            "Room 101 People Moisture",
            gains$value$value_chr,
            fixed = TRUE
        ),
        na.rm = TRUE
    ))
    programs <- gains$value[
        class_name == "EnergyManagementSystem:Program",
        value_chr
    ]
    expect_true(any(grepl("HgAirFnWTdb", programs), na.rm = TRUE))
    expect_false(any(
        grepl(
            "5.536|DeST_People_T_|Temperature Correction",
            gains$value$value_chr
        ),
        na.rm = TRUE
    ))
    expect_equal(
        gains$value[
            class_name == "EnergyManagementSystem:ProgramCallingManager" &
                field_name == "EnergyPlus Model Calling Point",
            value_chr
        ],
        "BeginZoneTimestepBeforeInitHeatBalance"
    )

    # A zero nominal heat input stays zero, without a cold-room heat correction.
    DBI::dbExecute(
        dest,
        "UPDATE ROOM_TYPE_DATA SET O_HEAT_PER_PERSON=0, O_DAMP_PER_PERSON=0"
    )
    zero <- people__convert(dest, ep)
    expect_equal(
        zero$value[
            class_name == "People" & field_name == "Sensible Heat Fraction",
            value_num
        ],
        c(1, 1)
    )
    expect_equal(
        zero$value[
            class_name == "Schedule:Constant" & field_name == "Hourly Value",
            value_num
        ],
        c(0, 1)
    )
    expect_false(any(grepl(
        "EnergyManagementSystem|OtherEquipment",
        zero$object$class_name
    )))
    # Moisture remains an independent source after removing redistribution.
    DBI::dbExecute(dest, "UPDATE ROOM_TYPE_DATA SET E_MAX_HUM=1")
    wet <- equipment__convert(dest, ep)
    expect_true(any(grepl("Moisture", wet$value$value_chr), na.rm = TRUE))

    # Negative source counts/power must not disappear as inactive rows or
    # become an inflated max-minus-min source with its negative minimum lost.
    original <- DBI::dbReadTable(dest, "ROOM_TYPE_DATA")
    converters <- list(
        O = people__convert,
        L = light__convert,
        E = equipment__convert
    )
    for (prefix in names(converters)) {
        fields <- paste0(
            prefix,
            if (prefix == "O") {
                c("_MINNUMBER", "_MAXNUMBER")
            } else {
                c("_MINPOWER", "_MAXPOWER")
            }
        )
        for (basis in 0:1) {
            for (bounds in list(c(-1, 10), c(-2, -1), c(0, -1))) {
                row <- original
                row[[fields[[1L]]]] <- bounds[[1L]]
                row[[fields[[2L]]]] <- bounds[[2L]]
                row[[paste0(prefix, "_PER_AREA")]] <- basis
                row$E_MAX_HUM <- 0
                DBI::dbWriteTable(dest, "ROOM_TYPE_DATA", row, overwrite = TRUE)
                expect_error(converters[[prefix]](dest, ep), "nonnegative")
            }
        }
    }
})

test_that("lighting heat ratio preserves electricity and scales minimum and variable heat", {
    ep <- eplusr::empty_idf(23.1)
    lights <- data.table::data.table(
        NAME = "Test Lights",
        ROOM_NAME = "Room",
        ROOM_AREA = 10,
        SCHEDULE_NAME = "Occupancy",
        METHOD = "Watts/Area",
        WATTS_PER_AREA = 12,
        MIN_WATTS_PER_AREA = 2,
        FRACTION_RADIANT = .7,
        HEAT_TO_ELECTRIC_RATIO = .9
    )
    # Use the same prebuilt Lights values as the owning converter.
    make_correction <- function(lights) {
        source <- light__values(lights, 1L, "watts_per_floor_area", "Minimum")
        light__ratio_values(
            lights,
            1L,
            source,
            "watts_per_floor_area",
            conv__idd_field_name(ep, "OtherEquipment", 3L)
        )
    }
    correction <- make_correction(lights)
    expect_equal(
        vapply(correction, `[[`, numeric(1L), "design_level"),
        c(-10, -2)
    )
    expect_equal(
        vapply(correction, `[[`, character(1L), "schedule_name"),
        c("Occupancy", "Minimum")
    )
    expect_true(all(
        vapply(correction, `[[`, character(1L), "fuel_type") == "None"
    ))
    for (fraction in c(0, .25, 1)) {
        electric <- 10 * (2 + 10 * fraction)
        correction_power <- correction[[1L]]$design_level *
            fraction +
            correction[[2L]]$design_level
        expect_equal(electric + correction_power, .9 * electric)
    }
    lights$HEAT_TO_ELECTRIC_RATIO <- 1
    expect_length(
        make_correction(lights),
        0L
    )
    lights$HEAT_TO_ELECTRIC_RATIO <- 0
    lights$METHOD <- "LightingLevel"
    lights$LIGHTING_LEVEL <- 100
    lights$MIN_LIGHTING_LEVEL <- 20
    zero <- make_correction(lights)
    expect_equal(vapply(zero, `[[`, numeric(1L), "design_level"), c(-80, -20))
    for (invalid in c(NA_real_, NaN, Inf, -1)) {
        lights$HEAT_TO_ELECTRIC_RATIO <- invalid
        expect_error(
            make_correction(lights),
            "finite and non-negative"
        )
    }
})

test_that("lighting corrections preserve reusable values and area diagnostics", {
    lights <- data.table::data.table(
        NAME = "Light",
        ROOM_NAME = "Room",
        ROOM_AREA = 10,
        METHOD = "Watts/Area",
        HEAT_TO_ELECTRIC_RATIO = 1.2
    )
    source <- list(list(
        name = "Light Minimum",
        schedule_name = "Minimum",
        watts_per_floor_area = 2,
        fraction_radiant = .7
    ))
    before <- serialize(list(lights, source), NULL)
    value <- light__ratio_values(
        lights,
        1L,
        source,
        "watts_per_floor_area",
        "Zone Name"
    )
    expect_equal(value[[1L]]$design_level, 4)
    expect_identical(value[[1L]][["Zone Name"]], "Room")
    expect_identical(serialize(list(lights, source), NULL), before)
    for (area in c(0, -1, NA_real_, NaN, Inf)) {
        data.table::set(lights, j = "ROOM_AREA", value = area)
        expect_error(
            light__ratio_values(
                lights,
                1L,
                source,
                "watts_per_floor_area",
                "Zone Name"
            ),
            "requires a positive finite room area"
        )
    }
    expect_identical(
        light__ratio_values(
            lights,
            1L,
            list(),
            "watts_per_floor_area",
            "Zone Name"
        ),
        list()
    )
    data.table::set(lights, j = "HEAT_TO_ELECTRIC_RATIO", value = 1)
    expect_identical(
        light__ratio_values(lights, 1L, source, "watts_per_floor_area", NULL),
        list()
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
    expect_identical(
        unname(people__field_names(old)),
        c(
            "Number of People",
            "People per Zone Floor Area",
            "Zone Floor Area per Person"
        )
    )
    expect_identical(
        unname(people__field_names(current)),
        c(
            "Number of People",
            "People per Floor Area",
            "Floor Area per Person"
        )
    )
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
        people__values(people, 1L, "Minimum"),
        "Invalid People.*minimum.*exceeds maximum"
    )
    expect_error(
        light__values(
            lights,
            1L,
            "watts_per_floor_area",
            "Minimum"
        ),
        "Invalid Lights.*minimum.*exceeds maximum"
    )
    expect_error(
        equipment__values(
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

    gain <- equipment__convert(dest, ep)
    # Older targets lack the execution point needed to preserve source timing.
    expect_error(
        equipment__convert(
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
    area_gain <- equipment__convert(dest, ep)
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
        equipment__convert(dest, ep),
        "Invalid ROOM_TYPE_DATA equipment moisture generation"
    )
    DBI::dbExecute(
        dest,
        "UPDATE ROOM_TYPE_DATA SET E_MAX_HUM=0.1, E_MIN_HUM=0.2"
    )
    expect_error(
        equipment__convert(dest, ep),
        "MIN_HUM <= MAX_HUM"
    )

    # Zero-moisture equipment keeps its original older-version support.
    DBI::dbExecute(
        dest,
        "UPDATE ROOM_TYPE_DATA SET E_MAX_HUM=0, E_MIN_HUM=0, E_MAXPOWER=40"
    )
    dry_gain <- equipment__convert(
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

    expect_warning(
        gains <- internal_gains__convert(dest, ep),
        class = "destep_unsupported_gain_distribution"
    )
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
})
