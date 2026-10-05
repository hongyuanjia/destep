# A minimal active room type isolates source ownership and units without a
# large model whose other changes could mask a conversion error.
input_test__database <- function() {
    db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbWriteTable(
        db,
        "ROOM",
        data.frame(ID = 1L, NAME = "Input room", TYPE = 1L, AREA = 20)
    )
    DBI::dbWriteTable(
        db,
        "SCHEDULE_YEAR",
        data.frame(SCHEDULE_ID = 1L, NAME = "Source schedule")
    )
    DBI::dbWriteTable(
        db,
        "DIST_MODE",
        data.frame(DIST_MODE_ID = 1L, DIST_AIR = 0.87654321)
    )
    DBI::dbWriteTable(
        db,
        "ROOM_TYPE_DATA",
        data.frame(
            ID = 1L,
            O_SCHEDULE = 1L,
            O_MAXNUMBER = 2,
            O_MINNUMBER = 0.5,
            O_PER_AREA = 0L,
            O_HEAT_PER_PERSON = 61,
            O_DAMP_PER_PERSON = 109,
            O_MIN_REQUIRE_FRESH_AIR = 0,
            O_DIST_MODE = 1L,
            L_SCHEDULE = 1L,
            L_MAXPOWER = 10,
            L_MINPOWER = 0,
            L_PER_AREA = 0L,
            L_HEAT_RATE = 1,
            L_DIST_MODE = 1L,
            E_SCHEDULE = 1L,
            E_MAXPOWER = 20,
            E_MINPOWER = 0,
            E_PER_AREA = 0L,
            E_MAX_HUM = 0,
            E_MIN_HUM = 0,
            E_DIST_MODE = 1L
        )
    )
    db
}

test_that("source radiant fractions retain all input digits", {
    db <- input_test__database()
    on.exit(DBI::dbDisconnect(db))
    ep <- eplusr::empty_idf("23.1")
    gains <- internal_gains__convert(db, ep)
    for (class in c("People", "Lights", "ElectricEquipment")) {
        values <- gains$value
        actual <- values$value_num[
            values$class_name == class & values$field_name == "Fraction Radiant"
        ]
        expect_true(length(actual) > 0)
        expect_equal(
            actual,
            rep(1 - 0.87654321, length(actual)),
            tolerance = 1e-14
        )
    }
})

test_that("people mass source preserves counts units and version restrictions", {
    db <- input_test__database()
    on.exit(DBI::dbDisconnect(db))
    ep <- eplusr::empty_idf("23.1")
    gain <- people__convert(db, ep)
    values <- gain$value
    lines <- values$value_chr[
        values$class_name == "EnergyManagementSystem:Program"
    ]
    expect_true(any(grepl("SET MassRate =", lines, fixed = TRUE)))
    expect_true(any(grepl("@HgAirFnWTdb", lines, fixed = TRUE)))
    # Frozen kg/s coefficients from 0.5 and (2-0.5) persons at 109 g/h/person.
    expect_true(
        sprintf(
            "SET MassRate = %.17g + %.17g * DeST_People_Moisture_1_Schedule",
            0.5 * 109 / 1000 / 3600,
            1.5 * 109 / 1000 / 3600
        ) %in%
            lines
    )
    expect_equal(
        values$value_num[
            values$class_name == "People" &
                values$field_name == "Sensible Heat Fraction"
        ],
        c(1, 1)
    )
    expect_equal(
        values$value_chr[
            values$class_name == "OtherEquipment" &
                values$field_name == "Fuel Type"
        ],
        "None"
    )
    expect_error(
        people__convert(db, eplusr::empty_idf("9.0.1")),
        "Nonzero people moisture requires EnergyPlus 9.1"
    )

    DBI::dbExecute(
        db,
        "UPDATE ROOM_TYPE_DATA SET O_PER_AREA=1, O_MAXNUMBER=0.1, O_MINNUMBER=0.025"
    )
    area <- people__convert(db, ep)
    expect_true(
        "SET MassRate = MassRate * DeST_People_Moisture_1_Area" %in%
            area$value$value_chr
    )
    DBI::dbExecute(db, "UPDATE ROOM_TYPE_DATA SET O_DAMP_PER_PERSON=-1")
    expect_error(people__convert(db, ep), "Invalid O_DAMP_PER_PERSON")
    DBI::dbExecute(db, "UPDATE ROOM_TYPE_DATA SET O_DAMP_PER_PERSON=0")
    dry <- people__convert(db, eplusr::empty_idf("9.0.1"))
    expect_false(any(dry$object$class_name == "OtherEquipment"))
    DBI::dbExecute(db, "UPDATE ROOM_TYPE_DATA SET O_MAXNUMBER=0, O_MINNUMBER=0")
    expect_null(people__convert(db, ep))
    for (invalid in c(Inf, NaN, -1)) {
        expect_error(
            people__moisture_objects(
                db,
                ep,
                data.table::data.table(
                    NAME = "Bad source",
                    MOISTURE_GRAMS_PER_HOUR = invalid
                )
            ),
            "Invalid O_DAMP_PER_PERSON"
        )
    }
})

test_that("automatic soil is idempotent per construction and leaves physical soil intact", {
    layers <- data.table::data.table(
        ID = c(1L, 2L),
        KIND = 4L,
        NAME = c("First", "Second"),
        LAYER_NO = 0L,
        LENGTH = 1200,
        MATERIAL_ID = c(-900000004L, 12L),
        MATERIAL_NAME = c("DeST Automatic Soil", "Physical soil"),
        MATERIAL_CONDUCTIVITY = 0.93,
        MATERIAL_DENSITY = 1800,
        MATERIAL_SPECIFIC_HEAT = 1010
    )
    before <- data.table::copy(layers)
    expect_warning(
        result <- const__append_dest_ground_soil(layers),
        "explicit layer matching DeST automatic soil"
    )
    expect_identical(layers, before)
    expect_equal(nrow(result), 3L)
    expect_equal(
        result$ID[result$MATERIAL_NAME == "DeST Automatic Soil"],
        c(1L, 2L)
    )
    expect_true("Physical soil" %in% result$MATERIAL_NAME)
    expect_identical(const__append_dest_ground_soil(result), result)
    expect_equal(nrow(const__append_dest_ground_soil(layers[0])), 0L)
})

test_that("unverified underground wall stacks have an actionable diagnostic", {
    db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(db))
    DBI::dbWriteTable(
        db,
        "MAIN_ENCLOSURE",
        data.frame(ID = 42L, KIND = 1L, SIDE1 = 1L, SIDE2 = 2L)
    )
    DBI::dbWriteTable(
        db,
        "SURFACE",
        data.frame(SURFACE_ID = 1:2, TYPE = c(0L, 2L))
    )
    expect_error(const__assert_ground_scope(db), "42 \\(KIND=1\\)")
    DBI::dbExecute(db, "UPDATE MAIN_ENCLOSURE SET KIND=4")
    expect_silent(const__assert_ground_scope(db))
})

test_that("non-finite ground temperature is not converted to a monthly boundary", {
    data <- data.table::data.table(ID = 1, HOUR = 0:8759, T = 10)
    data.table::set(data, 1L, "T", Inf)
    expect_error(
        ground_temperature__validate_table(data, 1),
        "T contains non-finite"
    )
})
