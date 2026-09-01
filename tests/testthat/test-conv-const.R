## Create the smallest construction database needed to exercise DeST door
## layer selection without depending on a real model that contains no doors.
const_test__door_db <- function(
    door_construction = c(10L, 20L),
    enclosure = c(100L, 200L),
    material = c(1L, 2L),
    app_id = c(0L, 0L),
    app_flag = c(0L, 0L)
) {
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbWriteTable(
        dest,
        "DOOR",
        data.frame(
            ID = seq_along(door_construction),
            DOOR_CONSTRUCTION = door_construction,
            OF_ENCLOSURE = enclosure
        )
    )
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
        "SYS_DOOR",
        data.frame(
            DOOR_ID = door_construction,
            CNAME = paste("Door", door_construction),
            MATERIAL_ID = material,
            APP_ID = app_id,
            APP_FLAG = app_flag
        )
    )
    DBI::dbWriteTable(
        dest,
        "SYS_MATERIAL",
        data.frame(
            MATERIAL_ID = material,
            CNAME = paste("Opaque", material),
            CONDUCTIVITY = rep(0.5, length(material)),
            DENSITY = rep(800, length(material)),
            SPECIFIC_HEAT = rep(1000, length(material))
        )
    )
    DBI::dbWriteTable(
        dest,
        "SYS_APP_MATERIAL",
        data.frame(
            APP_MATERIAL_ID = integer(),
            CNAME = character(),
            THICK = double(),
            CONDUCTIVITY = double(),
            DENSITY = double(),
            SPECIFIC_HEAT = double(),
            GROUP_ID = character(),
            EX_COEF = double(),
            RF_COEF = double(),
            EMISSIVITY = double()
        )
    )
    DBI::dbWriteTable(
        dest,
        "MAIN_ENCLOSURE",
        data.frame(
            ID = enclosure,
            CONSTRUCTION = c(1000L, 2000L)[seq_along(enclosure)],
            KIND = rep(1L, length(enclosure))
        )
    )

    layer_tables <- c(
        "SYS_OUTWALL_MATERIAL",
        "SYS_INWALL_MATERIAL",
        "SYS_ROOF_MATERIAL",
        "SYS_GROUNDFLOOR_MATERIAL",
        "SYS_MIDDLEFLOOR_MATERIAL",
        "SYS_AIRFLOOR_MATERIAL"
    )
    empty_layer <- data.frame(
        STRUCT_ID = integer(),
        MATERIAL_ID = integer(),
        LAYER_NO = integer(),
        LENGTH = double()
    )
    for (table in layer_tables) {
        DBI::dbWriteTable(dest, table, empty_layer)
    }
    DBI::dbWriteTable(
        dest,
        "SYS_OUTWALL_MATERIAL",
        data.frame(
            STRUCT_ID = c(1000L, 2000L),
            MATERIAL_ID = material,
            LAYER_NO = 0L,
            LENGTH = c(120, 180)
        ),
        overwrite = TRUE
    )
    dest
}

test_that("keeps every distinct non-default door construction", {
    dest <- const_test__door_db()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    door <- const__door_layers(dest)
    opaque <- door[door$LAYER_NO == 0L, ]

    expect_equal(opaque$ID, c(10L, 20L))
    expect_equal(opaque$LENGTH, c(120, 180))
})

test_that("resolves transparent door materials through APP_ID", {
    dest <- const_test__door_db(
        door_construction = 10L,
        enclosure = 100L,
        material = 62L,
        app_id = 1L,
        app_flag = 1L
    )
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    DBI::dbWriteTable(
        dest,
        "SYS_APP_MATERIAL",
        data.frame(
            APP_MATERIAL_ID = 1L,
            CNAME = "3mm Clear Glass",
            THICK = 3,
            CONDUCTIVITY = 0.9,
            DENSITY = 2500,
            SPECIFIC_HEAT = 750,
            GROUP_ID = "普通玻璃",
            EX_COEF = 0.045,
            RF_COEF = 1.5,
            EMISSIVITY = 0.84
        ),
        overwrite = TRUE
    )

    door <- const__door_layers(dest)
    glazing <- door[door$LAYER_NO == 1L, ]

    expect_equal(glazing$MATERIAL_ID, 1L)
    expect_equal(glazing$MATERIAL_NAME, "3mm Clear Glass")
    expect_equal(glazing$LENGTH, 3)
    expect_equal(glazing$MATERIAL_CONDUCTIVITY, 0.9)
    expect_equal(glazing$MATERIAL_EXTINCTION_COEFFICIENT, 0.045)
    expect_equal(glazing$MATERIAL_REFRACTIVE_INDEX, 1.5)
    expect_equal(glazing$MATERIAL_EMISSIVITY, 0.84)
})

test_that("maps ordinary SYS_WINDOW glass from refraction inputs", {
    ep <- eplusr::empty_idf("9.0.1")
    material <- data.table::data.table(
        MATERIAL_ID = integer(),
        LENGTH = double(),
        MATERIAL_NAME = character(),
        MATERIAL_CONDUCTIVITY = double(),
        MATERIAL_DENSITY = double(),
        MATERIAL_SPECIFIC_HEAT = double(),
        THERMAL_ABSORPTANCE = double(),
        SOLAR_ABSORPTANCE = double(),
        VISIBLE_ABSORPTANCE = double()
    )
    glazing <- data.table::data.table(
        MATERIAL_ID = 1L,
        LENGTH = 3.0,
        MATERIAL_NAME = "3mm Clear Glass",
        MATERIAL_CONDUCTIVITY = 0.756,
        MATERIAL_DENSITY = 2500.0,
        MATERIAL_SPECIFIC_HEAT = 837.0,
        MATERIAL_GROUP = "普通玻璃",
        MATERIAL_EXTINCTION_COEFFICIENT = 0.045,
        MATERIAL_REFRACTIVE_INDEX = 1.5,
        MATERIAL_EMISSIVITY = 0.84
    )
    construction <- data.table::data.table(
        ID = 1L,
        KIND = -1L,
        name = "Single Window",
        value = list(c("Single Window", "3mm Clear Glass"))
    )

    converted <- const__assemble_objects(
        TRUE,
        ep,
        material,
        data.table::data.table(),
        glazing,
        data.table::data.table(),
        construction
    )
    values <- converted$value[
        class_name == "WindowMaterial:Glazing:RefractionExtinctionMethod"
    ]

    expect_equal(
        unique(converted$object$class_name),
        c("WindowMaterial:Glazing:RefractionExtinctionMethod", "Construction")
    )
    expect_equal(values[field_name == "Thickness", value_num], 0.003)
    expect_equal(
        values[field_name == "Solar Extinction Coefficient", value_num],
        45.0
    )
    expect_equal(
        values[field_name == "Solar Index of Refraction", value_num],
        1.5
    )
    expect_equal(
        values[field_name == "Infrared Hemispherical Emissivity", value_num],
        0.84
    )
})

test_that("warns about SimpleGlazingSystem in EnergyPlus 9.0 through 9.3", {
    expect_warning(
        const__warn_simple_glazing_version(
            eplusr::empty_idf("9.0.1"),
            1L
        ),
        "corrected in EnergyPlus 9.4"
    )
    expect_warning(
        const__warn_simple_glazing_version(
            eplusr::empty_idf("9.3.0"),
            1L
        ),
        "corrected in EnergyPlus 9.4"
    )
    expect_no_warning(const__warn_simple_glazing_version(
        eplusr::empty_idf("9.4.0"),
        1L
    ))
})

test_that("resolves aggregate window type performance and fallbacks", {
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(
        dest,
        "DEFAULT_SETTING",
        data.frame(
            TABLE_NAME = "WINDOW",
            FIELD_NAME = "WINDOW_CONSTRUCTION",
            TYPE = 2L,
            LONG = 9L
        )
    )
    DBI::dbWriteTable(
        dest,
        "WINDOW",
        data.frame(
            ID = 1:4,
            TYPE = c(10L, 20L, 30L, 40L),
            WINDOW_CONSTRUCTION = c(4L, 0L, 4L, 4L)
        )
    )
    DBI::dbWriteTable(
        dest,
        "WINDOW_TYPE_DATA",
        data.frame(
            ID = c(10L, 20L, 30L),
            NAME = c("Original", "Invalid K", "Optional VT"),
            K = c(3.2, 0.0, 2.0),
            SC = c(0.5942529, 0.4, 0.4022989),
            LIGHT_TRANS_RATIO = c(0.78, 0.58, 1.2)
        )
    )

    type <- const__window_type_performance(dest)
    expect_equal(type[WINDOW_ID == 1L, SHGC], 0.517, tolerance = 1e-7)
    expect_true(type[WINDOW_ID == 1L, TYPE_DATA_VALID])
    expect_equal(type[WINDOW_ID == 2L, DETAILED_CONSTRUCTION_ID], 9L)
    expect_equal(type[WINDOW_ID == 2L, FALLBACK_REASON], "invalid K value")
    expect_true(is.na(type[WINDOW_ID == 3L, LIGHT_TRANS_RATIO]))
    expect_equal(
        type[WINDOW_ID == 4L, FALLBACK_REASON],
        "missing WINDOW_TYPE_DATA record"
    )
})

test_that("returns a stable aggregate window type schema without windows", {
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    DBI::dbWriteTable(
        dest,
        "WINDOW",
        data.frame(
            ID = integer(),
            TYPE = integer(),
            WINDOW_CONSTRUCTION = integer()
        )
    )

    type <- const__window_type_performance(dest)
    expect_equal(nrow(type), 0L)
    expect_true(all(
        c(
            "TYPE_DATA_VALID",
            "TYPE_CONSTRUCTION_NAME",
            "FALLBACK_REASON"
        ) %in%
            names(type)
    ))
})

test_that("appends DeST's automatic soil only to ground-floor constructions", {
    layer <- data.table::data.table(
        ID = c(10L, 10L, 20L),
        NAME = c("Ground", "Ground", "Wall"),
        KIND = c(4L, 4L, 1L),
        LAYER_NO = c(0L, 1L, 0L),
        LENGTH = c(20, 40, 200),
        MATERIAL_ID = c(1L, 2L, 3L),
        MATERIAL_NAME = c("Cement", "Gravel", "Brick"),
        MATERIAL_CONDUCTIVITY = c(0.93, 1.547, 0.81),
        MATERIAL_DENSITY = c(1800, 2200, 1800),
        MATERIAL_SPECIFIC_HEAT = c(837, 837, 1050)
    )

    actual <- const__append_dest_ground_soil(layer)
    soil <- actual[MATERIAL_NAME == "DeST Automatic Soil"]

    expect_equal(nrow(actual), 4L)
    expect_equal(soil$ID, 10L)
    expect_equal(soil$KIND, 4L)
    expect_equal(soil$LAYER_NO, 2L)
    expect_equal(soil$LENGTH, 1200)
    expect_equal(soil$MATERIAL_CONDUCTIVITY, 0.93)
    expect_equal(soil$MATERIAL_DENSITY, 1800)
    expect_equal(soil$MATERIAL_SPECIFIC_HEAT, 1010)
    expect_equal(
        nrow(const__append_dest_ground_soil(actual)),
        nrow(actual)
    )
})

test_that("maps DeST's thermally massless sentinel to Material:NoMass", {
    ep <- eplusr::empty_idf(23.1)
    material <- data.table::data.table(
        MATERIAL_ID = 1:4,
        LENGTH = c(1003, 1007, 245.2, 25),
        MATERIAL_NAME = c(
            "Insulation 1003mm",
            "Insulation 1007mm",
            "Foam Insulation",
            "Timber 25mm"
        ),
        MATERIAL_CONDUCTIVITY = c(0.04, 0.04, 0.04, 0.14),
        MATERIAL_DENSITY = c(0.1, 10, 10, 650),
        MATERIAL_SPECIFIC_HEAT = c(0.1, 10, 1400, 1200),
        THERMAL_ABSORPTANCE = 0.9,
        SOLAR_ABSORPTANCE = 0.6,
        VISIBLE_ABSORPTANCE = 0.6
    )
    construction <- data.table::data.table(
        ID = 1L,
        KIND = 4L,
        name = "Floor",
        value = list(c(
            "Floor",
            "Insulation 1003mm",
            "Insulation 1007mm",
            "Foam Insulation",
            "Timber 25mm"
        ))
    )

    converted <- const__assemble_objects(
        TRUE,
        ep,
        material,
        data.table::data.table(),
        data.table::data.table(),
        data.table::data.table(),
        construction
    )
    no_mass <- converted$value[class_name == "Material:NoMass"]

    expect_equal(sum(converted$object$class_name == "Material"), 2L)
    expect_equal(sum(converted$object$class_name == "Material:NoMass"), 2L)
    expect_equal(
        no_mass[field_name == "Thermal Resistance", value_num],
        c(25.075, 25.175),
        tolerance = 1e-10
    )
    expect_equal(
        no_mass[field_name == "Solar Absorptance", value_num],
        rep(0.6, 2L)
    )
    expect_true(any(converted$object$class_name == "Construction"))
})

test_that("maps DeST's near-zero heat capacity layers to Material:NoMass", {
    ep <- eplusr::empty_idf(23.1)
    material <- data.table::data.table(
        MATERIAL_ID = 1:3,
        LENGTH = c(127, 100, 25),
        MATERIAL_NAME = c(
            "Insulation 127mm",
            "Insulation 100mm",
            "Timber 25mm"
        ),
        MATERIAL_CONDUCTIVITY = c(0.047, 0.001, 0.14),
        MATERIAL_DENSITY = c(30, 30, 650),
        MATERIAL_SPECIFIC_HEAT = c(1e-5, 0, 1200),
        THERMAL_ABSORPTANCE = 0.9,
        SOLAR_ABSORPTANCE = 0.6,
        VISIBLE_ABSORPTANCE = 0.6
    )
    construction <- data.table::data.table(
        ID = 1L,
        KIND = 4L,
        name = "Floor",
        value = list(c(
            "Floor",
            "Insulation 127mm",
            "Insulation 100mm",
            "Timber 25mm"
        ))
    )

    converted <- const__assemble_objects(
        TRUE,
        ep,
        material,
        data.table::data.table(),
        data.table::data.table(),
        data.table::data.table(),
        construction
    )
    no_mass <- converted$value[class_name == "Material:NoMass"]

    expect_equal(sum(converted$object$class_name == "Material"), 1L)
    expect_equal(sum(converted$object$class_name == "Material:NoMass"), 2L)
    expect_equal(
        no_mass[field_name == "Thermal Resistance", value_num],
        c(0.127 / 0.047, 100),
        tolerance = 1e-10
    )
})

test_that("can convert 'Construction' and 'Material'", {
    skip_on_cran()

    eplusr::use_idd(23.1, "auto")
    ep <- eplusr::empty_idf(23.1)

    path <- ensure_dest_test_file()
    dest <- read_dest(path, verbose = TRUE)
    conv__update_names(dest)

    # can convert 'Material', 'Construction'
    expect_type(const <- const__convert(dest, ep), "list")
    expect_named(const, c("object", "value"))
    expect_equal(
        unique(const$object$class_name),
        c(
            "Material",
            "WindowMaterial:SimpleGlazingSystem",
            "Construction"
        )
    )
    glazing <- const$value[
        class_name == "WindowMaterial:SimpleGlazingSystem"
    ]
    expect_true(all(
        c(
            "U-Factor",
            "Solar Heat Gain Coefficient",
            "Visible Transmittance"
        ) %in%
            glazing$field_name
    ))
    material_thickness <- const$value[
        class_name == "Material" & field_name == "Thickness",
        value_num
    ]
    expect_equal(max(material_thickness, na.rm = TRUE), 1.2)
    expect_true(any(material_thickness == 0.02))
    expect_true(any(material_thickness == 1.2))
    table <- attr(const, "table")
    soil <- table[
        KIND == 4L & MATERIAL_NAME == "DeST Automatic Soil 1200mm"
    ]
    expect_gt(nrow(soil), 0L)
    expect_equal(unique(soil$LENGTH), 1200)
    construction <- const$value[class_name == "Construction"]
    layer_count <- construction[field_name != "Name", .N, by = "rleid"]
    regular_id <- layer_count[N > 1L]$rleid[[1L]]
    regular_name <- construction[
        rleid == regular_id & field_name == "Name",
        value_chr
    ]
    reverse_id <- construction[
        field_name == "Name" & value_chr == paste0(regular_name, " [Reverse]"),
        rleid
    ]
    expect_length(reverse_id, 1L)
    expect_equal(
        construction[rleid == reverse_id & field_name != "Name", value_chr],
        rev(construction[rleid == regular_id & field_name != "Name", value_chr])
    )
    expect_s3_class(table, "data.table")
})
