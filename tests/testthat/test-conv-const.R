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
    DBI::dbWriteTable(dest, "DOOR", data.frame(
        ID = seq_along(door_construction),
        DOOR_CONSTRUCTION = door_construction,
        OF_ENCLOSURE = enclosure
    ))
    DBI::dbWriteTable(dest, "DEFAULT_SETTING", data.frame(
        TABLE_NAME = character(), FIELD_NAME = character(),
        TYPE = integer(), LONG = integer()
    ))
    DBI::dbWriteTable(dest, "SYS_DOOR", data.frame(
        DOOR_ID = door_construction,
        CNAME = paste("Door", door_construction),
        MATERIAL_ID = material,
        APP_ID = app_id,
        APP_FLAG = app_flag
    ))
    DBI::dbWriteTable(dest, "SYS_MATERIAL", data.frame(
        MATERIAL_ID = material,
        CNAME = paste("Opaque", material),
        CONDUCTIVITY = rep(0.5, length(material)),
        DENSITY = rep(800, length(material)),
        SPECIFIC_HEAT = rep(1000, length(material))
    ))
    DBI::dbWriteTable(dest, "SYS_APP_MATERIAL", data.frame(
        APP_MATERIAL_ID = integer(), CNAME = character(), THICK = double(),
        CONDUCTIVITY = double(), DENSITY = double(), SPECIFIC_HEAT = double()
    ))
    DBI::dbWriteTable(dest, "MAIN_ENCLOSURE", data.frame(
        ID = enclosure,
        CONSTRUCTION = c(1000L, 2000L)[seq_along(enclosure)],
        KIND = rep(1L, length(enclosure))
    ))

    layer_tables <- c(
        "SYS_OUTWALL_MATERIAL", "SYS_INWALL_MATERIAL",
        "SYS_ROOF_MATERIAL", "SYS_GROUNDFLOOR_MATERIAL",
        "SYS_MIDDLEFLOOR_MATERIAL", "SYS_AIRFLOOR_MATERIAL"
    )
    empty_layer <- data.frame(
        STRUCT_ID = integer(), MATERIAL_ID = integer(),
        LAYER_NO = integer(), LENGTH = double()
    )
    for (table in layer_tables) {
        DBI::dbWriteTable(dest, table, empty_layer)
    }
    DBI::dbWriteTable(
        dest, "SYS_OUTWALL_MATERIAL",
        data.frame(
            STRUCT_ID = c(1000L, 2000L), MATERIAL_ID = material,
            LAYER_NO = 0L, LENGTH = c(120, 180)
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

test_that("resolves aggregate window type performance and fallbacks", {
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(dest, "DEFAULT_SETTING", data.frame(
        TABLE_NAME = "WINDOW",
        FIELD_NAME = "WINDOW_CONSTRUCTION",
        TYPE = 2L,
        LONG = 9L
    ))
    DBI::dbWriteTable(dest, "WINDOW", data.frame(
        ID = 1:4,
        TYPE = c(10L, 20L, 30L, 40L),
        WINDOW_CONSTRUCTION = c(4L, 0L, 4L, 4L)
    ))
    DBI::dbWriteTable(dest, "WINDOW_TYPE_DATA", data.frame(
        ID = c(10L, 20L, 30L),
        NAME = c("Original", "Invalid K", "Optional VT"),
        K = c(3.2, 0.0, 2.0),
        SC = c(0.5942529, 0.4, 0.4022989),
        LIGHT_TRANS_RATIO = c(0.78, 0.58, 1.2)
    ))

    type <- destep_window_type_performance(dest)
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
    DBI::dbWriteTable(dest, "WINDOW", data.frame(
        ID = integer(), TYPE = integer(), WINDOW_CONSTRUCTION = integer()
    ))

    type <- destep_window_type_performance(dest)
    expect_equal(nrow(type), 0L)
    expect_true(all(c(
        "TYPE_DATA_VALID", "TYPE_CONSTRUCTION_NAME", "FALLBACK_REASON"
    ) %in% names(type)))
})

test_that("can convert 'Construction' and 'Material'", {
    skip_on_cran()

    eplusr::use_idd(23.1, "auto")
    ep <- eplusr::empty_idf(23.1)

    path <- ensure_dest_test_file()
    dest <- read_dest(path, verbose = TRUE)
    destep_update_name(dest)

    # can convert 'Material', 'Construction'
    expect_type(const <- destep_conv_const(dest, ep), "list")
    expect_named(const, c("object", "value"))
    expect_equal(
        unique(const$object$class_name),
        c(
            "Material", "WindowMaterial:SimpleGlazingSystem", "Construction"
        )
    )
    glazing <- const$value[
        class_name == "WindowMaterial:SimpleGlazingSystem"
    ]
    expect_true(all(c(
        "U-Factor", "Solar Heat Gain Coefficient", "Visible Transmittance"
    ) %in% glazing$field_name))
    material_thickness <- const$value[
        class_name == "Material" & field_name == "Thickness",
        value_num
    ]
    expect_equal(max(material_thickness, na.rm = TRUE), 0.2)
    expect_true(any(material_thickness == 0.02))
    construction <- const$value[class_name == "Construction"]
    layer_count <- construction[field_name != "Name", .N, by = "rleid"]
    regular_id <- layer_count[N > 1L]$rleid[[1L]]
    regular_name <- construction[
        rleid == regular_id & field_name == "Name", value_chr
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
    expect_s3_class(attr(const, "table"), "data.table")
})
