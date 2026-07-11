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
    expect_s3_class(attr(const, "table"), "data.table")
})
