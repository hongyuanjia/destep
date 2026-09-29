# Build the minimal active DeST shading tables used by converter unit tests.
shading__test_database <- function(rotation = 0.0, active = TRUE) {
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbWriteTable(dest, "WINDOW", data.table::data.table(
        ID = 1L,
        NAME = "East Window",
        SC = if (active) 0.4 else 0.0,
        SHADINGID = 11L
    ))
    DBI::dbWriteTable(dest, "SHADING", data.table::data.table(
        ID = 11L,
        TAO = 0.0,
        ROU = 0.5,
        LIB_SHADING_ID = 21L
    ))
    DBI::dbWriteTable(dest, "LIB_SHADING", data.table::data.table(
        ID = 21L,
        B0 = 0.5,
        B1 = 0.0,
        B2 = 0.0,
        DEG = rotation,
        DIST = 0.05,
        W = 1.0,
        N = 20L,
        HF = 0.0,
        HL = 2.7,
        WL = 1.0,
        DEGL = 0.0,
        HR = 2.7,
        WR = 1.0,
        DEGR = 0.0
    ))
    dest
}

# Return one upper-left, counterclockwise east-facing converted window table.
shading__test_window <- function() {
    data.table::data.table(
        ID = 1L,
        OUTPUT_PART_ID = "1-1",
        NAME = "East Window",
        SURFACE_NAME = "East Wall",
        INTERZONE = FALSE,
        PART_COUNT = 1L,
        POINT_NO = 0:3,
        POINT_X = 8.0,
        POINT_Y = c(1.5, 1.5, 4.5, 4.5),
        POINT_Z = c(2.2, 0.2, 0.2, 2.2)
    )
}

test_that("converts validated DeST overhang and side-fin geometry", {
    dest <- shading__test_database()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    converted <- shading__convert(
        dest, eplusr::empty_idf(23.1), shading__test_window()
    )

    expect_named(converted, c("object", "value"))
    expect_equal(
        converted$object$class_name,
        rep("Shading:Zone:Detailed", 3L)
    )
    expect_setequal(
        attr(converted, "table")$KIND,
        c("overhang", "low_fin", "high_fin")
    )
    expect_setequal(
        converted$value[
            field_name == "Base Surface Name", value_chr
        ],
        "East Wall"
    )
    overhang <- converted$value[
        rleid == converted$object$rleid[[1L]] & grepl("coordinate", field_name),
        value_num
    ]
    expect_equal(
        matrix(overhang, ncol = 3L, byrow = TRUE),
        rbind(
            c(9, 1.5, 2.7), c(8, 1.5, 2.7),
            c(8, 4.5, 2.7), c(9, 4.5, 2.7)
        )
    )
})

test_that("supports EnergyPlus 9.0.1 shading fields", {
    skip_if_not("9.0.1" %in% eplusr::avail_eplus())
    dest <- shading__test_database()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    converted <- NULL
    expect_warning(
        converted <- shading__convert(
            dest, eplusr::empty_idf("9.0.1"), shading__test_window()
        ),
        "geometry compatibility profile"
    )

    expect_equal(
        converted$object$class_name,
        rep("Shading:Zone:Detailed", 3L)
    )
    expect_setequal(
        attr(converted, "table")$KIND,
        c("overhang", "low_fin", "high_fin")
    )
})

test_that("skips inactive shading and rejects unvalidated rotation", {
    inactive <- shading__test_database(active = FALSE)
    on.exit(DBI::dbDisconnect(inactive), add = TRUE)
    expect_null(shading__convert(
        inactive, eplusr::empty_idf(23.1), shading__test_window()
    ))

    rotated <- shading__test_database(rotation = 15.0)
    on.exit(DBI::dbDisconnect(rotated), add = TRUE)
    expect_error(
        shading__convert(rotated, eplusr::empty_idf(23.1), shading__test_window()),
        "Only opaque, symmetric, zero-rotation"
    )
})
