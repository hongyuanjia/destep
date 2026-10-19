# Build the minimal active DeST shading tables used by converter unit tests.
shading__test_database <- function(rotation = 0.0, active = TRUE) {
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbWriteTable(
        dest,
        "WINDOW",
        data.table::data.table(
            ID = 1L,
            NAME = "East Window",
            SC = if (active) 0.4 else 0.0,
            SHADINGID = 11L,
            SIDE1 = 100L,
            SIDE2 = 101L
        )
    )
    DBI::dbWriteTable(
        dest,
        "SHADING",
        data.table::data.table(
            ID = 11L,
            TAO = 0.0,
            ROU = 0.5,
            LIB_SHADING_ID = 21L
        )
    )
    DBI::dbWriteTable(
        dest,
        "LIB_SHADING",
        data.table::data.table(
            ID = 21L,
            GROUP = 0L,
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
        )
    )
    # Stored polygons use an exterior face 50 mm outside the target middle
    # plane. They are specified independently of the production generator.
    points <- rbind(
        cbind(8.05, c(1.5, 1.5, 4.5, 4.5), c(2.2, 0.2, 0.2, 2.2)),
        rbind(
            c(9.05, 1.5, 2.7),
            c(8.05, 1.5, 2.7),
            c(8.05, 4.5, 2.7),
            c(9.05, 4.5, 2.7)
        ),
        rbind(
            c(9.05, 1.5, 2.7),
            c(9.05, 1.5, 0),
            c(8.05, 1.5, 0),
            c(8.05, 1.5, 2.7)
        ),
        rbind(
            c(9.05, 4.5, 2.7),
            c(9.05, 4.5, 0),
            c(8.05, 4.5, 0),
            c(8.05, 4.5, 2.7)
        )
    )
    DBI::dbWriteTable(
        dest,
        "SURFACE",
        data.frame(
            SURFACE_ID = c(100L, 201:203),
            TYPE = c(1L, rep(3L, 3L)),
            OF_SHADING = c(0L, rep(11L, 3L)),
            GEOMETRY = 1:4
        )
    )
    DBI::dbWriteTable(
        dest,
        "GEOMETRY",
        data.frame(
            GEOMETRY_ID = 1:4,
            BOUNDARY_LOOP_ID = 1:4
        )
    )
    DBI::dbWriteTable(
        dest,
        "LOOP_POINT",
        data.frame(
            LOOP_ID = rep(1:4, each = 4L),
            POINT_NO = rep(0:3, 4L),
            POINT = 1:16
        )
    )
    DBI::dbWriteTable(
        dest,
        "POINT",
        data.frame(
            POINT_ID = 1:16,
            X = points[, 1L],
            Y = points[, 2L],
            Z = points[, 3L]
        )
    )
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
        dest,
        eplusr::empty_idf(23.1),
        shading__test_window()
    )

    expect_named(converted, c("object", "value"))
    expect_equal(
        converted$object$class_name,
        rep(
            c("Shading:Zone:Detailed", "ShadingProperty:Reflectance"),
            each = 3L
        )
    )
    expect_setequal(
        attr(converted, "table")$KIND,
        c("overhang", "low_fin", "high_fin")
    )
    expect_setequal(
        converted$value[
            field_name == "Base Surface Name",
            value_chr
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
            c(9, 1.5, 2.7),
            c(8, 1.5, 2.7),
            c(8, 4.5, 2.7),
            c(9, 4.5, 2.7)
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
            dest,
            eplusr::empty_idf("9.0.1"),
            shading__test_window()
        ),
        "geometry compatibility profile"
    )

    expect_equal(
        converted$object$class_name,
        rep(
            c("Shading:Zone:Detailed", "ShadingProperty:Reflectance"),
            each = 3L
        )
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
        inactive,
        eplusr::empty_idf(23.1),
        shading__test_window()
    ))

    rotated <- shading__test_database(rotation = 15.0)
    on.exit(DBI::dbDisconnect(rotated), add = TRUE)
    expect_error(
        shading__convert(
            rotated,
            eplusr::empty_idf(23.1),
            shading__test_window()
        ),
        "Only opaque, symmetric, zero-rotation"
    )
})

test_that("opaque shading preserves reflectance for every generated piece", {
    dest <- shading__test_database()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    converted <- shading__convert(
        dest,
        eplusr::empty_idf("23.1"),
        shading__test_window()
    )
    fields <- converted$value
    expect_setequal(
        fields[field_name == "Shading Surface Name", value_chr],
        attr(converted, "table")$SHADING_NAME
    )
    expect_equal(
        fields[
            field_name ==
                "Diffuse Solar Reflectance of Unglazed Part of Shading Surface",
            value_num
        ],
        rep(0.5, 3L)
    )
    expect_equal(
        fields[
            field_name == "Fraction of Shading Surface That Is Glazed",
            value_num
        ],
        rep(0, 3L)
    )
    expect_equal(attr(converted, "table")$SOLAR_REFLECTANCE, rep(0.5, 3L))
    # The source does not identify visible reflectance; leave that field at
    # the target default instead of copying the solar value into it.
    visible <- fields[
        field_name ==
            "Diffuse Visible Reflectance of Unglazed Part of Shading Surface",
        value_num
    ]
    expect_equal(visible, rep(0.2, 3L))
    for (value in c(-0.1, 1.1, Inf)) {
        DBI::dbExecute(dest, "UPDATE SHADING SET ROU = ?", params = list(value))
        expect_error(
            shading__convert(
                dest,
                eplusr::empty_idf("23.1"),
                shading__test_window()
            ),
            "Invalid DeST shading reflectance ROU"
        )
    }
})

test_that("shading rejects stale parameters until stored polygons agree", {
    dest <- shading__test_database()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    DBI::dbExecute(dest, "UPDATE LIB_SHADING SET W = 2 WHERE ID = 21")
    expect_error(
        shading__convert(dest, eplusr::empty_idf(23.1), shading__test_window()),
        "Stored DeST shading geometry does not match"
    )
    # Update only the two overhang outer vertices. Side fins stay unchanged.
    DBI::dbExecute(dest, "UPDATE POINT SET X = X + 1 WHERE POINT_ID IN (5,8)")
    result <- shading__convert(
        dest,
        eplusr::empty_idf(23.1),
        shading__test_window()
    )
    expect_equal(sum(result$object$class_name == "Shading:Zone:Detailed"), 3L)
})

test_that("shading rejects missing, duplicate and nonfinite stored geometry", {
    edits <- c(
        "DELETE FROM SURFACE WHERE OF_SHADING = 11",
        "UPDATE LOOP_POINT SET POINT_NO = 0 WHERE LOOP_ID = 2",
        "UPDATE POINT SET X = NULL WHERE POINT_ID = 5",
        "DELETE FROM POINT WHERE POINT_ID = 5",
        "UPDATE POINT SET X = X + 0.1 WHERE POINT_ID = 5"
    )
    for (edit in edits) {
        dest <- shading__test_database()
        DBI::dbExecute(dest, edit)
        expect_error(
            shading__convert(
                dest,
                eplusr::empty_idf(23.1),
                shading__test_window()
            ),
            "Stored DeST shading geometry does not match"
        )
        DBI::dbDisconnect(dest)
    }
    dest <- shading__test_database()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    DBI::dbExecute(dest, "DROP TABLE POINT")
    expect_error(
        shading__convert(dest, eplusr::empty_idf(23.1), shading__test_window()),
        "requires stored window and shading geometry"
    )
})

# Attach a synthetic louver definition to an isolated real-model copy. These
# inputs test the public rejection boundary, not native louver reconstruction.
shading_types__fixture <- function(
    group = 1L,
    dest = destep_test__unmultiplied_fixture()
) {
    DBI::dbExecute(
        dest,
        "INSERT INTO LIB_SHADING
         (ID, B0, B1, B2, DEG, DIST, W, N, HF, HL, WL, DEGL, HR, WR, DEGR, [GROUP])
         VALUES (990001, 0.5, 0, 0, 0, 0.05, 1, 20, 0, 2.7, 1, 0, 2.7, 1, 0, ?)",
        params = list(group)
    )
    DBI::dbExecute(
        dest,
        "INSERT INTO SHADING (ID, TAO, ROU, LIB_SHADING_ID)
         VALUES (990002, 0, 0.5, 990001)"
    )
    DBI::dbExecute(
        dest,
        "UPDATE WINDOW SET SC = 0.4, SHADINGID = 990002 WHERE ID = 925563"
    )
    dest
}

test_that("to_idf identifies unsupported horizontal louvers before geometry", {
    dest <- shading_types__fixture()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    error <- tryCatch(
        suppressWarnings(to_idf(dest, 23.1)),
        error = identity
    )
    expect_s3_class(error, "destep_unsupported_shading_type")
    expect_match(conditionMessage(error), "horizontal louvers")
    expect_match(conditionMessage(error), "925563")
    expect_equal(error$shading$library_id, 990001)
    expect_equal(error$shading$group, 1)
})

test_that("to_idf distinguishes vertical and unknown shading types", {
    dest <- shading_types__fixture(2L)
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    expect_error(
        suppressWarnings(to_idf(dest, 23.1)),
        "vertical louvers",
        class = "destep_unsupported_shading_type"
    )
    # NULL, an unfamiliar enumeration and a fractional value must never be
    # rounded or defaulted to the ordinary-shading family.
    for (group in c(NA_real_, 3, 0.5)) {
        DBI::dbExecute(
            dest,
            "UPDATE LIB_SHADING SET [GROUP] = ? WHERE ID = 990001",
            params = list(group)
        )
        expect_error(
            suppressWarnings(to_idf(dest, 23.1)),
            "unknown shading type",
            class = "destep_unsupported_shading_type"
        )
    }
    DBI::dbExecute(dest, "ALTER TABLE LIB_SHADING DROP COLUMN [GROUP]")
    expect_error(
        suppressWarnings(to_idf(dest, 23.1)),
        "unknown shading type",
        class = "destep_unsupported_shading_type"
    )
})

test_that("public conversion rejects stale ordinary geometry without editing input", {
    path <- ensure_dest_test_file()
    before_file <- tools::md5sum(path)
    dest <- shading_types__fixture(0L, read_dest(path))
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    DBI::dbExecute(dest, "UPDATE STOREY SET MULTIPLE = 1")
    DBI::dbExecute(
        dest,
        "UPDATE LIB_SHADING SET HL = 0, HR = 0, WL = 0, WR = 0
         WHERE ID = 990001"
    )
    # Independently specify a 1 m overhang, 0.5 m above window 925563's
    # saved exterior top edge. This synthetic geometry is not native evidence.
    DBI::dbExecute(
        dest,
        "INSERT INTO SURFACE (SURFACE_ID, TYPE, OF_SHADING, GEOMETRY)
         VALUES (1990000001, 3, 990002, 1990000001)"
    )
    DBI::dbExecute(
        dest,
        "INSERT INTO GEOMETRY (GEOMETRY_ID, BOUNDARY_LOOP_ID)
         VALUES (1990000001, 1990000001)"
    )
    DBI::dbAppendTable(
        dest,
        "LOOP_POINT",
        data.frame(
            LOOP_ID = 1990000001L,
            POINT_NO = 0:3,
            POINT = 1990000001:1990000004
        )
    )
    DBI::dbAppendTable(
        dest,
        "POINT",
        data.frame(
            POINT_ID = 1990000001:1990000004,
            X = c(104.826136883, 103.826136883, 103.826136883, 104.826136883),
            Y = c(
                26.15775194460188,
                26.15775194460188,
                29.64311702899812,
                29.64311702899812
            ),
            Z = 4.162985845025499
        )
    )
    idf <- suppressWarnings(to_idf(dest, 23.1))
    expect_true(idf$is_valid(level = "final"))

    # Change only the imported library depth, retaining the original polygons.
    # Rejection must preserve both the supplied tables and the Access file.
    DBI::dbExecute(dest, "UPDATE LIB_SHADING SET W = 2 WHERE ID = 990001")
    tables <- DBI::dbListTables(dest)
    before <- lapply(tables, DBI::dbReadTable, conn = dest)
    expect_error(
        suppressWarnings(to_idf(dest, 23.1)),
        "Stored DeST shading geometry does not match"
    )
    expect_identical(lapply(tables, DBI::dbReadTable, conn = dest), before)
    expect_identical(tools::md5sum(path), before_file)
})
