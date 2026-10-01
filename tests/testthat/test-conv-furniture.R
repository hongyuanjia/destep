test_that("furniture uses the effective room type and full two-face slab", {
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    DBI::dbWriteTable(
        dest,
        "ROOM",
        data.frame(
            ID = 1:4,
            NAME = paste("Room", 1:4),
            AREA = c(10, 49.9, 50, 75),
            TYPE = c(1L, 2L, 2L, 3L),
            FURNITURE_COEF = 1
        )
    )
    DBI::dbWriteTable(
        dest,
        "ROOM_TYPE_DATA",
        data.frame(
            ID = 1:3,
            FURNITURE_COEF = c(1, 17, 7)
        )
    )
    ep <- eplusr::empty_idf(23.1)
    result <- furniture__convert(dest, ep)
    expect_match(
        paste(attr(result, "assumptions"), collapse = " "),
        "0.2.230705",
        fixed = TRUE
    )
    expect_match(
        paste(unlist(result$object$comment), collapse = " "),
        "equivalent factors",
        fixed = TRUE
    )
    room <- attr(result, "table")
    expect_equal(room$ID, 2:4)
    expect_equal(room$COEFFICIENT, c(17, 17, 7))
    # These independently frozen capacities are reproduced by the native
    # engine to <0.02%; they detect using room volume or drawing coefficients.
    expect_equal(
        room$ONE_FACE_AREA * 0.05 * 377 * 1930,
        c(3466333.44, 3462436.3636363633, 1493072.7272727275)
    )
    values <- result$value
    expect_equal(
        values$value_num[
            values$class_name == "InternalMass" &
                values$field_name == "Surface Area"
        ],
        2 * room$ONE_FACE_AREA
    )
    expect_equal(
        values$value_num[
            values$class_name == "Material" &
                values$field_name == "Thickness"
        ],
        0.05
    )
    expect_equal(
        values$value_num[
            values$class_name == "SurfaceProperty:ConvectionCoefficients" &
                values$field_name == "Convection Coefficient 1"
        ],
        rep(8.7, 3)
    )
    # The prescribed-source mode changes only numerical radiant pickup, while
    # retaining the independently verified slab capacity and surface areas.
    prescribed <- furniture__convert(dest, ep, "dest")
    expect_equal(
        prescribed$value$value_num[
            prescribed$value$class_name == "Material" &
                prescribed$value$field_name == "Thermal Absorptance"
        ],
        1e-12
    )
    expect_equal(attr(prescribed, "table"), room)
    DBI::dbExecute(dest, "UPDATE ROOM_TYPE_DATA SET FURNITURE_COEF=1")
    expect_null(furniture__convert(dest, ep))
})

test_that("invalid furniture inputs do not silently disappear", {
    expect_error(furniture__slab_area(0, 17), "positive finite")
    expect_error(furniture__slab_area(20, NA_real_), "non-negative finite")
    expect_error(furniture__slab_area(20, -1), "non-negative finite")
    expect_equal(furniture__slab_area(c(20, 50), c(0, 1)), c(0, 0))
})
