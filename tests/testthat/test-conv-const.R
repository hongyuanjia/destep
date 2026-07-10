test_that("can convert 'Construction' and 'Material'", {
    skip_on_cran()

    eplusr::use_idd(23.1, "auto")
    ep <- eplusr::empty_idf(23.1)

    path <- ensure_dest_test_file()
    dest <- read_dest(path, verbose = TRUE)
    destep_update_name(dest)

    # can convert 'Material', 'Construction'
    expect_type(expect_message(const <- destep_conv_const(dest, ep)), "list")
    expect_named(const, c("object", "value"))
    expect_equal(
        unique(const$object$class_name),
        c(
            "Material", "WindowMaterial:Glazing", "WindowMaterial:Gas",
            "Construction"
        )
    )
    material_thickness <- const$value[
        class_name == "Material" & field_name == "Thickness",
        value_num
    ]
    expect_equal(max(material_thickness, na.rm = TRUE), 0.2)
    expect_true(any(material_thickness == 0.02))
    expect_s3_class(attr(const, "table"), "data.table")
})
