test_that("to_eplus() works", {
    skip_on_cran()
    eplusr::use_idd(23.1, "auto")
    ep <- eplusr::empty_idf(23.1)

    path <- ensure_dest_test_file()
    dest <- read_dest(path)

    # can insert a comment about the original DeST version in `Version` comment
    expect_equal(
        destep_comment_version(dest, ep)$object$comment[[1]],
        "Converted from DeST v2.6"
    )

    # can update all necessary names before conversion
    destep_update_name(dest)
    expect_equal(DBI::dbReadTable(dest, "STOREY")$NAME, paste("Storey", 1:3))
    expect_true(any(grepl("3-N-1 Wall", DBI::dbReadTable(dest, "SURFACE")$NAME)))
    expect_true(any(grepl("DefaultOutside Wall", DBI::dbReadTable(dest, "SURFACE")$NAME)))
    expect_true(any(grepl("DefaultGround Floor", DBI::dbReadTable(dest, "SURFACE")$NAME)))
    expect_true(all(startsWith(DBI::dbReadTable(dest, "SYS_OUTWALL")$CNAME, "ExtWall - ")))
    expect_true(all(startsWith(DBI::dbReadTable(dest, "SYS_INWALL")$CNAME, "IntWall - ")))
    expect_true(all(startsWith(DBI::dbReadTable(dest, "SYS_ROOF")$CNAME, "Roof - ")))
    expect_true(all(startsWith(DBI::dbReadTable(dest, "SYS_GROUNDFLOOR")$CNAME, "GroundFloor - ")))
    expect_true(all(startsWith(DBI::dbReadTable(dest, "SYS_MIDDLEFLOOR")$CNAME, "Ceiling - ")))

    # can convert 'Building'
    expect_type(bld <- destep_conv_building(dest, ep), "list")
    expect_named(bld, c("object", "value"))
    expect_s3_class(attr(bld, "table"), "data.table")
    # can specify which building to extract
    expect_error(destep_conv_building(dest, ep, TRUE), "integer or character")
    expect_equal(destep_conv_building(dest, ep, 1), destep_conv_building(dest, ep, "国管局1#"))

    # can convert 'Site:Location'
    expect_type(loc <- destep_conv_location(dest, ep), "list")
    expect_named(loc, c("object", "value"))
    expect_s3_class(attr(loc, "table"), "data.table")
    expect_equal(loc$value$value_chr[[1L]], "DefaultEnvironment")

    # can convert 'Zone', 'ZoneList', 'ZoneGroup'
    expect_type(zn <- destep_conv_zone(dest, ep), "list")
    expect_named(zn, c("object", "value"))
    expect_equal(unique(zn$object$class_name), c("Zone", "ZoneList", "ZoneGroup"))
    expect_s3_class(attr(zn, "table"), "data.table")

    # can convert 'Material', 'Construction'
    expect_type(expect_message(const <- destep_conv_const(dest, ep)), "list")
    expect_named(const, c("object", "value"))
    expect_equal(unique(const$object$class_name),
        c(
            "Material", "WindowMaterial:Glazing", "WindowMaterial:Gas",
            "Construction"
        )
    )
    expect_s3_class(attr(const, "table"), "data.table")

    # can convert 'BuildingSurface:Detailed'
    expect_type(surface <- destep_conv_surface(dest, ep), "list")
    expect_named(surface, c("object", "value"))
    expect_equal(unique(surface$object$class_name), "BuildingSurface:Detailed")
    expect_s3_class(attr(surface, "table"), "data.table")

    # can convert a DeST model to an EnergyPlus model
    expect_message(expect_s3_class(idf <- to_eplus(dest), "Idf"))
})
