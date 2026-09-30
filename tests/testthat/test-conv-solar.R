test_that("solar optical port retains independent two/three-pane reference values", {
    # Frozen values come from the earlier independently validated Python
    # reconstruction, including the 30-degree branch and grazing incidence.
    reference <- read.csv(test_path("fixture", "solar-optics-reference.csv"))
    for (i in seq_len(nrow(reference))) {
        row <- reference[i, ]
        media <- solar__media(row$layers, solar__index(row$sc, row$layers))
        actual <- solar__beam(media, row$degrees * pi / 180)
        expect_equal(unname(actual), c(row$transmittance, row$absorptance), tolerance = 1e-12)
        expect_gte(min(actual), 0)
        expect_lte(sum(actual), 1)
    }
})

test_that("solar construction validates inputs before adding any objects", {
    ep <- eplusr::empty_idf("23.1")
    count <- ep$object_num()
    expect_error(solar__construction(ep, "Bad", 0.4, 2, 4), "one to three")
    expect_error(solar__construction(ep, "Bad", 0.4, 2, 1.5), "one to three")
    expect_error(solar__construction(ep, "Bad", 0.4, 7, 2), "positive glass resistance")
    expect_error(solar__construction(ep, "Bad", 1.2, 2, 2), "SC in")
    expect_error(solar__construction(ep, "Bad", 0.4, 2, 2,
        inside_emissivity = NA_real_), "emissivity")
    expect_equal(ep$object_num(), count)
})

test_that("solar windows preserve asymmetric face emissivity across shared types", {
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    DBI::dbWriteTable(dest, "WINDOW", data.frame(
        ID = 1:3, NAME = c("A", "B", "C"), TYPE = 7L,
        WINDOW_CONSTRUCTION = 1L, SIDE1 = c(10L, 21L, 30L),
        SIDE2 = c(11L, 20L, 31L)))
    DBI::dbWriteTable(dest, "SURFACE", data.frame(
        SURFACE_ID = c(10L, 11L, 20L, 21L, 30L, 31L),
        TYPE = rep(c(1L, 0L), 3L), BLACKNESS = c(.9, .8, .9, .8, .7, .6)))
    DBI::dbWriteTable(dest, "WINDOW_TYPE_DATA", data.frame(
        ID = 7L, NAME = "Shared", K = 2, SC = .48,
        LIGHT_TRANS_RATIO = .6, LAYER_NUM = 2L))
    # A clipped source window contributes two named target pieces. Reversed
    # SIDE1/SIDE2 must still select the same physical inside/outside properties.
    pieces <- data.table::data.table(ID = c(1L, 1L, 2L, 3L),
        NAME = c("A part 1", "A part 2", "B", "C"))
    faces <- window__emissivity_table(dest, pieces)
    expect_equal(faces$OUTSIDE_EMISSIVITY, c(.9, .9, .9, .7))
    expect_equal(faces$INSIDE_EMISSIVITY, c(.8, .8, .8, .6))

    ep <- eplusr::empty_idf("23.1")
    ep$add(`WindowMaterial:SimpleGlazingSystem` = list(name = "Original Glass",
        u_factor = 2, solar_heat_gain_coefficient = .4),
        Construction = list(name = "Shared Simple Glazing Construction", outside_layer = "Original Glass"),
        Construction = list(name = "Shared Simple Glazing Construction [Reverse]", outside_layer = "Original Glass"),
        Material = list(name = "Wall Material", roughness = "Rough", thickness = .2,
            conductivity = 1, density = 1000, specific_heat = 1000),
        Construction = list(name = "Wall", outside_layer = "Wall Material"),
        Zone = list(name = "Room"),
        `BuildingSurface:Detailed` = list(name = "Host", surface_type = "Wall",
            construction_name = "Wall", zone_name = "Room", outside_boundary_condition = "Outdoors",
            sun_exposure = "SunExposed", wind_exposure = "WindExposed",
            number_of_vertices = 4, vertex_1_x_coordinate = 0, vertex_1_y_coordinate = 0, vertex_1_z_coordinate = 0,
            vertex_2_x_coordinate = 3, vertex_2_y_coordinate = 0, vertex_2_z_coordinate = 0,
            vertex_3_x_coordinate = 3, vertex_3_y_coordinate = 0, vertex_3_z_coordinate = 3,
            vertex_4_x_coordinate = 0, vertex_4_y_coordinate = 0, vertex_4_z_coordinate = 3))
    for (name in pieces$NAME) {
        solar__add_fields(ep, "FenestrationSurface:Detailed", c(name, "Window",
            trimws(paste("Shared Simple Glazing Construction", if (name == "B") "[Reverse]" else "")),
            "Host", "", "autocalculate", "", 1, 4,
            0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1))
    }
    expect_warning(audit <- solar__apply(dest, ep, windows = pieces), "solar optical tables")
    expect_equal(nrow(audit), 2L)
    expect_equal(audit$NORMAL_T[[1L]], audit$NORMAL_T[[2L]])
    constructions <- vapply(pieces$NAME, function(name) {
        ep$object(name)$to_table(wide = TRUE)$`Construction Name`
    }, character(1L))
    expect_equal(length(unique(constructions[1:3])), 1L)
    expect_false(constructions[[1L]] == constructions[[4L]])
    for (i in seq_len(nrow(faces))) {
        stack <- ep$object(constructions[[i]])$to_table(wide = TRUE)
        outer <- ep$object(stack$`Outside Layer`)$to_table(wide = TRUE)
        inner <- ep$object(stack$`Layer 3`)$to_table(wide = TRUE)
        expect_equal(as.double(outer$`Front Side Infrared Hemispherical Emissivity`),
            faces$OUTSIDE_EMISSIVITY[[i]])
        expect_equal(as.double(inner$`Back Side Infrared Hemispherical Emissivity`),
            faces$INSIDE_EMISSIVITY[[i]])
        expect_equal(as.double(outer$`Back Side Infrared Hemispherical Emissivity`), 1e-6)
        expect_equal(as.double(inner$`Front Side Infrared Hemispherical Emissivity`), 1e-6)
    }

    # Endpoint values use explicit representable limits; missing or physically
    # invalid source fields stop conversion rather than silently using 0.84.
    DBI::dbExecute(dest, "UPDATE SURFACE SET BLACKNESS = 0 WHERE SURFACE_ID = 10")
    DBI::dbExecute(dest, "UPDATE SURFACE SET BLACKNESS = 1 WHERE SURFACE_ID = 11")
    limits <- window__emissivity_table(dest)
    expect_equal(limits[ID == 1, OUTSIDE_EMISSIVITY], 1e-6)
    expect_equal(limits[ID == 1, INSIDE_EMISSIVITY], .99999)
    DBI::dbExecute(dest, "UPDATE SURFACE SET BLACKNESS = 1.1 WHERE SURFACE_ID = 10")
    expect_error(window__emissivity_table(dest), "BLACKNESS")
    DBI::dbExecute(dest, "UPDATE SURFACE SET BLACKNESS = NULL WHERE SURFACE_ID = 10")
    expect_error(window__emissivity_table(dest), "BLACKNESS")
    DBI::dbExecute(dest, "UPDATE SURFACE SET TYPE = 0 WHERE SURFACE_ID = 10")
    expect_error(window__emissivity_table(dest), "one outside")
})

test_that("solar tables and glass resistance form valid EnergyPlus objects", {
    ep <- eplusr::empty_idf("23.1")
    ep$add(Building = list(name = "Solar"), GlobalGeometryRules = list(
        starting_vertex_position = "UpperLeftCorner", vertex_entry_direction = "Counterclockwise",
        coordinate_system = "Relative"))
    audit <- solar__construction(ep, "Window", 0.480459770115, 2, 2)
    expect_true(ep$is_valid(level = "final"))
    expect_equal(audit$GLASS_RESISTANCE, 1 / 2 - 1 / 8.7 - 1 / 23.3, tolerance = 1e-12)
    expect_equal(audit$NORMAL_T, 0.4180440853691042, tolerance = 1e-12)
    expect_gt(audit$NORMAL_A, 0)
    expect_equal(ep$object_num("Table:Lookup"), 5L)
    gap <- ep$object("Window DeST Solar Gap")$to_table(wide = TRUE)
    # Gap conduction plus two thin sheets must equal the source glass R.
    expect_equal(as.numeric(gap$Thickness) /
        as.numeric(gap$`Conductivity Coefficient A`) + 2e-6,
        audit$GLASS_RESISTANCE, tolerance = 1e-12)
})

test_that("legacy window optics remains the default public API", {
    expect_identical(formals(to_eplus)$window_optics,
        quote(c("simple_glazing", "dest_solar")))
})

test_that("single panes retain resistance and independent face emissivities", {
    ep <- eplusr::empty_idf("23.1")
    audit <- solar__construction(ep, "Single", .57, 3.7, 1L, .72, .83)
    stack <- ep$object(audit$CONSTRUCTION)$to_table(wide = TRUE)
    pane <- ep$object(stack$`Outside Layer`)$to_table(wide = TRUE)
    expect_equal(ep$object_num("WindowMaterial:Glazing"), 1L)
    expect_equal(ep$object_num("WindowMaterial:Gas"), 0L)
    expect_equal(ep$object_num("Table:Lookup"), 3L)
    expect_equal(as.double(pane$Thickness) / as.double(pane$Conductivity),
        1 / 3.7 - 1 / 8.7 - 1 / 23.3, tolerance = 1e-12)
    expect_equal(as.double(pane$`Front Side Infrared Hemispherical Emissivity`), .72)
    expect_equal(as.double(pane$`Back Side Infrared Hemispherical Emissivity`), .83)
    # Independent one-pane native holdout, frozen before this implementation.
    expect_equal(audit$SOURCE_DIFFUSE_T, .4750445388132898, tolerance = 1e-12)
    expect_equal(audit$SOURCE_DIFFUSE_A, .09256660524552242, tolerance = 1e-12)
})
