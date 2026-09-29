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
    expect_error(solar__construction(ep, "Bad", 0.4, 2, 1), "two or three")
    expect_error(solar__construction(ep, "Bad", 0.4, 7, 2), "positive glass resistance")
    expect_error(solar__construction(ep, "Bad", 1.2, 2, 2), "SC in")
    expect_equal(ep$object_num(), count)
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
