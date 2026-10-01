# Use unequal areas and window emissivity so normalization and radiant-pool
# mistakes cannot cancel in these independently calculated recipient shares.
source__test_faces <- function() {
    data.frame(
        name = c("wall", "window", "floor", "roof"),
        zone = "Room",
        area = c(8, 2, 10, 10),
        category = c("wall", "wall", "floor", "roof"),
        is_window = c(FALSE, TRUE, FALSE, FALSE),
        epsilon = c(.9, .8, .9, .9),
        construction = c("Opaque", "Glass", "Opaque", "Opaque")
    )
}

# A constant source keeps source watts separate from the schedule multiplier.
source__test_source <- function() {
    list(
        name = "Gain",
        zone = "Room",
        kind = "equipment",
        design_power = 200,
        schedule = "Gain Schedule",
        mode = list(air = .1, wall = .2, floor = .4, roof = .1),
        existing_radiant = .9,
        existing_air = .1
    )
}

test_that("literal source sums and net receiving areas are preserved", {
    faces <- source__test_faces()
    mode <- source__test_source()$mode
    shares <- source__fractions(faces, mode)
    # Radiant total .7; the category-area denominator is 7 m2.
    expect_equal(unname(shares), c(.16, .04, .4, .1), tolerance = 1e-14)
    expect_equal(sum(shares), .7, tolerance = 1e-14)
    expect_equal(
        unname(source__fractions(
            faces,
            list(air = 1, wall = 0, floor = 0, roof = 0)
        )),
        rep(0, 4)
    )
    expect_error(
        source__fractions(
            faces,
            list(air = .5, wall = .5, floor = .5, roof = 0)
        ),
        "at most one"
    )
    expect_error(
        source__fractions(faces, list(air = 0, wall = -1, floor = 0, roof = 0)),
        "nonnegative"
    )
    expect_error(
        source__fractions(
            faces[1:2, ],
            list(air = 0, wall = 0, floor = 1, roof = 0)
        ),
        "no receiving area"
    )
})

test_that("carrier and signed corrections reproduce independent prescribed shares", {
    faces <- source__test_faces()
    item <- source__plan(faces, list(source__test_source()))[[1L]]
    expect_equal(item$carrier, .67, tolerance = 1e-14)
    expect_equal(
        unname(item$carrier * item$pool[["window"]]),
        .04,
        tolerance = 1e-14
    )
    # Carrier 134 W plus opaque corrections sum to 140 W surface input,
    # leaving the literal 20 W air share and the unassigned 40 W unfilled.
    opaque <- !faces$is_window
    correction <- 200 *
        (item$fractions[opaque] - item$carrier * item$pool[opaque])
    expect_equal(200 * item$carrier + sum(correction), 140, tolerance = 1e-12)
    expect_equal(item$mode$air * item$design_power, 20)
    expect_equal(item$existing_radiant, .9)
})

test_that("unsupported glazing and malformed source inventories fail before mutation", {
    faces <- source__test_faces()
    extra <- faces[2, ]
    extra$name <- "second window"
    extra$epsilon <- .6
    expect_error(
        source__plan(rbind(faces, extra), list(source__test_source())),
        "Mixed glazing"
    )
    faces$area[[1L]] <- 0
    expect_error(
        source__plan(faces, list(source__test_source())),
        "receiving-face"
    )
    source <- source__test_source()
    source$kind <- "solar"
    expect_error(
        source__plan(source__test_faces(), list(source)),
        "design_power = 1"
    )
    source$kind <- "people"
    expect_error(
        source__plan(source__test_faces(), list(source)),
        "Unsupported"
    )
})

test_that("source projection refuses hidden duplicate gains and incomplete optical data", {
    faces <- source__test_faces()
    sources <- list(source__test_source())
    expect_error(
        source__project(list(c("People", "Occupants")), faces, sources),
        "Unsupported existing"
    )
    expect_error(
        source__project(
            list(
                c("ElectricEquipment", "Gain"),
                c("Schedule:Constant", "Source Always On", "", "1")
            ),
            faces,
            sources
        ),
        "already exist"
    )
    expect_error(
        source__check_existing(
            list(c("ElectricEquipment", "Unmapped")),
            faces,
            sources
        ),
        "Unsupported existing"
    )
    expect_no_error(source__check_existing(
        list(
            c("ElectricEquipment", "Gain"),
            c("Schedule:File", "SourceEquipment")
        ),
        faces,
        sources
    ))
    sources[[1L]]$kind <- "solar"
    sources[[1L]]$design_power <- 1
    objects <- list(c("Version", "26.1"))
    expect_error(source__project(objects, faces, sources), "Solar columns")
    columns <- list("Gain Schedule" = c(0, NA_real_))
    expect_error(
        source__project(objects, faces, sources, columns),
        "Solar columns"
    )
    expect_identical(objects, list(c("Version", "26.1")))
})

test_that("furniture receives no prescribed share and people powers retain units", {
    faces <- source__test_faces()
    mass <- faces[1, ]
    mass$name <- "Furniture"
    mass$area <- 100
    mass$category <- "furniture"
    mass$epsilon <- 1e-12
    faces <- rbind(faces, mass)
    item <- source__test_source()
    item$kind <- "people"
    item$design_power <- 2
    item$sensible_heat <- 53
    planned <- source__plan(faces, list(item))[[1L]]
    expect_equal(planned$fractions[["Furniture"]], 0)
    expect_gt(planned$pool[["Furniture"]], 0)
    expect_identical(
        planned$power_expression,
        "2 * SourceSchedule1 * 53"
    )
})

test_that("shared optical tables are blocked exactly once", {
    # Two constructions share one outer pane. A second update must not erase
    # the retained front absorption or add the old transmission twice.
    table <- function(name, value) {
        c(
            "Table:Lookup",
            name,
            "Axes",
            "DivisorOnly",
            "1",
            "0",
            "1",
            "Dimensionless",
            "",
            "",
            "",
            rep(as.character(value), 182L)
        )
    }
    glazing <- c(
        "WindowMaterial:Glazing",
        "Outer",
        "SpectralAndAngle",
        rep("", 16L),
        "T",
        "R"
    )
    objects <- list(
        c("FenestrationSurface:Detailed", "One", "Window", "C1"),
        c("FenestrationSurface:Detailed", "Two", "Window", "C2"),
        c("Construction", "C1", "Outer", "Gap", "Inner"),
        c("Construction", "C2", "Outer"),
        glazing,
        table("T", .4),
        table("R", .2)
    )
    blocked <- source__block_solar(objects)
    expect_equal(as.numeric(blocked$objects[[6L]][-(1:11)]), rep(0, 182L))
    expect_equal(as.numeric(blocked$objects[[7L]][-(1:11)]), rep(.6, 182L))
    expect_length(blocked$changes, 1L)
    expect_equal(as.numeric(objects[[6L]][-(1:11)]), rep(.4, 182L))
})
