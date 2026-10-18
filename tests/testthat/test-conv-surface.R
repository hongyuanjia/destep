test_that("can convert 'BuildingSurface:Detailed'", {
    skip_on_cran()

    ep <- eplusr::empty_idf(23.1)
    dest <- destep_test__unmultiplied_fixture()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    conv__update_names(dest)

    # can convert 'BuildingSurface:Detailed'
    expect_warning(
        surface <- surface__convert(dest, ep),
        class = "destep_geometry_closure_tolerance"
    )
    expect_type(surface, "list")
    expect_warning(
        automatic <- surface__convert(
            dest,
            ep,
            surface_convection = "energyplus"
        ),
        class = "destep_geometry_closure_tolerance"
    )
    expect_false(
        "SurfaceProperty:ConvectionCoefficients" %in%
            automatic$object$class_name
    )
    expect_equal(attr(automatic, "table"), attr(surface, "table"))
    expect_equal(
        automatic$value,
        surface$value[class_name == "BuildingSurface:Detailed"]
    )
    expect_named(surface, c("object", "value"))
    expect_setequal(
        unique(surface$object$class_name),
        c("BuildingSurface:Detailed", "SurfaceProperty:ConvectionCoefficients")
    )
    table <- attr(surface, "table")
    expect_s3_class(table, "data.table")
    # Exterior wall, roof, and exposed-floor libraries are already
    # outside-to-inside in DeST; ground and room-to-room faces still need
    # reciprocal directional stacks.
    expect_false(any(table[
        KIND_ENCLOSURE %in% c(1L, 3L, 6L) & BOUNDARY == "Outdoors",
        grepl(" \\[Reverse\\]", CONSTRUCTION)
    ]))
    expect_true(all(table[
        KIND_ENCLOSURE == 4L,
        grepl(" \\[Reverse\\]", CONSTRUCTION)
    ]))
    interior_wall <- table[
        KIND_ENCLOSURE == 2L & BOUNDARY == "Surface",
        .(HAS_REVERSE = grepl(" \\[Reverse\\]", CONSTRUCTION)),
        by = ENCLOSURE_ID
    ]
    expect_gt(nrow(interior_wall), 0L)
    expect_true(all(
        interior_wall[,
            .(
                RECIPROCAL = identical(
                    sort(unique(HAS_REVERSE)),
                    c(FALSE, TRUE)
                )
            ),
            by = ENCLOSURE_ID
        ]$RECIPROCAL
    ))
    # DeST inner-wall layers ascend from SIDE1 to SIDE2. EnergyPlus lists each
    # room-side construction outside-to-inside, so SIDE1 must receive the
    # reverse stack and SIDE2 must retain the source stack.
    expect_true(all(table[
        KIND_ENCLOSURE == 2L & BOUNDARY == "Surface" & SIDE == 1L,
        grepl(" \\[Reverse\\]", CONSTRUCTION)
    ]))
    expect_false(any(table[
        KIND_ENCLOSURE == 2L & BOUNDARY == "Surface" & SIDE == 2L,
        grepl(" \\[Reverse\\]", CONSTRUCTION)
    ]))
    convection <- surface$value[
        class_name == "SurfaceProperty:ConvectionCoefficients"
    ]
    inside <- convection[
        field_name == "Convection Coefficient 1 Location"
    ]
    outside <- convection[
        field_name == "Convection Coefficient 2 Location"
    ]
    expect_equal(nrow(inside), data.table::uniqueN(table$OUTPUT_ID))
    expect_true(all(inside$value_chr == "Inside"))
    expect_equal(
        nrow(outside),
        data.table::uniqueN(table[BOUNDARY == "Outdoors"]$OUTPUT_ID)
    )
    expect_true(all(outside$value_chr == "Outside"))
    expect_equal(
        unique(convection[
            field_name == "Convection Coefficient 2",
            value_num
        ]),
        23.3,
        tolerance = 1e-6
    )
})

test_that("maps DeST surface coefficients by room-side boundary semantics", {
    surface <- data.table::data.table(
        OUTPUT_ID = c("out", "a", "b"),
        NAME = c("Exterior", "Partition A", "Partition B"),
        KIND_ENCLOSURE = c(1L, 2L, 2L),
        CONSTRUCTION = c("Wall", "Partition", "Partition [Reverse]"),
        BOUNDARY = c("Outdoors", "Surface", "Surface"),
        BOUNDARY_OBJECT = c(NA, "Partition B", "Partition A"),
        INSIDE_SOLAR_ABSORPTANCE = c(0.55, 0.40, 0.60),
        INSIDE_THERMAL_ABSORPTANCE = c(0.85, 0.80, 0.75),
        INSIDE_CONVECTION_COEFFICIENT = c(3.5, 3.5, 3.5),
        OUTSIDE_SOLAR_ABSORPTANCE = c(0.55, 0.1, 0.2),
        OUTSIDE_THERMAL_ABSORPTANCE = c(0.85, 0.1, 0.2),
        OUTSIDE_CONVECTION_COEFFICIENT = c(23.3, 3.5, 3.5)
    )

    mapped <- surface_property__assign_constructions(surface)
    convection <- surface_property__convection_values(mapped)

    # Interzone outside faces are the peer's room-side faces, not the raw peer
    # columns inherited through later geometry rewiring.
    expect_equal(
        mapped[NAME == "Partition A", OUTSIDE_SOLAR_ABSORPTANCE],
        0.60
    )
    expect_equal(
        mapped[NAME == "Partition B", OUTSIDE_THERMAL_ABSORPTANCE],
        0.80
    )
    expect_match(mapped[NAME == "Exterior", CONSTRUCTION], "o-a0.55-e0.85")
    expect_equal(convection[[1L]]$convection_coefficient_2, 23.3)
    expect_null(convection[[2L]]$convection_coefficient_2)
})

test_that("preserves exposed-floor zeros independently of adjacent wall values", {
    surface <- data.table::data.table(
        OUTPUT_ID = c("exposed-floor", "exterior-wall"),
        NAME = c("Exposed Floor", "Exterior Wall"),
        KIND_ENCLOSURE = c(6L, 1L),
        CONSTRUCTION = c("Floor Construction", "Wall Construction"),
        BOUNDARY = "Outdoors",
        BOUNDARY_OBJECT = NA_character_,
        INSIDE_SOLAR_ABSORPTANCE = c(0.1, 0.6),
        INSIDE_THERMAL_ABSORPTANCE = c(0.9, 0.9),
        INSIDE_CONVECTION_COEFFICIENT = 3.5,
        OUTSIDE_SOLAR_ABSORPTANCE = c(0.0, 0.6),
        OUTSIDE_THERMAL_ABSORPTANCE = c(0.0, 0.9),
        OUTSIDE_CONVECTION_COEFFICIENT = c(2.2, 23.3)
    )

    mapped <- surface_property__assign_constructions(surface)
    floor <- mapped[OUTPUT_ID == "exposed-floor"]

    expect_equal(floor$INSIDE_SOLAR_ABSORPTANCE, 0.1)
    expect_equal(floor$INSIDE_THERMAL_ABSORPTANCE, 0.9)
    expect_equal(floor$OUTSIDE_SOLAR_ABSORPTANCE, 0.0)
    expect_equal(floor$OUTSIDE_THERMAL_ABSORPTANCE, 1e-6)
    expect_match(
        floor$CONSTRUCTION,
        "i-a0.1-e0.9 o-a0-e1e-06",
        fixed = TRUE
    )
})

test_that("preserves exposed-floor zeros without another exterior reference", {
    surface <- data.table::data.table(
        OUTPUT_ID = "exposed-floor",
        NAME = "Exposed Floor",
        KIND_ENCLOSURE = 6L,
        CONSTRUCTION = "Floor Construction",
        BOUNDARY = "Outdoors",
        BOUNDARY_OBJECT = NA_character_,
        INSIDE_SOLAR_ABSORPTANCE = 0.1,
        INSIDE_THERMAL_ABSORPTANCE = 0.9,
        INSIDE_CONVECTION_COEFFICIENT = 3.5,
        OUTSIDE_SOLAR_ABSORPTANCE = 0.0,
        OUTSIDE_THERMAL_ABSORPTANCE = 0.0,
        OUTSIDE_CONVECTION_COEFFICIENT = 2.2
    )

    mapped <- surface_property__assign_constructions(surface)

    expect_equal(mapped$OUTSIDE_SOLAR_ABSORPTANCE, 0.0)
    expect_equal(mapped$OUTSIDE_THERMAL_ABSORPTANCE, 1e-6)
    expect_match(mapped$CONSTRUCTION, " o-a0-e1e-06", fixed = TRUE)
})

test_that("maps DeST thermal-emissivity limits to valid EnergyPlus values", {
    surface <- data.table::data.table(
        OUTPUT_ID = "exterior-wall",
        NAME = "Exterior Wall",
        KIND_ENCLOSURE = 1L,
        CONSTRUCTION = "Wall Construction",
        BOUNDARY = "Outdoors",
        BOUNDARY_OBJECT = NA_character_,
        INSIDE_SOLAR_ABSORPTANCE = 0.6,
        INSIDE_THERMAL_ABSORPTANCE = 0.0,
        INSIDE_CONVECTION_COEFFICIENT = 3.5,
        OUTSIDE_SOLAR_ABSORPTANCE = 0.6,
        OUTSIDE_THERMAL_ABSORPTANCE = 1.0,
        OUTSIDE_CONVECTION_COEFFICIENT = 23.3
    )

    mapped <- surface_property__assign_constructions(surface)

    expect_equal(mapped$INSIDE_THERMAL_ABSORPTANCE, 1e-6)
    expect_equal(mapped$OUTSIDE_THERMAL_ABSORPTANCE, 0.99999)
})

test_that("surface construction clones preserve reciprocal layer order", {
    surface <- data.table::data.table(
        OUTPUT_ID = c("a", "b"),
        NAME = c("A", "B"),
        KIND_ENCLOSURE = 2L,
        CONSTRUCTION = c("Partition", "Partition [Reverse]"),
        BOUNDARY = "Surface",
        BOUNDARY_OBJECT = c("B", "A"),
        INSIDE_SOLAR_ABSORPTANCE = c(0.40, 0.60),
        INSIDE_THERMAL_ABSORPTANCE = c(0.80, 0.75),
        INSIDE_CONVECTION_COEFFICIENT = 3.5,
        OUTSIDE_SOLAR_ABSORPTANCE = c(0.1, 0.2),
        OUTSIDE_THERMAL_ABSORPTANCE = c(0.1, 0.2),
        OUTSIDE_CONVECTION_COEFFICIENT = 3.5
    )
    mapped <- surface_property__assign_constructions(surface)
    material <- data.table::data.table(
        MATERIAL_ID = 1:3,
        LENGTH = 100,
        MATERIAL_NAME = c("Outer", "Core", "Inner"),
        MATERIAL_CONDUCTIVITY = 1.0,
        MATERIAL_DENSITY = 1000,
        MATERIAL_SPECIFIC_HEAT = 1000,
        THERMAL_ABSORPTANCE = 0.9,
        SOLAR_ABSORPTANCE = 0.7,
        VISIBLE_ABSORPTANCE = 0.7
    )
    construction <- data.table::data.table(
        ID = 1L,
        KIND = 2L,
        name = c("Partition", "Partition [Reverse]"),
        value = list(
            c("Partition", "Outer", "Core", "Inner"),
            c("Partition [Reverse]", "Inner", "Core", "Outer")
        )
    )

    converted <- const__apply_surface_properties(
        list(material = material, construction = construction),
        mapped
    )
    forward_name <- mapped[NAME == "A", CONSTRUCTION]
    reverse_name <- mapped[NAME == "B", CONSTRUCTION]
    forward <- converted$construction[name == forward_name, value][[1L]][-1L]
    reverse <- converted$construction[name == reverse_name, value][[1L]][-1L]

    expect_equal(reverse, rev(forward))
    expect_equal(
        converted$material[
            MATERIAL_NAME == forward[[1L]],
            .(SOLAR_ABSORPTANCE, THERMAL_ABSORPTANCE)
        ],
        data.table::data.table(
            SOLAR_ABSORPTANCE = 0.60,
            THERMAL_ABSORPTANCE = 0.75
        )
    )
    expect_true(all(converted$material$VISIBLE_ABSORPTANCE == 0.7))
})

test_that("splits one-layer constructions across unequal face properties", {
    material <- data.table::data.table(
        MATERIAL_ID = 1L,
        LENGTH = 6.096,
        MATERIAL_NAME = "Effective panel",
        MATERIAL_CONDUCTIVITY = 0.03077,
        MATERIAL_DENSITY = 2470,
        MATERIAL_SPECIFIC_HEAT = 750,
        THERMAL_ABSORPTANCE = 0.9,
        SOLAR_ABSORPTANCE = 0.7,
        VISIBLE_ABSORPTANCE = 0.7
    )
    construction <- data.table::data.table(
        ID = 1L,
        KIND = -2L,
        name = "Panel",
        value = list(c("Panel", "Effective panel"))
    )
    property <- data.table::data.table(
        BASE_CONSTRUCTION = "Panel",
        CONSTRUCTION = "Panel [DeST properties]",
        INSIDE_SOLAR_ABSORPTANCE = 0.6,
        INSIDE_THERMAL_ABSORPTANCE = 0.9,
        OUTSIDE_SOLAR_ABSORPTANCE = 0.1,
        OUTSIDE_THERMAL_ABSORPTANCE = 0.9
    )

    converted <- const__apply_surface_properties(
        list(material = material, construction = construction),
        property
    )
    layers <- converted$construction[
        name == property$CONSTRUCTION,
        value
    ][[1L]][-1L]
    split <- converted$material[MATERIAL_NAME %in% layers]

    expect_length(layers, 2L)
    expect_equal(sum(split$LENGTH), material$LENGTH)
    expect_equal(
        sum(split$LENGTH / split$MATERIAL_CONDUCTIVITY),
        material$LENGTH / material$MATERIAL_CONDUCTIVITY
    )
    expect_equal(
        sum(
            split$LENGTH * split$MATERIAL_DENSITY * split$MATERIAL_SPECIFIC_HEAT
        ),
        material$LENGTH *
            material$MATERIAL_DENSITY *
            material$MATERIAL_SPECIFIC_HEAT
    )
    expect_equal(split$SOLAR_ABSORPTANCE, c(0.1, 0.6))
    expect_true(all(split$THERMAL_ABSORPTANCE == 0.9))
})

test_that("surface polygon simplification removes only redundant vertices", {
    polygon <- data.table::data.table(
        POINT_NO = 0:5,
        POINT_X = c(0, 1, 2, 2, 1, 0),
        POINT_Y = c(0, 0, 0, 1, 1, 1),
        POINT_Z = 0
    )

    simplified <- surface__simplify_polygon(polygon)

    expect_equal(nrow(simplified), 4L)
    expect_equal(simplified$POINT_NO, 0:3)
    expect_equal(
        simplified[, .(POINT_X, POINT_Y, POINT_Z)],
        data.table::data.table(
            POINT_X = c(0, 2, 2, 0),
            POINT_Y = c(0, 0, 1, 1),
            POINT_Z = 0
        )
    )
})

test_that("surface polygons start at EnergyPlus's declared upper-left corner", {
    polygon <- list(
        east = rbind(
            c(8, 4.5, 0.2),
            c(8, 4.5, 2.2),
            c(8, 1.5, 2.2),
            c(8, 1.5, 0.2)
        ),
        west = rbind(
            c(0, 1.5, 0.2),
            c(0, 1.5, 2.2),
            c(0, 4.5, 2.2),
            c(0, 4.5, 0.2)
        ),
        north = rbind(
            c(0, 6, 0),
            c(0, 6, 2.7),
            c(8, 6, 2.7),
            c(8, 6, 0)
        ),
        south = rbind(
            c(8, 0, 0),
            c(8, 0, 2.7),
            c(0, 0, 2.7),
            c(0, 0, 0)
        ),
        roof = rbind(
            c(8, 0, 2.7),
            c(8, 6, 2.7),
            c(0, 6, 2.7),
            c(0, 0, 2.7)
        ),
        floor = rbind(
            c(8, 6, 0),
            c(8, 0, 0),
            c(0, 0, 0),
            c(0, 6, 0)
        )
    )
    expected <- list(
        east = c(8, 1.5, 2.2),
        west = c(0, 4.5, 2.2),
        north = c(8, 6, 2.7),
        south = c(0, 0, 2.7),
        roof = c(0, 6, 2.7),
        floor = c(0, 0, 0)
    )

    for (name in names(polygon)) {
        input <- data.table::as.data.table(polygon[[name]])
        data.table::setnames(input, c("POINT_X", "POINT_Y", "POINT_Z"))
        input[, POINT_NO := seq_len(.N) - 1L]
        normal_before <- geom__unit_normal(input)
        area_before <- geom__polygon_area(input)

        normalized <- geom__rotate_polygon_upper_left(input)

        expect_equal(
            unname(unlist(normalized[1L, .(POINT_X, POINT_Y, POINT_Z)])),
            expected[[name]],
            info = name
        )
        expect_equal(geom__unit_normal(normalized), normal_before, info = name)
        expect_equal(geom__polygon_area(normalized), area_before, info = name)
        expect_setequal(
            do.call(paste, c(input[, .(POINT_X, POINT_Y, POINT_Z)], sep = ":")),
            do.call(
                paste,
                c(
                    normalized[, .(POINT_X, POINT_Y, POINT_Z)],
                    sep = ":"
                )
            )
        )
    }
})

test_that("EnergyPlus geometry assumptions are versioned in one profile", {
    reference <- expect_silent(eplus_geom__profile("23.1"))
    expect_true(reference$validated)
    expect_equal(reference$coordinate_distance, 0.01)
    expect_equal(reference$closure_vertex_distance, 0.01)

    expect_warning(
        later <- eplus_geom__profile("24.1"),
        "validated against EnergyPlus 23.1"
    )
    expect_false(later$validated)
    expect_equal(later$reference_version, numeric_version("23.1"))
})

test_that("adjacent triangles merge into one convex surface", {
    # Two triangles are an auxiliary mesh of one rectangular DeST face. Their
    # common diagonal may be deleted without changing its boundary or area.
    surface <- data.table::rbindlist(list(
        data.table::data.table(
            PART = 1L,
            POINT_NO = 0:2,
            POINT_X = c(0, 4, 4),
            POINT_Y = c(0, 0, 3),
            POINT_Z = 0
        ),
        data.table::data.table(
            PART = 2L,
            POINT_NO = 0:2,
            POINT_X = c(0, 4, 0),
            POINT_Y = c(0, 3, 3),
            POINT_Z = 0
        )
    ))

    merged <- surface__merge_convex_parts(surface)

    expect_equal(data.table::uniqueN(merged$PART), 1L)
    expect_equal(nrow(merged), 4L)
    following <- seq_len(nrow(merged)) %% nrow(merged) + 1L
    expect_equal(
        abs(sum(
            merged$POINT_X *
                merged$POINT_Y[following] -
                merged$POINT_X[following] * merged$POINT_Y
        )) /
            2.0,
        12.0
    )
})

test_that("convex-part merging rejects a non-planar polygon", {
    # The two triangles are individually planar, but deleting their shared
    # diagonal would create a quadrilateral with one vertex 5 mm off plane.
    surface <- data.table::rbindlist(list(
        data.table::data.table(
            PART = 1L,
            POINT_NO = 0:2,
            POINT_X = c(0, 1, 1),
            POINT_Y = c(0, 0, 1),
            POINT_Z = c(0, 0, 0.005)
        ),
        data.table::data.table(
            PART = 2L,
            POINT_NO = 0:2,
            POINT_X = c(0, 1, 0),
            POINT_Y = c(0, 1, 1),
            POINT_Z = c(0, 0.005, 0)
        )
    ))

    merged <- surface__merge_convex_parts(surface)

    expect_equal(data.table::uniqueN(merged$PART), 2L)
})

test_that("convex-part merging is independent of part traversal order", {
    # This concave polygon has several legal local merges. A stable merge policy
    # must not emit a different number of surfaces when part rows are reordered.
    polygon <- data.table::data.table(
        VERTEX = 1:10,
        POINT_NO = 0:9,
        POINT_X = c(
            0.989748214576005,
            0.585767730908300,
            -0.816434295808996,
            -2.094515643110917,
            -2.547365040702336,
            1.300509053897897,
            1.800024425395703,
            1.145867268715982,
            2.272045004267075,
            1.060572752934251
        ),
        POINT_Y = c(
            0.406676496443011,
            1.837457261328847,
            2.070688628786034,
            1.486738100706347,
            -2.118904161038813,
            -1.350773412036306,
            -1.142463925382292,
            -0.553086176099802,
            -0.853681349296615,
            -0.197666698611673
        ),
        POINT_Z = 0
    )
    profile <- eplus_geom__profile()
    profile$coordinate_distance <- 1e-5
    triangle <- surface__triangulate_polygon(polygon, profile = profile)
    reorder_parts <- function(order) {
        data.table::rbindlist(lapply(order, function(part) {
            triangle[PART == part]
        }))
    }

    first <- surface__merge_convex_parts(
        reorder_parts(c(5L, 7L, 6L, 1L, 3L, 2L, 4L, 8L)),
        distance_tolerance = 1e-5
    )
    second <- surface__merge_convex_parts(
        reorder_parts(c(3L, 5L, 2L, 1L, 6L, 4L, 8L, 7L)),
        distance_tolerance = 1e-5
    )

    expect_equal(
        data.table::uniqueN(first$PART),
        data.table::uniqueN(second$PART)
    )
})

test_that("EnergyPlus-compatible closure accepts a harmless T-junction", {
    # One wall splits the roof's long edge at x=1. EnergyPlus inserts that
    # collinear point during its second closure pass, so no exported split is
    # required. Moving the point off the edge creates a genuine open shell.
    face <- function(name, type, coordinate) {
        data.table::data.table(
            ROOM = "Room",
            OUTPUT_ID = name,
            TYPE = type,
            POINT_NO = seq_len(nrow(coordinate)) - 1L,
            POINT_X = coordinate[, 1L],
            POINT_Y = coordinate[, 2L],
            POINT_Z = coordinate[, 3L]
        )
    }
    shell <- data.table::rbindlist(list(
        face(
            "Floor",
            "Floor",
            rbind(
                c(0, 0, 0),
                c(0, 1, 0),
                c(2, 1, 0),
                c(2, 0, 0)
            )
        ),
        face(
            "Roof",
            "Roof",
            rbind(
                c(0, 0, 1),
                c(2, 0, 1),
                c(2, 1, 1),
                c(0, 1, 1)
            )
        ),
        face(
            "Front",
            "Wall",
            rbind(
                c(0, 0, 0),
                c(2, 0, 0),
                c(2, 0, 1),
                c(1, 0, 1),
                c(0, 0, 1)
            )
        ),
        face(
            "Back",
            "Wall",
            rbind(
                c(2, 1, 0),
                c(0, 1, 0),
                c(0, 1, 1),
                c(2, 1, 1)
            )
        ),
        face(
            "Left",
            "Wall",
            rbind(
                c(0, 1, 0),
                c(0, 0, 0),
                c(0, 0, 1),
                c(0, 1, 1)
            )
        ),
        face(
            "Right",
            "Wall",
            rbind(
                c(2, 0, 0),
                c(2, 1, 0),
                c(2, 1, 1),
                c(2, 0, 1)
            )
        )
    ))

    expect_length(surface__energyplus_unclosed_rooms(shell), 0L)
    open_shell <- data.table::copy(shell)
    open_shell[OUTPUT_ID == "Front" & POINT_NO == 3L, POINT_Z := 0.95]
    expect_equal(surface__energyplus_unclosed_rooms(open_shell), "Room")
})

test_that("unrelated source vertices keep their saved positions", {
    point <- data.table::data.table(
        PLANE = rep(1:2, each = 2L),
        POINT_NO = rep(0:1, 2L),
        POINT_X = c(188.175, 190.192, 188.183, 190.192),
        POINT_Y = c(-16.750, -14.488, -16.742, -14.480),
        POINT_Z = 2.9
    )
    expect_identical(surface__canonical_source_vertices(point), point)

    sliver <- data.table::data.table(
        POINT_NO = 0:2,
        POINT_X = c(1712.530, 1712.531, 1712.531),
        POINT_Y = c(-1088.336, -1088.334, -1041.716),
        POINT_Z = -10.2
    )
    expect_equal(nrow(surface__simplify_polygon(sliver)), 0L)
})

# Calculate one polygon's unit normal and area for geometry-invariant tests.
surface__metrics <- function(surface) {
    following <- seq_len(nrow(surface)) %% nrow(surface) + 1L
    normal <- c(
        sum(
            (surface$POINT_Y - surface$POINT_Y[following]) *
                (surface$POINT_Z + surface$POINT_Z[following])
        ),
        sum(
            (surface$POINT_Z - surface$POINT_Z[following]) *
                (surface$POINT_X + surface$POINT_X[following])
        ),
        sum(
            (surface$POINT_X - surface$POINT_X[following]) *
                (surface$POINT_Y + surface$POINT_Y[following])
        )
    )
    magnitude <- sqrt(sum(normal^2))
    list(
        NX = normal[[1L]] / magnitude,
        NY = normal[[2L]] / magnitude,
        NZ = normal[[3L]] / magnitude,
        AREA = magnitude / 2.0,
        N_VERTEX = nrow(surface)
    )
}

test_that("windows are clipped to exact host surface parts", {
    host <- data.table::rbindlist(list(
        data.table::data.table(
            ID = 20L,
            PART = 1L,
            NAME = "Wall [1]",
            POINT_NO = 0:2,
            POINT_X = c(0, 4, 4),
            POINT_Y = 0,
            POINT_Z = c(0, 0, 4)
        ),
        data.table::data.table(
            ID = 20L,
            PART = 2L,
            NAME = "Wall [2]",
            POINT_NO = 0:2,
            POINT_X = c(0, 4, 0),
            POINT_Y = 0,
            POINT_Z = c(0, 4, 4)
        )
    ))
    window <- data.table::data.table(
        OUTPUT_ID = "200-1",
        ID = 200L,
        SURFACE_ID = 20L,
        ORIGINAL_NAME = "Window",
        NAME = "Window",
        SIDE = 1L,
        INTERZONE = FALSE,
        BOUNDARY_OBJECT = NA_character_,
        POINT_NO = 0:3,
        POINT_X = c(1, 3, 3, 1),
        POINT_Y = 0,
        POINT_Z = c(1, 1, 3, 3)
    )

    split <- subsurface__split_by_surface(window, host)
    metric <- split[, surface__metrics(.SD), by = "OUTPUT_PART_ID"]

    expect_equal(data.table::uniqueN(split$OUTPUT_PART_ID), 2L)
    expect_true(all(metric$N_VERTEX == 3L))
    expect_equal(sum(metric$AREA), 4.0, tolerance = 1e-8)
    expect_setequal(unique(split$SURFACE_NAME), c("Wall [1]", "Wall [2]"))

    outside <- data.table::copy(window)
    outside[, POINT_X := POINT_X + 10]
    expect_error(
        subsurface__split_by_surface(outside, host),
        "Could not place DeST subsurface"
    )
})

test_that("window-aware host triangulation avoids sub-centimetre slivers", {
    host <- data.table::data.table(
        POINT_NO = 0:4,
        POINT_X = c(99.324, 99.421, 99.421, 81.774, 81.774),
        POINT_Y = 9.075,
        POINT_Z = c(9.0, 9.0, 13.5, 13.5, 9.0)
    )
    window <- data.table::data.table(
        POINT_NO = 0:3,
        POINT_X = c(85.604, 95.591, 95.591, 85.604),
        POINT_Y = 9.075,
        POINT_Z = c(9.977, 9.977, 12.523, 12.523)
    )

    triangulated <- surface__triangulate_polygon(host, window)
    clipped <- lapply(unique(triangulated$PART), function(part) {
        subsurface__clip_polygon(window, triangulated[PART == part])
    })
    clipped <- Filter(Negate(is.null), clipped)

    host_metric <- triangulated[, surface__metrics(.SD), by = "PART"]
    clipped_area <- sum(vapply(
        clipped,
        function(value) surface__metrics(value)$AREA,
        numeric(1L)
    ))
    expect_equal(
        sum(host_metric$AREA),
        surface__metrics(host)$AREA,
        tolerance = 1e-8
    )
    expect_equal(clipped_area, surface__metrics(window)$AREA, tolerance = 1e-6)
})

test_that("window-clear triangulation search has a deterministic state cap", {
    polygon <- data.table::data.table(
        POINT_NO = 0:5,
        POINT_X = c(0, 3, 3, 1, 1, 0),
        POINT_Y = c(0, 0, 1, 1, 3, 3),
        POINT_Z = 0
    )
    avoid <- data.table::data.table(
        POINT_X = 0.5,
        POINT_Y = 0.5,
        POINT_Z = 0
    )
    profile <- eplus_geom__profile()
    profile$triangulation_max_states <- 0L

    triangle <- surface__triangulate_polygon(polygon, avoid, profile)

    expect_equal(data.table::uniqueN(triangle$PART), nrow(polygon) - 2L)
    expect_equal(
        sum(triangle[, surface__metrics(.SD)$AREA, by = "PART"]$V1),
        surface__metrics(polygon)$AREA,
        tolerance = 1e-8
    )
})

test_that("source boundary handling preserves an already convex polygon", {
    # Build one reciprocal floor/ceiling pair whose common footprint is a
    # rectangle. Source boundary handling should retain that exact
    # four-vertex polygon instead of introducing two unnecessary triangles.
    make_face <- function(
        id,
        name,
        type,
        z,
        azimuth,
        side,
        construction,
        peer
    ) {
        data.table::data.table(
            ID = id,
            PLANE = id,
            NAME = name,
            ORIGINAL_NAME = name,
            KIND_ENCLOSURE = 5L,
            TYPE_SURFACE = 0L,
            TYPE = type,
            SIDE = side,
            CONSTRUCTION = construction,
            ROOM = "Room",
            BOUNDARY = "Surface",
            BOUNDARY_OBJECT = peer,
            STOREY_ID = 1L,
            STOREY_NAME = "Typical",
            STOREY_MULTIPLIER = 5L,
            AZIMUTH = azimuth,
            TILT = 0.0,
            OUTPUT_ID = sprintf("%d-1", id),
            PART = 1L,
            PART_COUNT = 1L,
            POINT_NO = 0:3,
            POINT_X = c(0, 4, 4, 0),
            POINT_Y = c(0, 0, 3, 3),
            POINT_Z = z
        )
    }
    surface <- data.table::rbindlist(list(
        make_face(
            1L,
            "Floor",
            "Floor",
            0.0,
            999.0,
            1L,
            "Slab [Reverse]",
            "Ceiling"
        ),
        make_face(
            2L,
            "Ceiling",
            "Ceiling",
            3.0,
            -999.0,
            2L,
            "Slab",
            "Floor"
        )
    ))

    # This isolated floor/ceiling pair tests graph semantics, not a room shell.
    local_mocked_bindings(surface__repair_local_edges = function(
        surface,
        window,
        profile
    ) {
        surface
    })
    converted <- surface__preserve_boundaries(surface)
    object <- converted[,
        .(
            N_VERTEX = .N,
            AREA = surface__metrics(.SD)$AREA,
            BOUNDARY_OBJECT = BOUNDARY_OBJECT[[1L]],
            BOUNDARY_MODE = BOUNDARY_MODE[[1L]]
        ),
        by = .(OUTPUT_ID, NAME)
    ]

    expect_equal(object$N_VERTEX, c(4L, 4L))
    expect_equal(object$AREA, c(12.0, 12.0))
    expect_equal(object$BOUNDARY_MODE, rep("source", 2L))
    expect_equal(object$BOUNDARY_OBJECT, c("Ceiling", "Floor"))
})

test_that("unmultiplied fixture preserves source boundaries, geometry, and closure", {
    skip_on_cran()

    src <- ensure_dest_sqlite_file()
    on.exit(DBI::dbDisconnect(src), add = TRUE)
    path_tmp <- tempfile(fileext = ".sql")
    dest <- DBI::dbConnect(RSQLite::SQLite(), path_tmp)
    on.exit(
        {
            DBI::dbDisconnect(dest)
            unlink(path_tmp)
        },
        add = TRUE
    )
    RSQLite::sqliteCopyDatabase(src, dest)
    conv__update_names(dest)

    # Multipliers must not change source boundaries or require synthetic peers.
    expect_warning(
        multiplied <- surface__convert(dest, eplusr::empty_idf(23.1)),
        "joining unequal storey multipliers"
    )
    original_table <- attr(multiplied, "table")
    expect_identical(original_table$BOUNDARY, original_table$SOURCE_BOUNDARY)
    # Material-property variants append an optical-property suffix to the
    # source construction name; they must retain the original layer assembly.
    expect_true(all(startsWith(
        original_table$CONSTRUCTION,
        original_table$SOURCE_CONSTRUCTION
    )))
    expect_gt(nrow(attr(multiplied, "boundary_diagnostics")), 0L)
    DBI::dbExecute(dest, "UPDATE STOREY SET MULTIPLE = 1")
    surface <- attr(surface__convert(dest, eplusr::empty_idf(23.1)), "table")
    # Keep the fixture close to DeST's 560 SURFACE rows while allowing the
    # separate room-side objects required by EnergyPlus interzone boundaries.
    expect_lte(data.table::uniqueN(surface$OUTPUT_ID), 700L)
    metric <- surface[,
        surface__metrics(.SD),
        by = .(
            OUTPUT_ID,
            ID,
            PLANE,
            NAME,
            ORIGINAL_NAME,
            TYPE,
            KIND_ENCLOSURE,
            BOUNDARY,
            BOUNDARY_OBJECT,
            AZIMUTH,
            TILT,
            ROOM,
            BOUNDARY_MODE,
            SOURCE_TYPE,
            SOURCE_BOUNDARY,
            STOREY_MULTIPLIER
        )
    ]
    metric[,
        EXPECTED := list(list(geom__expected_surface_normal(
            AZIMUTH,
            TILT,
            geom__south_direction(dest)
        ))),
        by = "OUTPUT_ID"
    ]
    metric[,
        ALIGNMENT := NX *
            vapply(EXPECTED, `[[`, numeric(1L), 1L) +
            NY * vapply(EXPECTED, `[[`, numeric(1L), 2L) +
            NZ * vapply(EXPECTED, `[[`, numeric(1L), 3L)
    ]
    expect_true(all(metric$ALIGNMENT > 1.0 - 1e-6))

    original <- unique(metric, by = "ID")
    # With no repetition, all source exterior boundaries must remain exterior.
    expect_equal(
        data.table::uniqueN(
            metric[
                SOURCE_TYPE == "Roof" & SOURCE_BOUNDARY == "Outdoors"
            ]$ID
        ),
        18L
    )
    expect_equal(
        data.table::uniqueN(
            metric[
                SOURCE_TYPE == "Floor" & SOURCE_BOUNDARY == "Outdoors"
            ]$ID
        ),
        2L
    )
    expect_equal(
        data.table::uniqueN(
            metric[
                TYPE == "Roof" & BOUNDARY == "Outdoors"
            ]$ID
        ),
        18L
    )
    expect_equal(
        data.table::uniqueN(
            metric[
                TYPE == "Floor" & BOUNDARY == "Outdoors"
            ]$ID
        ),
        2L
    )
    expect_true(all(original[KIND_ENCLOSURE == 6L]$NZ < 0.0))

    self <- metric[BOUNDARY == "Surface" & NAME == BOUNDARY_OBJECT]
    expect_equal(nrow(self), 0L)

    paired <- metric[BOUNDARY == "Surface" & NAME != BOUNDARY_OBJECT]
    peer <- match(paired$BOUNDARY_OBJECT, metric$NAME)
    expect_false(anyNA(peer))
    expect_equal(metric$BOUNDARY_OBJECT[peer], paired$NAME)
    expect_equal(paired$N_VERTEX, metric$N_VERTEX[peer])
    expect_true(all(
        paired$NX *
            metric$NX[peer] +
            paired$NY * metric$NY[peer] +
            paired$NZ * metric$NZ[peer] <
            -1.0 + 1e-6
    ))
    # This unit-multiplier fixture has equal weights on both sides. Unequal
    # source multipliers are diagnosed separately without rewriting boundaries.
    expect_equal(
        paired$AREA * paired$STOREY_MULTIPLIER,
        metric$AREA[peer] * metric$STOREY_MULTIPLIER[peer],
        tolerance = 1e-8
    )
    expect_true(all(paired$BOUNDARY_MODE == "source"))

    # The 3 mm source edge separation is closed at source tolerance. Preserve
    # the two native volume-check warnings rather than cutting whole rooms.
    source_profile <- eplus_geom__profile()
    source_profile$closure_vertex_distance <- source_profile$plane_distance
    expect_length(
        surface__energyplus_unclosed_rooms(surface, source_profile),
        0L
    )
    expect_setequal(
        surface__energyplus_unclosed_rooms(surface),
        c("2-N-1", "3-N-1")
    )

    raw <- data.table::as.data.table(DBI::dbGetQuery(
        dest,
        "
        SELECT P.PLANE_ID AS PLANE, L.POINT_NO,
            ROUND(PT.X, 3) AS POINT_X, ROUND(PT.Y, 3) AS POINT_Y,
            ROUND(PT.Z, 3) AS POINT_Z
        FROM PLANE P
        INNER JOIN GEOMETRY G ON P.GEOMETRY = G.GEOMETRY_ID
        INNER JOIN LOOP_POINT L ON G.BOUNDARY_LOOP_ID = L.LOOP_ID
        INNER JOIN POINT PT ON L.POINT = PT.POINT_ID
        ORDER BY P.PLANE_ID, L.POINT_NO
    "
    ))
    raw_area <- raw[
        PLANE %in% surface$PLANE,
        .(SOURCE_AREA = surface__metrics(.SD)$AREA),
        by = "PLANE"
    ]
    converted_area <- metric[, .(CONVERTED_AREA = sum(AREA)), by = .(ID, PLANE)]
    area <- merge(converted_area, raw_area, by = "PLANE")
    expect_lt(max(abs(area$CONVERTED_AREA - area$SOURCE_AREA)), 1e-6)
})

test_that("unmultiplied fixture passes EnergyPlus detailed diagnostics", {
    skip_on_cran()

    dest <- destep_test__unmultiplied_fixture()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    warnings <- character()
    idf <- withCallingHandlers(to_idf(dest, 23.1), warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
    })
    # Check each geometry and window-representation diagnostic explicitly;
    # declared source limitations must not hide unexpected warnings.
    expected <- c(
        "EnergyPlus 9.0-9.3 contain a known WindowMaterial:SimpleGlazingSystem",
        "Room shell(s) 2-N-1, 3-N-1 close at the source geometry tolerance",
        "Skipped 9 ROOM row(s)",
        "DeST internal-gain surface distribution",
        "DeST window transmitted-solar distribution",
        "nominal SimpleGlazing K/SC approximation",
        "Mapped 23 DeST ventilation-range ROOM_RELATION row(s)"
    )
    expect_length(warnings, length(expected))
    for (pattern in expected) {
        expect_true(any(grepl(pattern, warnings, fixed = TRUE)))
    }
    # The returned 23.1 schema uses Periodic, not the generation baseline's
    # AverageOverDaysInFrequency spelling, in both audit and persisted header.
    expect_identical(
        attr(idf, "conversion")$simulation$effective$shadow_update_method,
        "Periodic"
    )
    expect_true(any(grepl(
        "shadow_update_method=Periodic",
        unlist(idf$Version$comment()),
        fixed = TRUE
    )))
    # Geometry and reciprocal-construction diagnostics are emitted during
    # input processing, so one simulation day covers them without a full year.
    idf$set(Annual = list(end_month = 1L, end_day_of_month = 1L))
    idf$add("Output:Diagnostics" := list(key_1 = "DisplayExtraWarnings"))
    # Isolate this run's outputs so source databases cannot enter cleanup.
    # The check runner supplies a persistent evidence directory; ordinary test
    # runs retain the same logs for the duration of their temporary session.
    evidence_root <- Sys.getenv(
        "DESTEP_TEST_ENGINE_EVIDENCE",
        unset = file.path(tempdir(), "destep-engine-evidence")
    )
    run_root <- file.path(evidence_root, "runs")
    dir.create(run_root, recursive = TRUE, showWarnings = FALSE)
    run_dir <- tempfile(pattern = "destep-geometry-", tmpdir = run_root)
    dir.create(run_dir)
    idf$save(file.path(run_dir, "geometry.idf"))
    job <- idf$run(
        eplusr::path_eplus_weather(
            23.1,
            "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
        ),
        dir = run_dir
    )
    errors <- job$errors()

    # The blocking job has ended and errors() reads text logs, not SQLite.
    # Save the input/error/end evidence before deleting this run's unneeded
    # result databases. These exact paths and sizes remain in the receipt.
    evidence <- file.path(
        evidence_root,
        basename(run_dir)
    )
    dir.create(evidence, recursive = TRUE, showWarnings = FALSE)
    logs <- list.files(
        run_dir,
        pattern = "[.](idf|err|end)$",
        full.names = TRUE
    )
    # A failed expectation would let the test continue into deletion. Use a
    # hard gate instead and retain the run directory if required evidence is
    # absent or cannot be saved; do not register unconditional exit cleanup.
    stopifnot(all(c("idf", "err", "end") %in% tools::file_ext(logs)))
    stopifnot(all(file.copy(logs, evidence)))
    outputs <- list.files(
        run_dir,
        pattern = "[.](sql|sqlite|eso)$",
        full.names = TRUE
    )
    stopifnot(all(Sys.readlink(outputs) == ""))
    cleanup <- data.frame(
        case = rep("unmultiplied fixture geometry", length(outputs)),
        path = outputs,
        bytes = file.info(outputs)$size,
        reason = rep(
            "Only IDF, error and end logs are required for geometry checks",
            length(outputs)
        ),
        evidence = rep(evidence, length(outputs)),
        status = rep("planned", length(outputs))
    )
    write.csv(cleanup, file.path(evidence, "cleanup.csv"), row.names = FALSE)
    stopifnot(all(unlink(outputs) == 0L), !any(file.exists(outputs)))
    cleanup$status <- rep("deleted", nrow(cleanup))
    write.csv(cleanup, file.path(evidence, "cleanup.csv"), row.names = FALSE)
    stopifnot(unlink(run_dir, recursive = TRUE) == 0L)

    expect_false(any(errors$level %in% c("Severe", "Fatal")))
    # Intact convex faces may contain harmless collinear vertices that the
    # engine removes. Reject enclosure/area/degeneracy failures after cleanup;
    # the deletion notice alone no longer implies invalid geometry.
    geometry_problem <- grepl(
        paste(
            c(
                "not fully enclosed",
                "vertex size mismatch",
                "invalid Building Surface Name",
                "floor area.*differs",
                "zone volume.*differs",
                "degenerate",
                "non-?convex",
                "possibly coincident",
                "InterZone Surface Areas do not match as expected",
                "Base surface does not surround subsurface",
                "Distance between two vertices < \\.01",
                "same materials in the reverse order"
            ),
            collapse = "|"
        ),
        errors$message,
        ignore.case = TRUE
    )
    # These exact source-preserving warnings are registered separately. Keep
    # every other enclosure, nonconvex-consumer or geometry warning blocking.
    known_source <- grepl(
        '^CheckConvexity: Zone="3-N-(1|3|12)", Surface="3-N-(1|3|12) ROOF" is non-convex[.]$|^CalculateZoneVolume: The Zone="(2-N-1|3-N-1)" is not fully enclosed[.]',
        errors$message
    )
    expect_equal(sum(known_source), 5L)
    geometry_problem <- geometry_problem & !known_source
    expect_false(
        any(geometry_problem),
        info = paste(
            errors$message[geometry_problem],
            collapse = "\n"
        )
    )
})

# Create complete room-side metadata around a diagnostic horizontal outline.
geometry_test__face <- function(
    id,
    x,
    y,
    z = 0,
    type = "Roof",
    boundary = "Outdoors"
) {
    data.table::data.table(
        ID = id,
        PLANE = id,
        NAME = paste0("Face ", id),
        ROOM = paste0("Room ", id),
        TYPE_SURFACE = 0L,
        TYPE = type,
        BOUNDARY = boundary,
        BOUNDARY_OBJECT = NA_character_,
        POINT_NO = seq_along(x) - 1L,
        POINT_X = x,
        POINT_Y = y,
        POINT_Z = z
    )
}

test_that("source coordinates are shared by identity, never nearest location", {
    source <- data.table::data.table(
        PLANE = 1:3,
        POINT_NO = 0L,
        POINT_X = c(100, 100.005, 100.006),
        POINT_Y = c(20, 20.015, 20.008),
        POINT_Z = c(0, 0, 0.001)
    )
    expect_identical(surface__canonical_source_vertices(source), source)
    # A complete disjoint triangle cannot warp an otherwise planar roof.
    roof <- geometry_test__face(1L, c(0, 10, 10, 0), c(0, 0, 10, 10))
    other <- geometry_test__face(2L, c(-0.005, -5, -5), c(0, -5, 5), 0.005)
    together <- surface__canonical_source_vertices(rbind(roof, other))
    expect_equal(together[together$ID == 1L, ], roof)
    expect_equal(
        geom__polygon_frame(together[together$ID == 1L, ])$planarity_error,
        0
    )
    repeated <- rbind(roof, roof)
    expect_identical(surface__canonical_source_vertices(repeated), repeated)
    data.table::set(repeated, i = 5L, j = "POINT_Z", value = 0.005)
    expect_error(
        surface__canonical_source_vertices(repeated),
        "inconsistent copies"
    )
})

test_that("unobstructed roofs and nonsolar interior faces remain whole", {
    x <- c(0, 4, 4, 2, 2, 0)
    y <- c(0, 0, 1, 1, 3, 3)
    for (role in c("Roof", "Floor", "Ceiling")) {
        boundary <- if (role == "Roof") "Outdoors" else "Surface"
        source <- geometry_test__face(
            1L,
            x,
            y,
            type = role,
            boundary = boundary
        )
        target <- surface__normalize_topology(source)
        expect_equal(data.table::uniqueN(target$OUTPUT_ID), 1L)
        expect_equal(geom__polygon_area(target), geom__polygon_area(source))
        expect_equal(target$POINT_X, source$POINT_X)
        expect_equal(target$POINT_Y, source$POINT_Y)
        expect_equal(target$ROOM, source$ROOM)
        expect_equal(source$NAME, rep("Face 1", 6L))
    }
})

test_that("only constrained convex clipping consumers request decomposition", {
    roof <- geometry_test__face(1L, c(0, 4, 4, 2, 2, 0), c(0, 0, 1, 1, 3, 3))
    expect_equal(
        data.table::uniqueN(surface__normalize_topology(roof)$OUTPUT_ID),
        1L
    )
    # A genuine external clipping/casting face remains decomposable. This does
    # not change room ownership or generate a surrogate shading calculation.
    wall <- data.table::copy(roof)
    data.table::set(wall, j = "TYPE", value = "Wall")
    data.table::set(wall, j = "POINT_Z", value = wall$POINT_Y)
    data.table::set(wall, j = "POINT_Y", value = 0)
    expect_gt(
        data.table::uniqueN(surface__normalize_topology(wall)$OUTPUT_ID),
        1L
    )
})

test_that("only an adjoining rising exterior wall constrains a concave roof", {
    roof <- geometry_test__face(1L, c(0, 4, 4, 2, 2, 0), c(0, 0, 1, 1, 3, 3))
    wall <- geometry_test__face(
        2L,
        c(1, 3, 3, 1),
        c(0, 0, 0, 0),
        c(0, 0, 2, 2),
        type = "Wall"
    )
    profile <- eplus_geom__profile()
    expect_identical(surface__roof_rising_walls(roof, wall, profile), 2L)
    target <- surface__normalize_topology(rbind(roof, wall))
    parts <- split(target[target$ID == 1L, ], by = "OUTPUT_ID")
    expect_gt(length(parts), 1L)
    expect_equal(
        sum(vapply(parts, geom__polygon_area, numeric(1L))),
        geom__polygon_area(roof)
    )
    expect_setequal(unique(target[target$ID == 1L, ]$ROOM), roof$ROOM[[1L]])

    # Remote heights, a corner, coplanar/below walls and an interior wall must
    # not split the roof. A 5 mm gap is source separation, not a snap request.
    controls <- lapply(seq_len(7L), function(i) data.table::copy(wall))
    data.table::set(controls[[1L]], j = "POINT_Y", value = 100)
    data.table::set(controls[[2L]], j = "POINT_X", value = c(4, 5, 5, 4))
    data.table::set(controls[[3L]], j = "POINT_Z", value = c(-2, -2, 0, 0))
    data.table::set(controls[[4L]], j = "BOUNDARY", value = "Surface")
    data.table::set(controls[[5L]], j = "POINT_Y", value = -0.005)
    data.table::set(controls[[6L]], j = "TYPE_SURFACE", value = 1L)
    data.table::set(controls[[7L]], j = "POINT_Z", value = c(5, 5, 10, 10))
    for (other in controls) {
        value <- surface__normalize_topology(rbind(roof, other))
        expect_equal(data.table::uniqueN(value[value$ID == 1L, ]$OUTPUT_ID), 1L)
    }
    # A higher roof elsewhere is also insufficient, even directly overhead.
    upper <- data.table::copy(roof)
    for (name in c("ID", "PLANE")) {
        data.table::set(upper, j = name, value = 3L)
    }
    data.table::set(upper, j = "POINT_Z", value = 20)
    value <- surface__normalize_topology(rbind(roof, upper))
    expect_equal(data.table::uniqueN(value[value$ID == 1L, ]$OUTPUT_ID), 1L)
    expect_length(
        surface__convex_planes(roof[0], data.table::data.table(), profile),
        0L
    )
})

test_that("local roof incidence is independent of rotation and translation", {
    roof <- geometry_test__face(1L, c(0, 4, 4, 2, 2, 0), c(0, 0, 1, 1, 3, 3))
    wall <- geometry_test__face(
        2L,
        c(1, 3, 3, 1),
        c(0, 0, 0, 0),
        c(0, 0, 2, 2),
        type = "Wall"
    )
    # A reversed, partial wall edge still shares positive roof-boundary length.
    wall <- wall[4:1, ]
    theta <- 0.731
    rotation <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2L)
    for (value in list(roof, wall)) {
        xy <- as.matrix(value[, c("POINT_X", "POINT_Y"), with = FALSE])
        transformed <- xy %*% rotation
        data.table::set(value, j = "POINT_X", value = transformed[, 1L] + 1000)
        data.table::set(value, j = "POINT_Y", value = transformed[, 2L] - 2000)
        data.table::set(value, j = "POINT_Z", value = value$POINT_Z + 30)
    }
    expect_identical(
        surface__roof_rising_walls(roof, wall, eplus_geom__profile()),
        2L
    )
    expect_length(
        surface__roof_rising_walls(roof, wall[0], eplus_geom__profile()),
        0L
    )
})

test_that("second-pass T-junction closure does not request extra cuts", {
    # Two front walls meet whole floor/roof edges halfway along their length.
    # This is a true T-junction, not a removable point within one source face.
    faces <- list(
        rbind(c(0, 0, 0), c(0, 1, 0), c(2, 1, 0), c(2, 0, 0)),
        rbind(c(0, 0, 1), c(2, 0, 1), c(2, 1, 1), c(0, 1, 1)),
        rbind(c(0, 0, 0), c(1, 0, 0), c(1, 0, 1), c(0, 0, 1)),
        rbind(c(1, 0, 0), c(2, 0, 0), c(2, 0, 1), c(1, 0, 1)),
        rbind(c(2, 1, 0), c(0, 1, 0), c(0, 1, 1), c(2, 1, 1)),
        rbind(c(0, 1, 0), c(0, 0, 0), c(0, 0, 1), c(0, 1, 1)),
        rbind(c(2, 0, 0), c(2, 1, 0), c(2, 1, 1), c(2, 0, 1))
    )
    shell <- data.table::rbindlist(lapply(seq_along(faces), function(index) {
        xyz <- faces[[index]]
        value <- geometry_test__face(
            index,
            xyz[, 1L],
            xyz[, 2L],
            xyz[, 3L],
            type = c("Floor", "Roof", rep("Wall", 5L))[[index]]
        )
        data.table::set(value, j = "ROOM", value = "T junction")
        data.table::set(value, j = "OUTPUT_ID", value = as.character(index))
        value
    }))
    expect_length(surface__energyplus_unclosed_rooms(shell), 0L)
    diagnostic <- surface__energyplus_unclosed_rooms(shell, details = TRUE)
    expect_length(diagnostic$output_ids, 0L)
    expect_equal(diagnostic$defects, 0)
    expect_equal(diagnostic$first_defects, 0)
})

test_that("window and door hosts both participate in clipping protection", {
    db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(db))
    tables <- list(
        WINDOW = data.frame(ID = 1L, OF_ENCLOSURE = 1L, MIDDLE_PLANE = 2L),
        DOOR = data.frame(ID = 1L, OF_ENCLOSURE = 1L, MIDDLE_PLANE = 2L),
        MAIN_ENCLOSURE = data.frame(ID = 1L, MIDDLE_PLANE = 10L),
        PLANE = data.frame(PLANE_ID = 2L, GEOMETRY = 2L),
        GEOMETRY = data.frame(GEOMETRY_ID = 2L, BOUNDARY_LOOP_ID = 2L),
        LOOP_POINT = data.frame(LOOP_ID = 2L, POINT_NO = 0:3, POINT = 1:4),
        POINT = data.frame(
            POINT_ID = 1:4,
            X = c(0.2, 0.8, 0.8, 0.2),
            Y = c(0.2, 0.2, 0.8, 0.8),
            Z = 0
        )
    )
    for (name in names(tables)) {
        DBI::dbWriteTable(db, name, tables[[name]])
    }
    opening <- surface__opening_table(db)
    expect_setequal(unique(opening$OPENING_ID), c("WINDOW/1", "DOOR/1"))
    # An interior concave host with only an opaque door still needs convex
    # clipping pieces. Its original ownership and total area remain unchanged.
    host <- geometry_test__face(
        10L,
        c(0, 3, 3, 1, 1, 0),
        c(0, 0, 1, 1, 3, 3),
        type = "Wall",
        boundary = "Surface"
    )
    door <- opening[opening$OPENING_ID == "DOOR/1", ]
    target <- surface__normalize_topology(host, door)
    expect_gt(data.table::uniqueN(target$OUTPUT_ID), 1L)
    parts <- split(target, target$OUTPUT_ID)
    expect_true(surface__cut_preserves_openings(
        parts,
        door,
        eplus_geom__profile()
    ))
    expect_equal(
        sum(vapply(parts, geom__polygon_area, numeric(1L))),
        geom__polygon_area(host)
    )
})

test_that("window clipping cannot silently lose source area", {
    host <- geometry_test__face(1L, c(0, 4, 4, 0), c(0, 0, 3, 3))
    data.table::set(host, j = "PART", value = 1L)
    opening <- geometry_test__face(100L, c(3, 5, 5, 3), c(1, 1, 2, 2))
    for (name in c(
        "OUTPUT_ID",
        "ORIGINAL_NAME",
        "SURFACE_ID",
        "SIDE",
        "INTERZONE"
    )) {
        value <- switch(
            name,
            OUTPUT_ID = "100-1",
            ORIGINAL_NAME = "Source window",
            SURFACE_ID = 1L,
            SIDE = 1L,
            INTERZONE = FALSE
        )
        data.table::set(opening, j = name, value = value)
    }
    expect_error(
        subsurface__split_by_surface(opening, host),
        class = "destep_subsurface_coverage_loss"
    )
    # The whole valid opening is preserved without an artificial host split.
    data.table::set(opening, j = "POINT_X", value = c(1, 2.005, 2.005, 1))
    target <- subsurface__split_by_surface(opening, host)
    expect_equal(
        sum(
            target[,
                list(area = geom__polygon_area(.SD)),
                by = "OUTPUT_PART_ID"
            ]$area
        ),
        1.005,
        tolerance = 1e-6
    )
    first <- geometry_test__face(1L, c(0, 2, 2, 0), c(0, 0, 3, 3))
    second <- geometry_test__face(1L, c(2, 4, 4, 2), c(0, 0, 3, 3))
    data.table::set(first, j = "PART", value = 1L)
    data.table::set(second, j = "PART", value = 2L)
    expect_error(
        subsurface__split_by_surface(opening, rbind(first, second)),
        class = "destep_subsurface_coverage_loss"
    )
})
