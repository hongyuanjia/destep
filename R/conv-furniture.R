# Convert the area-dependent furniture coefficient used by the installed DeST
# 0.2.230705 solver. Independent native capacity cases include both sides of
# the 50 m2 branch; the constants are not fitted to whole-building energy use.
furniture__slab_area <- function(area, coefficient) {
    if (any(!is.finite(area) | area <= 0) ||
        any(!is.finite(coefficient) | coefficient < 0)) {
        stop("Furniture requires positive finite room areas and non-negative finite coefficients.",
            call. = FALSE)
    }
    reference <- ifelse(area < 50,
        area * 53064 * 3.6,
        (0.0571 * area + 6.6667) * 1e6
    )
    # Coefficients zero and one do not create a slab in the native solver.
    # Keep the observed 50 m2 branch unchanged, including its small jump.
    pmax(coefficient - 1, 0) * reference / (1930 * 377 * 44 * 0.05)
}

# Resolve the effective room-type coefficient, rather than the unused drawing
# ROOM.FURNITURE_COEF field, and generate a purely convective storage slab.
furniture__convert <- function(dest, ep) {
    if (!db_has_fields(dest, "ROOM", c("ID", "NAME", "AREA", "TYPE")) ||
        !db_has_fields(dest, "ROOM_TYPE_DATA", c("ID", "FURNITURE_COEF"))) {
        return(NULL)
    }
    room <- data.table::as.data.table(DBI::dbGetQuery(dest, "
        SELECT R.ID, R.NAME AS ROOM_NAME, R.AREA,
            T.FURNITURE_COEF AS COEFFICIENT
        FROM ROOM R LEFT JOIN ROOM_TYPE_DATA T ON R.TYPE = T.ID
        ORDER BY R.ID
    "))
    if (!nrow(room)) return(NULL)
    room[, ONE_FACE_AREA := furniture__slab_area(AREA, COEFFICIENT)]
    room <- room[ONE_FACE_AREA > 0]
    if (!nrow(room)) return(NULL)
    room[, NAME := paste(ROOM_NAME, "DeST Furniture")]
    zone_field <- conv__idd_field_name(ep, "InternalMass", 3L)
    mass <- c(list(name = room$NAME,
        construction_name = "DeST Furniture Construction"),
        stats::setNames(list(room$ROOM_NAME), zone_field),
        list(surface_area = 2 * room$ONE_FACE_AREA))
    # EnergyPlus assigns equal temperatures to both faces of InternalMass and
    # reports one face's exchange. Full thickness with twice the one-face area
    # therefore preserves the native two-sided slab capacity and conductance.
    # The positive 1e-6 absorptance is required by the IDD; it suppresses direct
    # radiation to this convective-only native component without an EMS model.
    out <- conv__add(dest, ep,
        "Material" := list(name = "DeST Furniture Material",
            roughness = "MediumSmooth", thickness = 0.05, conductivity = 0.11,
            density = 377, specific_heat = 1930, thermal_absorptance = 1e-6,
            solar_absorptance = 0, visible_absorptance = 0),
        "Construction" := list(name = "DeST Furniture Construction",
            outside_layer = "DeST Furniture Material"),
        "InternalMass" := mass,
        "SurfaceProperty:ConvectionCoefficients" := list(surface_name = room$NAME,
            convection_coefficient_1_location = "Inside",
            convection_coefficient_1_type = "Value", convection_coefficient_1 = 8.7)
    )
    attr(out, "table") <- room
    out
}

# Validate furniture storage and receiving-face metadata at the source mapping
# boundary; all numerical furniture properties remain owned by this module.
furniture__check_source <- function(objects, faces) {
    mass <- Filter(function(o) o[[1L]] == "InternalMass", objects)
    if (!setequal(vapply(mass, `[[`, character(1L), 2L), faces$name[faces$category == "furniture"])) {
        stop("Furniture receiving inventory must match every InternalMass object.", call. = FALSE)
    }
    for (item in mass) {
        construction <- objects[[source__index(objects, "Construction", item[[3L]])]]
        material <- objects[[source__index(objects, "Material", construction[[3L]])]]
        film <- objects[[source__index(objects, "SurfaceProperty:ConvectionCoefficients", item[[2L]])]]
        face <- faces[faces$name == item[[2L]], ]
        if (face$zone != item[[4L]] || length(construction) != 3L ||
            any(abs(as.numeric(material[4:7]) - c(.05, .11, 377, 1930)) > 1e-12) ||
            as.numeric(material[[8L]]) > 1e-10 || any(as.numeric(material[9:10]) != 0) ||
            film[[3L]] != "Inside" || film[[4L]] != "Value" || as.numeric(film[[5L]]) != 8.7 ||
            abs(face$epsilon - as.numeric(material[[8L]])) > 1e-16 ||
            abs(face$area - as.numeric(utils::tail(item, 1L))) > 1e-8) {
            stop("Unsupported furniture storage or radiative properties.", call. = FALSE)
        }
    }
    invisible(NULL)
}
