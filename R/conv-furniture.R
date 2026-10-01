# Convert the area-dependent furniture coefficient used by the installed DeST
# 0.2.230705 solver. Independent native capacity cases include both sides of
# the 50 m2 branch; the constants are not fitted to whole-building energy use.
furniture__slab_area <- function(area, coefficient) {
    if (
        any(!is.finite(area) | area <= 0) ||
            any(!is.finite(coefficient) | coefficient < 0)
    ) {
        stop(
            "Furniture requires positive finite room areas and non-negative finite coefficients.",
            call. = FALSE
        )
    }
    reference <- data.table::fifelse(
        area < 50,
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
    if (
        !db_has_fields(dest, "ROOM", c("ID", "NAME", "AREA", "TYPE")) ||
            !db_has_fields(dest, "ROOM_TYPE_DATA", c("ID", "FURNITURE_COEF"))
    ) {
        return(NULL)
    }
    room <- data.table::as.data.table(DBI::dbGetQuery(
        dest,
        "
        SELECT R.ID, R.NAME AS ROOM_NAME, R.AREA,
            T.FURNITURE_COEF AS COEFFICIENT
        FROM ROOM R LEFT JOIN ROOM_TYPE_DATA T ON R.TYPE = T.ID
        ORDER BY R.ID
    "
    ))
    if (!nrow(room)) {
        return(NULL)
    }
    room[, ONE_FACE_AREA := furniture__slab_area(AREA, COEFFICIENT)]
    room <- room[ONE_FACE_AREA > 0]
    if (!nrow(room)) {
        return(NULL)
    }
    room[, NAME := paste(ROOM_NAME, "DeST Furniture")]
    zone_field <- conv__idd_field_name(ep, "InternalMass", 3L)
    mass <- c(
        list(
            name = room$NAME,
            construction_name = "DeST Furniture Construction"
        ),
        stats::setNames(list(room$ROOM_NAME), zone_field),
        list(surface_area = 2 * room$ONE_FACE_AREA)
    )
    # EnergyPlus assigns equal temperatures to both faces of InternalMass and
    # reports one face's exchange. Full thickness with twice the one-face area
    # therefore preserves the native two-sided slab capacity and conductance.
    # The IDD requires strictly positive absorptance for this storage slab.
    absorptance <- 1e-6
    out <- conv__add(
        dest,
        ep,
        "Material" := list(
            name = "DeST Furniture Material",
            roughness = "MediumSmooth",
            thickness = 0.05,
            conductivity = 0.11,
            density = 377,
            specific_heat = 1930,
            thermal_absorptance = absorptance,
            solar_absorptance = 0,
            visible_absorptance = 0
        ),
        "Construction" := list(
            name = "DeST Furniture Construction",
            outside_layer = "DeST Furniture Material"
        ),
        "InternalMass" := mass,
        "SurfaceProperty:ConvectionCoefficients" := list(
            surface_name = room$NAME,
            convection_coefficient_1_location = "Inside",
            convection_coefficient_1_type = "Value",
            convection_coefficient_1 = 8.7
        )
    )
    attr(out, "table") <- room
    # These identify the tested solver rule, not an inferred database version.
    # Density and specific heat are an equivalent factorization of rho*cp.
    assumptions <- c(
        "Furniture equivalence verified against DeST bshell 0.2.230705 (ThreadPool).",
        "Solver SHA256: 375ed97539714ce77cc68d39bb4ea0a4b2d5fdd8836aaf6cac8a4406be6c7832.",
        "Equivalent convective slab: 0.05 m; 0.11 W/(m K); volumetric capacity 727610 J/(m3 K).",
        "Density 377 kg/m3 and specific heat 1930 J/(kg K) are equivalent factors, not identified furniture materials.",
        "Cross-version applicability and radiant furniture coupling are not established by the capacity/free-float tests."
    )
    data.table::set(
        out$object,
        NULL,
        "comment",
        rep(list(assumptions), nrow(out$object))
    )
    attr(out, "assumptions") <- assumptions
    out
}
