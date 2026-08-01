# Resolve the aggregate thermal and optical properties of every DeST window.
# The returned table is shared by construction and fenestration conversion so
# both paths apply exactly the same validity checks and fallback decisions.
const__window_type_performance <- function(dest) {
    window <- DBI::dbGetQuery(
        dest,
        "SELECT ID AS WINDOW_ID, TYPE AS TYPE_ID,
                WINDOW_CONSTRUCTION AS DETAILED_CONSTRUCTION_ID
         FROM WINDOW"
    )
    data.table::setDT(window)
    if (nrow(window) == 0L) {
        # Preserve a stable schema so models without windows can pass through
        # the same downstream construction code without special-case branches.
        window[, `:=`(
            TYPE_NAME = character(), K = double(), SC = double(),
            LIGHT_TRANS_RATIO = double(), TYPE_RECORD_FOUND = logical(),
            SHGC = double(), TYPE_DATA_VALID = logical(),
            SIMPLE_GLAZING_NAME = character(),
            TYPE_CONSTRUCTION_NAME = character(),
            FALLBACK_REASON = character()
        )]
        return(window)
    }

    # A zero construction reference means DeST expects the matching default.
    # Resolve it here because invalid type records must fall back to the same
    # detailed construction that the original window would have used.
    if ("DEFAULT_SETTING" %in% DBI::dbListTables(dest) &&
        db_has_fields(
            dest, "DEFAULT_SETTING",
            c("TABLE_NAME", "FIELD_NAME", "TYPE", "LONG")
        )) {
        default <- DBI::dbGetQuery(
            dest,
            "SELECT DISTINCT LONG
             FROM DEFAULT_SETTING
             WHERE TABLE_NAME = 'WINDOW'
               AND FIELD_NAME = 'WINDOW_CONSTRUCTION'
               AND TYPE = 2
               AND LONG IS NOT NULL"
        )$LONG
        if (length(default) > 1L) {
            stop("Multiple default DeST window constructions were found.")
        }
        if (length(default) == 1L) {
            window[DETAILED_CONSTRUCTION_ID == 0L,
                DETAILED_CONSTRUCTION_ID := default[[1L]]]
        }
    }

    required <- c("ID", "NAME", "K", "SC", "LIGHT_TRANS_RATIO")
    has_type_data <- "WINDOW_TYPE_DATA" %in% DBI::dbListTables(dest) &&
        db_has_fields(dest, "WINDOW_TYPE_DATA", required)
    if (has_type_data) {
        type <- DBI::dbGetQuery(
            dest,
            "SELECT ID AS TYPE_ID, NAME AS TYPE_NAME, K, SC,
                    LIGHT_TRANS_RATIO
             FROM WINDOW_TYPE_DATA"
        )
        data.table::setDT(type)
        type[, TYPE_RECORD_FOUND := TRUE]
        window <- merge(window, type, by = "TYPE_ID", all.x = TRUE, sort = FALSE)
        window[is.na(TYPE_RECORD_FOUND), TYPE_RECORD_FOUND := FALSE]
    } else {
        window[, `:=`(
            TYPE_NAME = NA_character_, K = NA_real_, SC = NA_real_,
            LIGHT_TRANS_RATIO = NA_real_, TYPE_RECORD_FOUND = FALSE
        )]
    }

    # SQLite/Access drivers can expose numeric columns using different R
    # storage modes, so normalize them before applying physical bounds.
    for (column in c("K", "SC", "LIGHT_TRANS_RATIO")) {
        data.table::set(window, NULL, column, as.double(window[[column]]))
    }
    window[, SHGC := 0.87 * SC]
    window[, TYPE_DATA_VALID :=
        is.finite(K) & K > 0.0 & is.finite(SHGC) & SHGC > 0.0 & SHGC <= 1.0]
    window[!is.finite(LIGHT_TRANS_RATIO) |
        LIGHT_TRANS_RATIO <= 0.0 | LIGHT_TRANS_RATIO > 1.0,
        LIGHT_TRANS_RATIO := NA_real_]

    # EnergyPlus object names are derived from the stable, normalized DeST
    # type name. The suffixes also keep them distinct from SYS_WINDOW objects.
    window[is.na(TYPE_NAME) | !nzchar(TYPE_NAME),
        TYPE_NAME := sprintf("Window Type Data %s", TYPE_ID)]
    window[, `:=`(
        SIMPLE_GLAZING_NAME = sprintf("%s Simple Glazing", TYPE_NAME),
        TYPE_CONSTRUCTION_NAME = sprintf(
            "%s Simple Glazing Construction", TYPE_NAME
        )
    )]
    if (!has_type_data) {
        window[, FALLBACK_REASON :=
            "missing WINDOW_TYPE_DATA table or required fields"]
    } else {
        window[, FALLBACK_REASON := data.table::fcase(
            !TYPE_RECORD_FOUND,
                "missing WINDOW_TYPE_DATA record",
            !is.finite(K) | K <= 0.0,
                "invalid K value",
            !is.finite(SHGC) | SHGC <= 0.0 | SHGC > 1.0,
                "invalid SC value",
            default = NA_character_
        )]
    }
    window
}

# Read opaque and transparent door layers while retaining one representative
# host enclosure for every distinct DeST door construction.
const__door_layers <- function(dest) {
    DBI::dbGetQuery(dest,
        "
        WITH DR AS (
            SELECT DOOR_CONSTRUCTION, MIN(OF_ENCLOSURE) AS OF_ENCLOSURE
            FROM DOOR
            WHERE DOOR_CONSTRUCTION != 0
            GROUP BY DOOR_CONSTRUCTION
            UNION
            SELECT D.LONG AS DOOR_CONSTRUCTION, DR.OF_ENCLOSURE
            FROM DOOR DR
            LEFT JOIN DEFAULT_SETTING D
            ON DR.DOOR_CONSTRUCTION = 0 AND
               D.TABLE_NAME = 'DOOR' AND D.FIELD_NAME = 'DOOR_CONSTRUCTION' AND D.TYPE = 2
            WHERE D.LONG IS NOT NULL
        )
        SELECT * FROM (
            SELECT
                S.DOOR_ID             AS ID,
                S.CNAME               AS NAME,
                -- use KIND = -2 to indicate that the construction is a door
                -2                    AS KIND,
                -- mark the normal layer as 0
                0                     AS LAYER_NO,
                L.LENGTH              AS LENGTH,
                S.MATERIAL_ID         AS MATERIAL_ID,
                M.CNAME               AS MATERIAL_NAME,
                M.CONDUCTIVITY        AS MATERIAL_CONDUCTIVITY,
                M.DENSITY             AS MATERIAL_DENSITY,
                M.SPECIFIC_HEAT       AS MATERIAL_SPECIFIC_HEAT
            FROM DR D
            LEFT JOIN SYS_DOOR S
            ON D.DOOR_CONSTRUCTION = S.DOOR_ID
            LEFT JOIN SYS_MATERIAL M
            ON S.MATERIAL_ID = M.MATERIAL_ID
            LEFT JOIN MAIN_ENCLOSURE E
            ON D.OF_ENCLOSURE = E.ID
            LEFT JOIN (
                SELECT STRUCT_ID, KIND, SUM(LENGTH) AS LENGTH
                FROM (
                    SELECT STRUCT_ID, 1 AS KIND, LENGTH
                    FROM SYS_OUTWALL_MATERIAL
                    UNION
                    SELECT STRUCT_ID, 2 AS KIND, LENGTH
                    FROM SYS_INWALL_MATERIAL
                    UNION
                    SELECT STRUCT_ID, 3 AS KIND, LENGTH
                    FROM SYS_ROOF_MATERIAL
                    UNION
                    SELECT STRUCT_ID, 4 AS KIND, LENGTH
                    FROM SYS_GROUNDFLOOR_MATERIAL
                    UNION
                    SELECT STRUCT_ID, 5 AS KIND, LENGTH
                    FROM SYS_MIDDLEFLOOR_MATERIAL
                    UNION
                    SELECT STRUCT_ID, 6 AS KIND, LENGTH
                    FROM SYS_AIRFLOOR_MATERIAL
                )
                GROUP BY STRUCT_ID, KIND
            ) L
            ON E.CONSTRUCTION = L.STRUCT_ID AND E.KIND = L.KIND
        ) WHERE ID IS NOT NULL -- in case there are no doors
        UNION
        SELECT * FROM (
            SELECT
                S.DOOR_ID             AS ID,
                S.CNAME               AS NAME,
                -- use KIND = -2 to indicate that the construction is a door
                -2                    AS KIND,
                -- mark the glaze layer as 1
                1                     AS LAYER_NO,
                M.THICK               AS LENGTH,
                S.APP_ID              AS MATERIAL_ID,
                M.CNAME               AS MATERIAL_NAME,
                M.CONDUCTIVITY        AS MATERIAL_CONDUCTIVITY,
                M.DENSITY             AS MATERIAL_DENSITY,
                M.SPECIFIC_HEAT       AS MATERIAL_SPECIFIC_HEAT
            FROM DOOR D
            LEFT JOIN SYS_DOOR S
            ON D.DOOR_CONSTRUCTION = S.DOOR_ID AND S.APP_ID != 0 AND S.APP_FLAG = 1
            LEFT JOIN SYS_APP_MATERIAL M
            -- APP_ID identifies the transparent material; MATERIAL_ID is the
            -- separate opaque door-body material.
            ON S.APP_ID = M.APP_MATERIAL_ID
        ) WHERE ID IS NOT NULL -- in case there are no glaze layers
        "
    )
}

# Read every opaque construction layer referenced by MAIN_ENCLOSURE, resolving
# DeST's construction table from the enclosure kind.
const__opaque_layers <- function(dest) {
    # The construction ID refers to different tables based on the kind of the
    # construction:
    #
    # KIND = 1 -> SYS_OUTWALL     -> SYS_OUTWALL_MATERIAL     -> MATERIAL
    # KIND = 2 -> SYS_INWALL      -> SYS_INWALL_MATERIAL      -> MATERIAL
    # KIND = 3 -> SYS_ROOF        -> SYS_ROOF_MATERIAL        -> MATERIAL
    # KIND = 4 -> SYS_GROUNDFLOOR -> SYS_GROUNDFLOOR_MATERIAL -> MATERIAL
    # KIND = 5 -> SYS_MIDDLEFLOOR -> SYS_MIDDLEFLOOR_MATERIAL -> MATERIAL
    # KIND = 6 -> SYS_AIRFLOOR    -> SYS_AIRFLOOR_MATERIAL    -> MATERIAL
    # TODO: translate Chinese names?
    DBI::dbGetQuery(dest,
        "
        WITH SYS_CONST AS (
            SELECT STRUCT_ID, CNAME, 1 AS KIND
            FROM SYS_OUTWALL
            UNION
            SELECT STRUCT_ID, CNAME, 2 AS KIND
            FROM SYS_INWALL
            UNION
            SELECT STRUCT_ID, CNAME, 3 AS KIND
            FROM SYS_ROOF
            UNION
            SELECT STRUCT_ID, CNAME, 4 AS KIND
            FROM SYS_GROUNDFLOOR
            UNION
            SELECT STRUCT_ID, CNAME, 5 AS KIND
            FROM SYS_MIDDLEFLOOR
            UNION
            SELECT STRUCT_ID, CNAME, 6 AS KIND
            FROM SYS_AIRFLOOR
        ),
        SYS_CONST_MATERIAL AS (
            SELECT STRUCT_ID, 1 AS KIND, MATERIAL_ID, LAYER_NO, LENGTH
            FROM SYS_OUTWALL_MATERIAL
            UNION
            SELECT STRUCT_ID, 2 AS KIND, MATERIAL_ID, LAYER_NO, LENGTH
            FROM SYS_INWALL_MATERIAL
            UNION
            SELECT STRUCT_ID, 3 AS KIND, MATERIAL_ID, LAYER_NO, LENGTH
            FROM SYS_ROOF_MATERIAL
            UNION
            SELECT STRUCT_ID, 4 AS KIND, MATERIAL_ID, LAYER_NO, LENGTH
            FROM SYS_GROUNDFLOOR_MATERIAL
            UNION
            SELECT STRUCT_ID, 5 AS KIND, MATERIAL_ID, LAYER_NO, LENGTH
            FROM SYS_MIDDLEFLOOR_MATERIAL
            UNION
            SELECT STRUCT_ID, 6 AS KIND, MATERIAL_ID, LAYER_NO, LENGTH
            FROM SYS_AIRFLOOR_MATERIAL
        ),
        CONST AS (
            SELECT DISTINCT CONSTRUCTION, KIND FROM MAIN_ENCLOSURE WHERE CONSTRUCTION != 0
            UNION
            SELECT DISTINCT D.LONG AS CONSTRUCTION, E.KIND
            FROM MAIN_ENCLOSURE E
            LEFT JOIN DEFAULT_SETTING D
            -- handle default construction
            ON E.CONSTRUCTION = 0 AND E.KIND = D.KIND AND
            D.TABLE_NAME = 'MAIN_ENCLOSURE' AND D.FIELD_NAME = 'CONSTRUCTION' AND D.TYPE = 2
            WHERE D.LONG IS NOT NULL
        )
        SELECT
            C.STRUCT_ID     AS ID,
            C.CNAME         AS NAME,
            CONST.KIND      AS KIND,
            CM.LAYER_NO     AS LAYER_NO,
            CM.LENGTH       AS LENGTH,
            CM.MATERIAL_ID  AS MATERIAL_ID,
            M.CNAME         AS MATERIAL_NAME,
            M.CONDUCTIVITY  AS MATERIAL_CONDUCTIVITY,
            M.DENSITY       AS MATERIAL_DENSITY,
            M.SPECIFIC_HEAT AS MATERIAL_SPECIFIC_HEAT
        FROM CONST
        LEFT JOIN SYS_CONST C
        ON CONST.CONSTRUCTION = C.STRUCT_ID AND CONST.KIND = C.KIND
        LEFT JOIN SYS_CONST_MATERIAL CM
        ON C.STRUCT_ID = CM.STRUCT_ID AND C.KIND = CM.KIND
        LEFT JOIN SYS_MATERIAL M
        ON CM.MATERIAL_ID = M.MATERIAL_ID
        "
    )
}

# Read detailed window construction layers used when aggregate type performance
# is missing or physically invalid.
const__window_layers <- function(dest) {
    # WINDOW -> SYS_WINDOW -> SYS_WINDOW_MATERIAL -> SYS_APP_MATERIAL
    # TODO: handle 'SHADING' in 'WINDOW' table
    DBI::dbGetQuery(dest,
        "
        WITH WIN AS (
            SELECT DISTINCT WINDOW_CONSTRUCTION FROM WINDOW WHERE WINDOW_CONSTRUCTION != 0
            UNION
            SELECT D.LONG AS WINDOW_CONSTRUCTION
            FROM WINDOW W
            LEFT JOIN DEFAULT_SETTING D
            ON W.WINDOW_CONSTRUCTION = 0 AND
               D.TABLE_NAME = 'WINDOW' AND D.FIELD_NAME = 'WINDOW_CONSTRUCTION' AND D.TYPE = 2
            WHERE D.LONG IS NOT NULL
        )
        SELECT
            S.WINDOW_ID           AS ID,
            S.CNAME               AS NAME,
            -- use KIND = -1 to indicate that the construction is a window
            -1                    AS KIND,
            SM.LAYER_NO           AS LAYER_NO,
            SM.LENGTH             AS LENGTH,
            SM.MATERIAL_ID        AS MATERIAL_ID,
            M.CNAME               AS MATERIAL_NAME,
            M.CONDUCTIVITY        AS MATERIAL_CONDUCTIVITY,
            M.DENSITY             AS MATERIAL_DENSITY,
            M.SPECIFIC_HEAT       AS MATERIAL_SPECIFIC_HEAT
        FROM WIN W
        LEFT JOIN SYS_WINDOW S
        ON W.WINDOW_CONSTRUCTION = S.WINDOW_ID
        LEFT JOIN SYS_WINDOW_MATERIAL SM
        ON S.WINDOW_ID = SM.WINDOW_ID
        LEFT JOIN SYS_APP_MATERIAL M
        ON SM.MATERIAL_ID = M.APP_MATERIAL_ID
        "
    )
}

# Normalize the referenced DeST layers and select detailed-window fallbacks
# before EnergyPlus object tables are derived from them.
const__prepare_layers <- function(dest) {
    const <- const__opaque_layers(dest)

    # Resolve the aggregate type data before loading detailed SYS_WINDOW layers.
    # Detailed layers are now retained only for windows that require fallback.
    window_type <- const__window_type_performance(dest)
    window <- const__window_layers(dest)

    # DOOR -> SYS_DOOR -> SYS_MATERIAL
    # TODO: GlazedDoor or Door?
    # NOTE: Doors only have a single layer in DeST. There are 'APP_ID' and
    # 'APP_FLAG' values to indicate if there are glazings in the door. Here we
    # form the data in the same format as normal construction: mark the normal
    # layer as 0 and the glaze layer as 1.
    door <- const__door_layers(dest)

    assert_unique_name(const$NAME[const$LAYER_NO == 0L], "construction")
    assert_unique_name(window$NAME[window$LAYER_NO == 0L], "window")
    assert_unique_name(door$NAME[door$LAYER_NO == 0L], "door")

    data.table::setDT(const)
    data.table::setDT(window)
    data.table::setDT(door)
    # Layer order is semantic, so make it deterministic before building both
    # the source-direction and reversed EnergyPlus construction stacks.
    data.table::setorderv(const, c("ID", "KIND", "LAYER_NO"))
    data.table::setorderv(window, c("ID", "LAYER_NO"))
    data.table::setorderv(door, c("ID", "LAYER_NO"))

    # Valid WINDOW_TYPE_DATA records replace the whole detailed glazing stack,
    # so only load SYS_WINDOW objects still referenced by fallback windows.
    fallback_construction <- unique(
        window_type[TYPE_DATA_VALID == FALSE, DETAILED_CONSTRUCTION_ID]
    )
    fallback_construction <- fallback_construction[
        !is.na(fallback_construction) & fallback_construction != 0L
    ]
    window <- window[ID %in% fallback_construction]

    fallback <- unique(
        window_type[
            TYPE_DATA_VALID == FALSE,
            .(WINDOW_ID, TYPE_ID, FALLBACK_REASON)
        ]
    )
    if (nrow(fallback) > 0L) {
        # Include the affected window and type identifiers so users can repair
        # the source data instead of receiving one blanket optical warning.
        warning(sprintf(
            paste0(
                "Using detailed SYS_WINDOW fallback properties for DeST ",
                "window(s): %s."
            ),
            paste(sprintf(
                "%s (type %s: %s)", fallback$WINDOW_ID, fallback$TYPE_ID,
                fallback$FALLBACK_REASON
            ), collapse = "; ")
        ))
    }

    # check if there are air layer in window constructions
    if (any(is_air <- window$MATERIAL_ID == 0L)) {
        data.table::set(window, which(is_air), "MATERIAL_NAME", "Air")
    }

    # NOTE: Here we append thickness to material names. This is because the
    # same material can be used with different thicknesses. But in EnergyPlus,
    # each material binds to a specific thickness. During conversion, we have to
    # create a new material with each thickness. Appending the thickness to the
    # material name should make them unique, since duplicated names have been
    # handled by 'conv__update_names()'
    if (nrow(const) > 0L) {
        data.table::set(const, NULL, "MATERIAL_NAME",
            with(const, paste0(MATERIAL_NAME, " ", round(LENGTH), "mm"))
        )
    }
    if (nrow(window) > 0L) {
        data.table::set(window, NULL, "MATERIAL_NAME",
            with(window, paste0(MATERIAL_NAME, " ", round(LENGTH), "mm"))
        )
    }
    # Door body and glazing materials can also vary by thickness, so apply the
    # same stable suffix used for opaque and window materials.
    if (nrow(door) > 0L) {
        data.table::set(door, NULL, "MATERIAL_NAME",
            with(door, paste0(MATERIAL_NAME, " ", round(LENGTH), "mm"))
        )
    }

    list(
        const = const, window = window,
        window_type = window_type, door = door
    )
}

# Return the normalized columns shared by opaque, glazing, and gas material
# object tables.
const__material_columns <- function() {
    c(
        "MATERIAL_ID", "LENGTH", "MATERIAL_NAME", "MATERIAL_CONDUCTIVITY",
        "MATERIAL_DENSITY", "MATERIAL_SPECIFIC_HEAT"
    )
}

# Build the normal and reversed EnergyPlus Construction objects for a layered
# DeST construction. The optional kind marks non-opaque construction classes.
const__layered_constructions <- function(layer, by, kind = NULL) {
    if (nrow(layer) == 0L) return(data.table::data.table())

    normal <- layer[,
        by = by,
        list(name = NAME[[1L]], value = list(c(NAME[[1L]], MATERIAL_NAME)))
    ]
    # SIDE1 looks against the stored DeST layer direction and therefore needs
    # an explicit reverse-stack Construction object.
    reverse <- layer[,
        by = by,
        list(
            name = sprintf("%s [Reverse]", NAME[[1L]]),
            value = list(c(
                sprintf("%s [Reverse]", NAME[[1L]]), rev(MATERIAL_NAME)
            ))
        )
    ]
    if (!is.null(kind)) {
        normal[, KIND := kind]
        reverse[, KIND := kind]
    }

    data.table::rbindlist(list(normal, reverse), use.names = TRUE, fill = TRUE)
}

# Select one material record for each material-and-thickness combination after
# applying an optional row mask for glazing, gas, or door-layer roles.
const__material_table <- function(layer, rows = NULL) {
    if (nrow(layer) == 0L) return(data.table::data.table())
    if (!is.null(rows)) layer <- layer[rows]
    unique(
        layer[, .SD, .SDcols = const__material_columns()],
        by = c("MATERIAL_ID", "LENGTH")
    )
}

# Build aggregate simple-glazing constructions and retain the validated source
# rows used later to create WindowMaterial:SimpleGlazingSystem objects.
const__window_type_objects <- function(window_type) {
    glazing <- unique(
        window_type[TYPE_DATA_VALID == TRUE],
        by = "TYPE_ID"
    )
    if (nrow(glazing) == 0L) {
        return(list(
            construction = data.table::data.table(), glazing = glazing
        ))
    }

    assert_unique_name(
        glazing$TYPE_CONSTRUCTION_NAME,
        "window type construction"
    )
    assert_unique_name(
        glazing$SIMPLE_GLAZING_NAME,
        "simple glazing material"
    )
    normal <- glazing[, list(
        ID = TYPE_ID,
        KIND = -3L,
        name = TYPE_CONSTRUCTION_NAME,
        value = Map(c, TYPE_CONSTRUCTION_NAME, SIMPLE_GLAZING_NAME)
    )]
    # Whole-window simple glazing is direction-independent, but reciprocal
    # interzone windows retain an explicit reverse construction name.
    reverse <- glazing[, list(
        ID = TYPE_ID,
        KIND = -3L,
        name = sprintf("%s [Reverse]", TYPE_CONSTRUCTION_NAME),
        value = Map(
            c,
            sprintf("%s [Reverse]", TYPE_CONSTRUCTION_NAME),
            SIMPLE_GLAZING_NAME
        )
    )]

    list(
        construction = data.table::rbindlist(list(normal, reverse)),
        glazing = glazing
    )
}

# Build door constructions and separate their opaque body from the optional
# glazing layer used by WindowMaterial:Glazing.
const__door_objects <- function(door) {
    if (nrow(door) == 0L) {
        empty <- data.table::data.table()
        return(list(construction = empty, material = empty, glazing = empty))
    }

    construction <- door[, by = "ID",
        # KIND = -2 distinguishes doors from other construction classes.
        list(KIND = -2L, name = NAME[[1L]], value = list(c(NAME[[1L]], MATERIAL_NAME)))
    ]

    list(
        construction = construction,
        material = const__material_table(door, door$LAYER_NO == 0L),
        glazing = const__material_table(door, door$LAYER_NO == 1L)
    )
}

# Derive deduplicated EnergyPlus construction and material tables from the
# normalized DeST source layers.
const__object_tables <- function(source) {
    opaque_const <- const__layered_constructions(
        source$const, c("ID", "KIND")
    )
    window_const <- const__layered_constructions(
        source$window, "ID", kind = -1L
    )
    window_type <- const__window_type_objects(source$window_type)
    door <- const__door_objects(source$door)

    dt_const <- unique(
        data.table::rbindlist(list(
            opaque_const, window_const,
            window_type$construction, door$construction
        ), use.names = TRUE, fill = TRUE),
        by = c("ID", "KIND", "name")
    )
    dt_mat <- unique(
        data.table::rbindlist(list(
            const__material_table(source$const), door$material
        ), use.names = TRUE, fill = TRUE),
        by = c("MATERIAL_ID", "LENGTH")
    )
    dt_glaze <- unique(
        data.table::rbindlist(list(
            const__material_table(
                source$window, source$window$MATERIAL_ID != 0L
            ),
            door$glazing
        ), use.names = TRUE, fill = TRUE),
        by = c("MATERIAL_ID", "LENGTH")
    )
    dt_air <- const__material_table(
        source$window, source$window$MATERIAL_ID == 0L
    )
    if (nrow(dt_air) > 0L) dt_air <- unique(dt_air, by = "LENGTH")

    list(
        construction = dt_const,
        material = dt_mat,
        glazing = dt_glaze,
        air = dt_air,
        simple_glazing = window_type$glazing
    )
}

# MAIN_ENCLOSURE$CONSTRUCTION -> Construction -> Material
const__convert <- function(dest, ep) {
    if (!db_has_rows(dest, "MAIN_ENCLOSURE")) return(NULL)

    source <- const__prepare_layers(dest)
    object <- const__object_tables(source)
    out <- const__assemble_objects(
        dest, ep, object$material, object$simple_glazing,
        object$glazing, object$air, object$construction
    )

    # always attach the table to the output in case it is useful later
    attr(out, "table") <- data.table::rbindlist(
        list(
            source$const, source$window,
            object$simple_glazing, source$door
        ),
        fill = TRUE
    )

    out
}

# Assemble the heterogeneous material and construction classes after the
# converter has normalized every source table and resolved its fallbacks.
const__assemble_objects <- function(
    dest, ep, dt_mat, win_type_glazing, dt_glaze, dt_air, dt_const
) {
    eval(as.call(c(
        conv__add, dest, ep,

        # Material
        bquote(
            "Material" := list(
                name                = .(dt_mat$MATERIAL_NAME),
                # NOTE: here we use "MediumSmooth" for roughness"
                roughness           = "MediumSmooth",
                # DeST construction lengths are stored in millimetres, while
                # EnergyPlus Material thickness is expressed in metres.
                thickness           = .(dt_mat$LENGTH / 1000),
                conductivity        = .(dt_mat$MATERIAL_CONDUCTIVITY),
                density             = .(dt_mat$MATERIAL_DENSITY),
                specific_heat       = .(dt_mat$MATERIAL_SPECIFIC_HEAT),
                # use EnergyPlus defaults for the rest
                thermal_absorptance = 0.9,
                solar_absorptance   = 0.7,
                visible_absorptance = 0.7
            )
        ),

        # WINDOW_TYPE_DATA describes whole-window performance rather than
        # individual panes. Emit one equivalent simple glazing system per type.
        lapply(seq_len(nrow(win_type_glazing)), function(index) {
            glazing <- win_type_glazing[index]
            value <- list(
                name = glazing$SIMPLE_GLAZING_NAME,
                u_factor = glazing$K,
                solar_heat_gain_coefficient = glazing$SHGC,
                visible_transmittance = if (!is.na(glazing$LIGHT_TRANS_RATIO)) {
                    glazing$LIGHT_TRANS_RATIO
                }
            )
            bquote("WindowMaterial:SimpleGlazingSystem" := .(value))
        }),

        if (nrow(dt_glaze) > 0L) {
            # Detailed DeST optical fields have no direct EnergyPlus match, so
            # retain the established 3 mm clear-glazing approximation.
            clear3mm <- list(
                Name = "CLEAR 3MM", Conductivity = 0.9, Thickness = 0.003,
                `Optical Data Type` = "SpectralAverage",
                `Solar Transmittance at Normal Incidence` = 0.837,
                `Front Side Solar Reflectance at Normal Incidence` = 0.075,
                `Back Side Solar Reflectance at Normal Incidence` = 0.075,
                `Visible Transmittance at Normal Incidence` = 0.898,
                `Front Side Visible Reflectance at Normal Incidence` = 0.081,
                `Back Side Visible Reflectance at Normal Incidence` = 0.081,
                `Infrared Transmittance at Normal Incidence` = 0,
                `Front Side Infrared Hemispherical Emissivity` = 0.84,
                `Back Side Infrared Hemispherical Emissivity` = 0.84
            )

            message(paste(
                "There are windows in the input DeST model. However, the optical",
                "properties of the glazing in DeST are not one-to-one match with",
                "the glazing in EnergyPlus. Here the optical properties of a 3mm",
                "clear glazing in EnergyPlus dataset 'WindowGlassMaterials.idf'",
                "will be used for all glazing in DeST. Please check the results in",
                "the converted IDF file."
            ))

            glaze              <- clear3mm
            glaze$Name         <- dt_glaze$MATERIAL_NAME
            glaze$Thickness    <- round(dt_glaze$LENGTH / 1000, 4L)
            glaze$Conductivity <- dt_glaze$MATERIAL_CONDUCTIVITY
            bquote("WindowMaterial:Glazing" := .(glaze))
        },

        if (nrow(dt_air) > 0L) {
            bquote("WindowMaterial:Gas" := list(
                name = .(dt_air$MATERIAL_NAME),
                gas_type = "Air",
                thickness = round(.(dt_air$LENGTH) / 1000, 4L)
            ))
        },

        # Construction
        lapply(dt_const$value, function(con) {
            bquote("Construction" := as.list(.(con)))
        })
    )))
}
