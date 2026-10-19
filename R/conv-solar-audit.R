# Retain saved solar-mode bindings and weights without injecting extra heat or
# normalizing incomplete budgets. Missing references remain visible even though
# the target deliberately uses its own solar-distribution algorithm.
solar__source_audit <- function(dest) {
    bindings <- data.table::data.table(
        WINDOW_ID = integer(),
        MODE_ID = integer(),
        DIST_AIR = double(),
        DIST_ROOF = double(),
        DIST_FLOOR = double(),
        DIST_AROUND = double(),
        SURFACE_WEIGHT_SUM = double(),
        SOURCE_STATUS = character()
    )
    receivers <- data.table::data.table(
        GROUP_ID = integer(),
        ROOM_ID = integer(),
        SURFACE_ID = integer(),
        RAW_AREA = double(),
        FAMILY = character(),
        SCOPE = character()
    )
    if (!db_has_rows(dest, "WINDOW")) {
        return(list(bindings = bindings, receivers = receivers))
    }
    has_binding <- db_has_fields(dest, "WINDOW", c("ID", "SUN_TRANS_DIST_MODE"))
    source <- DBI::dbGetQuery(
        dest,
        if (has_binding) {
            "SELECT ID AS WINDOW_ID, SUN_TRANS_DIST_MODE AS MODE_ID FROM WINDOW ORDER BY ID"
        } else {
            "SELECT ID AS WINDOW_ID, NULL AS MODE_ID FROM WINDOW ORDER BY ID"
        }
    )
    bindings <- data.table::as.data.table(source)
    fields <- c("DIST_AIR", "DIST_ROOF", "DIST_FLOOR", "DIST_AROUND")
    for (name in fields) {
        data.table::set(bindings, NULL, name, rep(NA_real_, nrow(bindings)))
    }
    data.table::set(
        bindings,
        NULL,
        "SOURCE_STATUS",
        if (has_binding) "unresolved_mode" else "missing_binding_field"
    )
    if (
        DBI::dbExistsTable(dest, "DIST_MODE") &&
            db_has_fields(dest, "DIST_MODE", c("DIST_MODE_ID", fields))
    ) {
        modes <- DBI::dbGetQuery(
            dest,
            "SELECT DIST_MODE_ID, DIST_AIR, DIST_ROOF, DIST_FLOOR, DIST_AROUND FROM DIST_MODE"
        )
        modes <- modes[
            !is.na(modes$DIST_MODE_ID) &
                modes$DIST_MODE_ID %in% bindings$MODE_ID,
            ,
            drop = FALSE
        ]
        if (anyDuplicated(modes$DIST_MODE_ID)) {
            abort(
                "Ambiguous DIST_MODE_ID in window solar input.",
                class = "destep_invalid_solar_distribution"
            )
        }
        index <- match(bindings$MODE_ID, modes$DIST_MODE_ID)
        for (name in fields) {
            data.table::set(
                bindings,
                NULL,
                name,
                as.double(modes[[name]][index])
            )
        }
        data.table::set(
            bindings,
            which(!is.na(index)),
            "SOURCE_STATUS",
            "saved_mode_retained_for_audit"
        )
        weights <- as.matrix(bindings[, .SD, .SDcols = fields])
        invalid <- which(
            !is.na(index) & rowSums(!is.finite(weights) | weights < 0) > 0
        )
        data.table::set(
            bindings,
            invalid,
            "SOURCE_STATUS",
            "invalid_source_weights"
        )
    }
    data.table::set(
        bindings,
        NULL,
        "SURFACE_WEIGHT_SUM",
        bindings$DIST_ROOF + bindings$DIST_FLOOR + bindings$DIST_AROUND
    )
    # Raw saved receiver metadata is not a target flux matrix. Its area/ownership
    # is useful for auditing; multi-room group dynamics remain unverified.
    required <- list(
        SURFACE = c("SURFACE_ID", "OF_ROOM", "TYPE", "AREA", "AZIMUTH"),
        ROOM = c("ID", "OF_ROOM_GROUP"),
        MAIN_ENCLOSURE = c("SIDE1", "SIDE2"),
        WINDOW = c("SIDE1", "SIDE2")
    )
    complete <- all(vapply(
        names(required),
        function(table) {
            DBI::dbExistsTable(dest, table) &&
                db_has_fields(dest, table, required[[table]])
        },
        logical(1L)
    ))
    if (complete) {
        receivers <- data.table::as.data.table(DBI::dbGetQuery(
            dest,
            "WITH COUNTS AS (SELECT OF_ROOM_GROUP, COUNT(*) AS ROOM_COUNT FROM ROOM GROUP BY OF_ROOM_GROUP)
            SELECT DISTINCT
            R.OF_ROOM_GROUP AS GROUP_ID, R.ID AS ROOM_ID, S.SURFACE_ID, S.AREA AS RAW_AREA, S.AZIMUTH,
            C.ROOM_COUNT
            FROM SURFACE S JOIN ROOM R ON S.OF_ROOM=R.ID
            JOIN COUNTS C ON C.OF_ROOM_GROUP=R.OF_ROOM_GROUP
            WHERE S.TYPE IN (0,4,5) AND (S.SURFACE_ID IN (SELECT SIDE1 FROM MAIN_ENCLOSURE UNION SELECT SIDE2 FROM MAIN_ENCLOSURE)
                OR S.SURFACE_ID IN (SELECT SIDE1 FROM WINDOW UNION SELECT SIDE2 FROM WINDOW))
            ORDER BY R.OF_ROOM_GROUP, R.ID, S.SURFACE_ID"
        ))
        data.table::set(
            receivers,
            NULL,
            "FAMILY",
            data.table::fcase(
                receivers$AZIMUTH == 999  , "floor" ,
                receivers$AZIMUTH == -999 , "roof"  ,
                default = "around"
            )
        )
        data.table::set(
            receivers,
            NULL,
            "SCOPE",
            data.table::fifelse(
                receivers$ROOM_COUNT == 1,
                "single_room_group_saved_receivers",
                "shared_room_group_unverified"
            )
        )
    }
    list(
        bindings = bindings,
        receivers = receivers,
        target = "EnergyPlus building-level solar algorithm",
        limits = c(
            "Saved weights and raw receiver metadata are retained, not applied as target heat sources.",
            "Weights are not normalized to one; a non-unit sum is not evidence of whole-model energy loss.",
            "Native absorption node placement, glass capacity and shared-group dynamics are not reproduced."
        )
    )
}

# Keep solar approximation identity readable after the R attribute is lost on
# IDF save/reload. Source weights are labelled as weights, never percentages.
solar__comments <- function(audit) {
    if (!nrow(audit$bindings)) {
        return(character())
    }
    c(
        if (isTRUE(audit$prepared)) {
            "destep N2 prepared sources: raw DeST weights applied to EnergyPlus own transmitted solar; preserve the weather-bound source schedule dependencies."
        } else {
            "destep solar approximation: target uses EnergyPlus solar distribution; saved DeST weights are audit-only."
        },
        vapply(
            seq_len(nrow(audit$bindings)),
            function(i) {
                row <- audit$bindings[i]
                sprintf(
                    "WINDOW.ID=%s mode=%s status=%s; saved air/roof/floor/around weights=%s/%s/%s/%s; surface sum=%s.",
                    row$WINDOW_ID,
                    row$MODE_ID,
                    row$SOURCE_STATUS,
                    row$DIST_AIR,
                    row$DIST_ROOF,
                    row$DIST_FLOOR,
                    row$DIST_AROUND,
                    row$SURFACE_WEIGHT_SUM
                )
            },
            character(1L)
        ),
        audit$limits
    )
}
