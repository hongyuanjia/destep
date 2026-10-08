# Append a named hourly source schedule to a disposable complete model. Reuse
# its schema so the test exercises the public converter without internal mocks.
controls_test__schedule <- function(dest, id, name, values, type = 4L) {
    row <- DBI::dbGetQuery(dest, "SELECT * FROM SCHEDULE_YEAR LIMIT 1")
    row$SCHEDULE_ID <- id
    row$NAME <- name
    row$TYPE <- type
    row$DATA <- I(list(destep_test_schedule_blob(values)))
    DBI::dbAppendTable(dest, "SCHEDULE_YEAR", row)
    invisible(id)
}

# Read emitted hourly values through the public IDF table interface, using an
# independent Compact interpreter or the referenced CSV column as appropriate.
controls_test__values <- function(idf, schedule_name) {
    fields <- idf$to_table()
    row <- fields[fields$name == schedule_name, ]
    if (unique(row$class) == "Schedule:Constant") {
        return(rep(as.numeric(row$value[row$field == "Hourly Value"]), 8760L))
    }
    if (unique(row$class) == "Schedule:Compact") {
        return(destep_test_expand_compact(row$value))
    }
    path <- row$value[row$field == "File Name"]
    column <- as.integer(row$value[row$field == "Column Number"])
    read.csv(path, header = FALSE)[[column]]
}

test_that("public temperature diagnostics respect each availability schedule", {
    skip_on_cran()
    dest <- destep_test__unmultiplied_fixture()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    # Isolate temperature controls from the separate ventilation temperature
    # band. Three room types share a crossing pair but have distinct on-hours.
    DBI::dbExecute(dest, "UPDATE ROOM_RELATION SET VENT_TYPE = 0")
    DBI::dbExecute(dest, "UPDATE ROOM_GROUP SET IS_AC_ROOM = 1")
    heating <- replace(rep(18, 8760), c(2L, 3L, 4L), 28)
    cooling <- rep(26, 8760)
    profiles <- list(
        heating,
        cooling,
        replace(rep(0, 8760), 3L, 1),
        replace(rep(0, 8760), 4L, 1),
        rep(0, 8760)
    )
    for (i in seq_along(profiles)) {
        controls_test__schedule(
            dest,
            900000L + i,
            paste("Control test", i),
            profiles[[i]],
            if (i > 2L) 2L else 4L
        )
    }
    types <- DBI::dbGetQuery(
        dest,
        "SELECT DISTINCT TYPE FROM ROOM ORDER BY TYPE LIMIT 2"
    )$TYPE
    expect_length(types, 2L)
    DBI::dbExecute(
        dest,
        paste(
            "UPDATE ROOM_TYPE_DATA SET SET_T_MIN_SCHEDULE = 900001,",
            "SET_T_MAX_SCHEDULE = 900002, AC_SCHEDULE_ID = 900005"
        )
    )
    for (i in seq_along(types)) {
        DBI::dbExecute(
            dest,
            "UPDATE ROOM_TYPE_DATA SET AC_SCHEDULE_ID = ? WHERE ID = ?",
            params = list(900002L + i, types[[i]])
        )
    }
    tables <- DBI::dbListTables(dest)
    before <- lapply(tables, DBI::dbReadTable, conn = dest)
    # File and Compact are both public representations of the same source
    # values; neither may repair the inactive or active inverted setpoints.
    for (format in c("compact", "file")) {
        seen <- NULL
        idf <- withCallingHandlers(
            to_idf(
                dest,
                23.1,
                options = destep_opts(
                    hvac = "ideal_loads",
                    schedule_format = format,
                    schedule_directory = if (format == "file") {
                        withr::local_tempdir()
                    } else {
                        NULL
                    }
                )
            ),
            warning = function(w) {
                if (inherits(w, "destep_thermostat_conflict")) {
                    seen <<- w
                }
                invokeRestart("muffleWarning")
            }
        )
        expect_s3_class(seen, "destep_thermostat_conflict")
        conflicts <- seen$conflicts
        expect_equal(nrow(conflicts), 2L)
        expect_equal(conflicts$AC_SCHEDULE_ID, c(900003L, 900004L))
        expect_equal(conflicts$HOURS, c(1L, 1L))
        expect_equal(conflicts$FIRST_HOUR_INDEX, c(2L, 3L))
        for (i in seq_along(types)) {
            expected <- DBI::dbGetQuery(
                dest,
                "SELECT ID FROM ROOM WHERE TYPE = ? ORDER BY ID",
                params = list(types[[i]])
            )$ID
            expect_equal(conflicts$ROOM_IDS[[i]], expected)
        }
        expect_identical(
            attr(idf, "conversion")$schedules$temperature_conflicts,
            conflicts
        )
        expect_true(idf$is_valid(level = "final"))
        for (i in seq_along(profiles)) {
            expect_equal(
                controls_test__values(idf, paste("Control test", i)),
                profiles[[i]]
            )
        }
        expect_identical(lapply(tables, DBI::dbReadTable, conn = dest), before)
    }
    # An unresolved availability ID remains an owning-control error, rather
    # than being interpreted as a permanently disabled schedule.
    DBI::dbExecute(dest, "UPDATE ROOM_TYPE_DATA SET AC_SCHEDULE_ID=-987654321")
    invalid <- lapply(tables, DBI::dbReadTable, conn = dest)
    expect_error(
        suppressWarnings(to_idf(
            dest,
            23.1,
            options = destep_opts(hvac = "ideal_loads")
        )),
        "ideal-loads schedule.*AC_SCHEDULE_ID=-987654321"
    )
    expect_identical(lapply(tables, DBI::dbReadTable, conn = dest), invalid)
})

test_that("public conversion preserves gain totals and reports omitted shares", {
    skip_on_cran()
    dest <- destep_test__unmultiplied_fixture()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    controls_test__schedule(
        dest,
        900010L,
        "Gain test profile",
        rep(c(0, 0.5, 1), 2920L),
        1L
    )
    mode <- DBI::dbGetQuery(dest, "SELECT * FROM DIST_MODE LIMIT 1")
    mode$DIST_MODE_ID <- 900020L
    mode$DIST_AIR <- 0.5
    mode$DIST_AROUND <- 0.25
    mode$DIST_FLOOR <- 0.05
    mode$DIST_ROOF <- 0.2
    DBI::dbAppendTable(dest, "DIST_MODE", mode)
    DBI::dbExecute(
        dest,
        paste(
            "UPDATE ROOM_TYPE_DATA SET",
            "O_MINNUMBER=1, O_MAXNUMBER=3, O_PER_AREA=0, O_HEAT_PER_PERSON=60,",
            "O_DAMP_PER_PERSON=0, O_SCHEDULE=900010, O_DIST_MODE=900020,",
            "L_MINPOWER=10, L_MAXPOWER=30, L_PER_AREA=0, L_HEAT_RATE=0.9,",
            "L_SCHEDULE=900010, L_DIST_MODE=900020,",
            "E_MINPOWER=20, E_MAXPOWER=60, E_PER_AREA=0,",
            "E_SCHEDULE=900010, E_DIST_MODE=900020"
        )
    )
    tables <- DBI::dbListTables(dest)
    before <- lapply(tables, DBI::dbReadTable, conn = dest)
    seen <- NULL
    idf <- withCallingHandlers(
        to_idf(dest, 23.1, options = destep_opts(hvac = "ideal_loads")),
        warning = function(w) {
            if (inherits(w, "destep_unsupported_gain_distribution")) {
                seen <<- w
            }
            invokeRestart("muffleWarning")
        }
    )
    expect_s3_class(seen, "destep_unsupported_gain_distribution")
    expect_equal(seen$distributions$GAIN_TYPE, c("E", "L", "O"))
    expect_equal(seen$distributions$DIST_MODE_ID, rep(900020L, 3L))
    expect_equal(seen$distributions$DIST_FLOOR, rep(0.05, 3L))
    tab <- idf$to_table()
    zone <- tab$value[tab$class == "Zone" & tab$field == "Name"][[1L]]
    # Independent worked example: at fractions 0, 0.5 and 1, the nominal
    # sensible gains are 60/120/180 W, 9/18/27 W and 20/40/60 W.
    totals <- matrix(0, nrow = 3L, ncol = 3L)
    classes <- c("People", "Lights", "ElectricEquipment", "OtherEquipment")
    for (object_class in classes) {
        target <- tab[tab$class == object_class, ]
        names <- unique(target$name[
            grepl("^Zone or ZoneList", target$field) & target$value == zone
        ])
        if (object_class == "OtherEquipment") {
            names <- names[grepl("Heat Ratio Correction$", names)]
        }
        for (object_name in names) {
            fields <- target[target$name == object_name, ]
            schedule_field <- if (object_class == "People") {
                "Number of People Schedule Name"
            } else {
                "Schedule Name"
            }
            schedule <- fields$value[fields$field == schedule_field]
            level_field <- switch(
                object_class,
                People = "Number of People",
                Lights = "Lighting Level",
                "Design Level"
            )
            watts <- as.numeric(fields$value[fields$field == level_field])
            if (object_class == "People") {
                activity <- fields$value[
                    fields$field == "Activity Level Schedule Name"
                ]
                watts <- watts * controls_test__values(idf, activity)[1:3]
            }
            column <- match(
                object_class,
                c("People", "Lights", "ElectricEquipment")
            )
            if (object_class == "OtherEquipment") {
                column <- 2L
            }
            totals[, column] <- totals[, column] +
                watts * controls_test__values(idf, schedule)[1:3]
            expect_equal(
                as.numeric(fields$value[fields$field == "Fraction Radiant"]),
                0.5
            )
        }
    }
    expect_equal(totals[, 1L], c(60, 120, 180))
    expect_equal(totals[, 2L], c(9, 18, 27), tolerance = 1e-6)
    expect_equal(totals[, 3L], c(20, 40, 60))
    expect_true(idf$is_valid(level = "final"))
    expect_identical(lapply(tables, DBI::dbReadTable, conn = dest), before)
    # Dangling active references must identify the source rather than silently
    # dropping the equipment gain or inserting an assumed distribution.
    DBI::dbExecute(dest, "UPDATE ROOM_TYPE_DATA SET E_DIST_MODE = -987654321")
    expect_error(
        suppressWarnings(to_idf(
            dest,
            23.1,
            options = destep_opts(hvac = "ideal_loads")
        )),
        "equipment reference.*DIST_MODE=-987654321"
    )
})

test_that("public ventilation respects room flags independently of availability", {
    skip_on_cran()
    dest <- destep_test__unmultiplied_fixture()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    controls_test__schedule(dest, 900030L, "AC test off", rep(0, 8760), 2L)
    DBI::dbExecute(dest, "UPDATE ROOM_TYPE_DATA SET AC_SCHEDULE_ID=900030")
    idf <- suppressWarnings(to_idf(
        dest,
        23.1,
        options = destep_opts(hvac = "ideal_loads")
    ))
    # This existing fixture has 28 outdoor minima and 23 AC-enabled ranges.
    # All availability values are zero; the 23 ranges must still be present.
    expect_equal(idf$object_num("ZoneVentilation:DesignFlowRate"), 51L)
    expect_equal(controls_test__values(idf, "通风全0.5"), rep(0.5, 8760))
    expect_equal(controls_test__values(idf, "AC test off"), rep(0, 8760))
    DBI::dbExecute(dest, "UPDATE ROOM_GROUP SET IS_AC_ROOM=0")
    non_ac <- suppressWarnings(to_idf(
        dest,
        23.1,
        options = destep_opts(hvac = "ideal_loads")
    ))
    expect_equal(non_ac$object_num("ZoneVentilation:DesignFlowRate"), 28L)
    expect_equal(controls_test__values(non_ac, "通风全0.5"), rep(0.5, 8760))
    tab <- non_ac$to_table(class = "ZoneVentilation:DesignFlowRate")
    expect_true(all(
        as.numeric(tab$value[tab$field == "Air Changes per Hour"]) == 1
    ))
    expect_false(any(grepl("Documented Range Supplement", tab$name)))
    expect_true(non_ac$is_valid(level = "final"))
    # Non-AC minimum ventilation does not consume the maximum reference or
    # the outside-temperature band, even when those unused controls conflict.
    controls_test__schedule(dest, 900031L, "Unused heating", rep(28, 8760))
    controls_test__schedule(dest, 900032L, "Unused cooling", rep(26, 8760))
    DBI::dbExecute(dest, "UPDATE ROOM_RELATION SET VENT_SET_MAX=-987654321")
    DBI::dbExecute(
        dest,
        paste(
            "UPDATE ROOM_TYPE_DATA SET SET_T_MIN_SCHEDULE=900031,",
            "SET_T_MAX_SCHEDULE=900032"
        )
    )
    minimum_only <- suppressWarnings(to_idf(
        dest,
        23.1,
        options = destep_opts(hvac = "ideal_loads")
    ))
    expect_equal(minimum_only$object_num("ZoneVentilation:DesignFlowRate"), 28L)
    expect_true(minimum_only$is_valid(level = "final"))
    DBI::dbExecute(dest, "UPDATE ROOM SET OF_ROOM_GROUP=-987654321")
    expect_error(
        suppressWarnings(to_idf(
            dest,
            23.1,
            options = destep_opts(hvac = "ideal_loads")
        )),
        "Cannot resolve documented ventilation-range inputs.*ROOM_RELATION ID"
    )
})
