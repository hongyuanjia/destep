test_that("public conversion preserves the envelope independently of multipliers", {
    skip_on_cran()

    source <- ensure_dest_sqlite_file()
    on.exit(DBI::dbDisconnect(source), add = TRUE)
    tables <- DBI::dbListTables(source)
    before <- lapply(tables, DBI::dbReadTable, conn = source)
    source_path <- DBI::dbGetInfo(source)$dbname
    source_hash <- tools::md5sum(source_path)

    # A controlled source copy changes only multiplicities. Representative
    # surfaces, openings and thermal properties must not depend on that weight.
    unit <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(unit), add = TRUE)
    RSQLite::sqliteCopyDatabase(source, unit)
    DBI::dbExecute(unit, "UPDATE STOREY SET MULTIPLE = 1")
    converted <- suppressWarnings(to_idf(
        source,
        23.1,
        options = destep_opts(hvac = "ideal_loads")
    ))
    unweighted <- suppressWarnings(to_idf(
        unit,
        23.1,
        options = destep_opts(hvac = "ideal_loads")
    ))

    expect_true(converted$is_valid(level = "final"))
    expect_true(unweighted$is_valid(level = "final"))
    # Object identifiers are allocation details; compare saved field values
    # ordered by public object name and field index instead.
    for (class in c(
        "BuildingSurface:Detailed",
        "FenestrationSurface:Detailed",
        "Construction",
        "Material",
        "Material:NoMass",
        "SurfaceProperty:ConvectionCoefficients"
    )) {
        expect_equal(
            converted$object_num(class),
            unweighted$object_num(class),
            info = class
        )
        if (converted$object_num(class) == 0L) {
            next
        }
        actual <- converted$to_table(class = class)
        expected <- unweighted$to_table(class = class)
        fields <- setdiff(names(actual), "id")
        actual <- actual[, fields, with = FALSE]
        expected <- expected[, fields, with = FALSE]
        data.table::setorderv(actual, c("name", "index"))
        data.table::setorderv(expected, c("name", "index"))
        expect_equal(actual, expected, info = class)
    }

    pairs <- attr(converted, "conversion")$surface_boundaries
    expect_s3_class(pairs, "data.table")
    expect_gt(nrow(pairs), 0L)
    expect_true(all(pairs$multiplier != pairs$peer_multiplier))
    expect_equal(
        nrow(attr(unweighted, "conversion")$surface_boundaries),
        0L
    )
    expect_identical(DBI::dbListTables(source), tables)
    expect_identical(lapply(tables, DBI::dbReadTable, conn = source), before)
    expect_identical(tools::md5sum(source_path), source_hash)
})

test_that("public conversion identifies missing enclosure references without mutation", {
    skip_on_cran()
    source <- ensure_dest_sqlite_file()
    on.exit(DBI::dbDisconnect(source), add = TRUE)
    source_path <- DBI::dbGetInfo(source)$dbname
    source_hash <- tools::md5sum(source_path)
    enclosure <- DBI::dbGetQuery(
        source,
        "SELECT MIN(ID) AS ID FROM MAIN_ENCLOSURE WHERE KIND = 2"
    )$ID
    expect_length(enclosure, 1L)

    # Each disposable input removes exactly one required source reference.
    # Observe the public diagnostic and every caller table after the failure.
    for (field in c("SIDE1", "SIDE2", "MIDDLE_PLANE")) {
        work <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
        tryCatch(
            {
                RSQLite::sqliteCopyDatabase(source, work)
                DBI::dbExecute(
                    work,
                    sprintf(
                        "UPDATE MAIN_ENCLOSURE SET %s = -987654321 WHERE ID = %d",
                        field,
                        enclosure
                    )
                )
                tables <- DBI::dbListTables(work)
                before <- lapply(tables, DBI::dbReadTable, conn = work)
                expect_error(
                    suppressWarnings(to_idf(
                        work,
                        23.1,
                        options = destep_opts(hvac = "ideal_loads")
                    )),
                    sprintf("MAIN_ENCLOSURE %d.*%s", enclosure, field),
                    class = "destep_invalid_surface_references"
                )
                expect_identical(DBI::dbListTables(work), tables)
                expect_identical(
                    lapply(tables, DBI::dbReadTable, conn = work),
                    before
                )
            },
            finally = DBI::dbDisconnect(work)
        )
    }
    expect_identical(tools::md5sum(source_path), source_hash)
})
