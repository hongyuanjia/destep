test_that("DeST schema catalog has the expected shape", {
    catalog <- destep_schema_catalog()
    tables <- catalog$tables
    fields <- catalog$fields
    observations <- catalog$observations

    expect_named(catalog, c("tables", "fields", "observations"))
    expect_s3_class(tables, "data.frame")
    expect_s3_class(fields, "data.frame")
    expect_s3_class(observations, "data.frame")
    expect_named(tables, c(
        "table", "category", "status", "primary_key", "converter", "used_by",
        "energyplus_classes",
        "future_energyplus_classes", "candidate_energyplus_classes",
        "semantics", "notes"
    ))
    expect_named(fields, c("table", "field", "ordinal", "role", "semantics", "notes"))
    expect_named(observations, c("source_model", "table", "rows", "notes"))
    expect_gt(nrow(tables), 0L)
    expect_gt(nrow(fields), 0L)
    expect_gt(nrow(observations), 0L)
    expect_equal(anyDuplicated(tables$table), 0L)
    expect_equal(anyDuplicated(paste(fields$table, fields$field, sep = "\r")), 0L)
    expect_equal(
        anyDuplicated(paste(observations$source_model, observations$table, sep = "\r")),
        0L
    )
    expect_true(all(fields$table %in% tables$table))
    expect_true(all(observations$table %in% tables$table))

    required_tables <- c(
        "BUILDING", "ENVIRONMENT", "STOREY", "ROOM", "SURFACE",
        "ROOM_TYPE_DATA", "ROOM_RELATION", "MAIN_ENCLOSURE", "PLANE",
        "GEOMETRY", "LOOP_POINT", "POINT", "SCHEDULE_YEAR", "WINDOW",
        "OCCUPANT_GAINS", "LIGHT_GAINS", "EQUIPMENT_GAINS"
    )
    expect_true(all(required_tables %in% tables$table))

    converted_tables <- tables$table[tables$status == "converted"]
    expect_true(all(c(
        "BUILDING", "ENVIRONMENT", "ROOM", "SURFACE", "MAIN_ENCLOSURE",
        "SCHEDULE_YEAR", "WINDOW", "OCCUPANT_GAINS", "LIGHT_GAINS",
        "EQUIPMENT_GAINS"
    ) %in% converted_tables))

    for (table_name in tables$table) {
        expect_true(grepl("^[A-Z0-9_]+$", table_name))
        expect_gt(sum(fields$table == table_name), 0L)
        expect_equal(
            as.integer(fields$ordinal[fields$table == table_name]),
            seq_len(sum(fields$table == table_name))
        )
    }
})

test_that("DeST schema catalog matches the real fixture schema", {
    skip_on_cran()
    catalog <- destep_schema_catalog()
    tables <- catalog$tables
    fields <- catalog$fields
    observations <- catalog$observations

    dest <- ensure_dest_sqlite_file()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    fixture_name <- "CoA_Chongqin_2015"
    fixture_tables <- DBI::dbListTables(dest)

    for (table_name in tables$table) {
        expect_true(table_name %in% fixture_tables)

        fixture_columns <- DBI::dbListFields(dest, table_name)
        catalog_columns <- fields$field[fields$table == table_name]
        expect_true(all(catalog_columns %in% fixture_columns))

        rows <- observations$rows[
            observations$table == table_name &
                observations$source_model == fixture_name
        ]
        if (length(rows) && nzchar(rows)) {
            count <- DBI::dbGetQuery(
                dest,
                sprintf("SELECT COUNT(*) AS N FROM `%s`", table_name)
            )$N[[1L]]
            expect_equal(count, as.integer(rows))
        }
    }
})
