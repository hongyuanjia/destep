test_that("destep_schema_coverage summarizes ad hoc SQLite databases", {
    sqlite <- tempfile(fileext = ".sqlite")
    con <- DBI::dbConnect(RSQLite::SQLite(), sqlite)
    on.exit({
        if (DBI::dbIsValid(con)) {
            DBI::dbDisconnect(con)
        }
    }, add = TRUE)

    DBI::dbExecute(con, "CREATE TABLE BUILDING (BUILDING_ID INTEGER, NAME TEXT)")
    DBI::dbExecute(con, "INSERT INTO BUILDING VALUES (1, 'Demo')")
    DBI::dbExecute(con, "CREATE TABLE CUSTOM_TABLE (ID INTEGER, VALUE TEXT)")
    DBI::dbExecute(con, "INSERT INTO CUSTOM_TABLE VALUES (1, 'x')")

    coverage <- destep_schema_coverage(con)

    expect_named(coverage, c("tables", "fields"))
    expect_named(coverage$tables, c(
        "table", "in_database", "rows", "is_non_empty", "in_catalog",
        "status", "category", "converter", "used_by", "energyplus_classes",
        "is_converted", "notes"
    ))
    expect_named(coverage$fields, c(
        "table", "field", "ordinal", "in_database", "in_catalog",
        "role", "semantics", "notes"
    ))
    expect_true(DBI::dbIsValid(con))

    custom_table <- coverage$tables[coverage$tables$table == "CUSTOM_TABLE", ]
    expect_equal(nrow(custom_table), 1L)
    expect_true(custom_table$in_database)
    expect_false(custom_table$in_catalog)
    expect_equal(custom_table$rows, 1L)
    expect_true(custom_table$is_non_empty)
    expect_false(custom_table$is_converted)

    missing_building_field <- coverage$fields[
        coverage$fields$table == "BUILDING" &
            coverage$fields$field == "CURRENT_STOREY",
    ]
    expect_equal(nrow(missing_building_field), 1L)
    expect_false(missing_building_field$in_database)
    expect_true(missing_building_field$in_catalog)

    new_custom_field <- coverage$fields[
        coverage$fields$table == "CUSTOM_TABLE" &
            coverage$fields$field == "VALUE",
    ]
    expect_equal(nrow(new_custom_field), 1L)
    expect_true(new_custom_field$in_database)
    expect_false(new_custom_field$in_catalog)

    DBI::dbDisconnect(con)
    path_coverage <- destep_schema_coverage(sqlite)
    path_custom_table <- path_coverage$tables[
        path_coverage$tables$table == "CUSTOM_TABLE",
    ]
    expect_equal(path_custom_table$rows, 1L)
})

test_that("destep_schema_coverage reports real fixture coverage", {
    skip_on_cran()

    dest <- ensure_dest_sqlite_file()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    coverage <- destep_schema_coverage(dest)

    window <- coverage$tables[coverage$tables$table == "WINDOW", ]
    expect_equal(nrow(window), 1L)
    expect_equal(window$rows, 45L)
    expect_equal(window$status, "converted")
    expect_true(window$is_converted)

    room_type_data <- coverage$tables[coverage$tables$table == "ROOM_TYPE_DATA", ]
    expect_equal(nrow(room_type_data), 1L)
    expect_true(room_type_data$in_database)
    expect_false(room_type_data$in_catalog)
    expect_true(room_type_data$is_non_empty)

    missing_catalog_fields <- coverage$fields[
        coverage$fields$in_catalog & !coverage$fields$in_database,
    ]
    expect_equal(nrow(missing_catalog_fields), 0L)
})
