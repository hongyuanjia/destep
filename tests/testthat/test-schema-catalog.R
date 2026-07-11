test_that("DeST schema catalog has the expected shape", {
    catalog <- destep_schema_catalog()
    tables <- catalog$tables
    fields <- catalog$fields
    fields_cn <- destep_read_schema_tsv("fields_cn.tsv")
    observations <- catalog$observations

    expect_named(catalog, c("tables", "fields", "observations"))
    expect_s3_class(tables, "data.frame")
    expect_s3_class(fields, "data.frame")
    expect_s3_class(fields_cn, "data.frame")
    expect_s3_class(observations, "data.frame")
    expect_named(tables, c(
        "table", "category", "status", "primary_key", "converter", "used_by",
        "energyplus_classes",
        "future_energyplus_classes", "candidate_energyplus_classes",
        "semantics", "notes"
    ))
    expect_named(fields, c("table", "field", "ordinal", "role", "semantics", "notes"))
    expect_named(fields_cn, c(
        "source_model", "table", "field", "ordinal", "description_cn"
    ))
    expect_named(observations, c("source_model", "table", "rows", "notes"))
    expect_gt(nrow(tables), 0L)
    expect_gt(nrow(fields), 0L)
    expect_gt(nrow(fields_cn), 0L)
    expect_gt(nrow(observations), 0L)
    expect_equal(anyDuplicated(tables$table), 0L)
    expect_equal(anyDuplicated(paste(fields$table, fields$field, sep = "\r")), 0L)
    expect_equal(
        anyDuplicated(paste(
            fields_cn$source_model, fields_cn$table, fields_cn$field, sep = "\r"
        )),
        0L
    )
    expect_equal(
        anyDuplicated(paste(observations$source_model, observations$table, sep = "\r")),
        0L
    )
    expect_true(all(fields$table %in% tables$table))
    expect_true(all(nzchar(tables$semantics)))
    expect_false(any(grepl("[\u4e00-\u9fff]", tables$semantics)))
    expect_true(all(nzchar(fields$semantics)))
    expect_false(any(grepl("[\u4e00-\u9fff]", fields$semantics)))
    expect_true(all(observations$table %in% tables$table))

    field_cn_ordinal <- suppressWarnings(as.integer(fields_cn$ordinal))
    expect_false(anyNA(field_cn_ordinal))
    for (field_group in split(fields_cn, paste(fields_cn$source_model, fields_cn$table))) {
        expect_equal(as.integer(field_group$ordinal), seq_len(nrow(field_group)))
    }

    fixture_fields_cn <- fields_cn[
        fields_cn$source_model == "CoA_Chongqin_2015",
    ]
    catalog_field_key <- paste(fields$table, fields$field, sep = "\r")
    fixture_field_key <- paste(fixture_fields_cn$table, fixture_fields_cn$field, sep = "\r")
    expect_true(all(catalog_field_key %in% fixture_field_key))
    expect_equal(
        fixture_fields_cn$description_cn[
            fixture_fields_cn$table == "ROOM_RELATION" &
                fixture_fields_cn$field == "VENT_TYPE"
        ],
        "通风定义的含义：0:换气次数；1:附加换气次数范围"
    )
    expect_equal(
        fixture_fields_cn$description_cn[
            fixture_fields_cn$table == "ROOM" &
                fixture_fields_cn$field == "TYPE"
        ],
        "房间类型(用于人员负荷的参考) 对应ROOM_TYPE_DATA表"
    )
    expect_equal(
        fixture_fields_cn$description_cn[
            fixture_fields_cn$table == "ROOM" &
                fixture_fields_cn$field == "AREA"
        ],
        "面积（单位：m2）"
    )

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
        "SCHEDULE_YEAR", "WINDOW", "ROOM_GROUP", "ROOM_RELATION",
        "WINDOW_TYPE_DATA", "OCCUPANT_GAINS", "LIGHT_GAINS",
        "EQUIPMENT_GAINS", "GROUND_DATA"
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
