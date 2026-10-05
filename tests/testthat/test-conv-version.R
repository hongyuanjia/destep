# A small source database distinguishes effective inputs from unused catalogue
# rows when selecting the generation baseline; it does not simulate HVAC.
version_test__source <- function() {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbWriteTable(con, "ROOM", data.frame(TYPE = 1L))
    DBI::dbWriteTable(
        con,
        "ROOM_TYPE_DATA",
        data.frame(
            ID = c(1L, 2L),
            O_MAXNUMBER = c(0, 5),
            O_MINNUMBER = 0,
            O_DAMP_PER_PERSON = 100,
            E_MIN_HUM = 0,
            E_MAX_HUM = c(0, 1)
        )
    )
    con
}

test_that("only active source features raise the reference generation baseline", {
    con <- version_test__source()
    on.exit(DBI::dbDisconnect(con))
    expect_identical(
        as.character(conv__generation_version(con, "23.1")),
        "9.0.1"
    )
    expect_error(
        conv__generation_version(con, "8.6"),
        class = "destep_unsupported_target_version"
    )
    DBI::dbExecute(con, "UPDATE ROOM_TYPE_DATA SET O_MAXNUMBER=2 WHERE ID=1")
    expect_identical(
        as.character(conv__generation_version(con, "23.1")),
        "9.1.0"
    )
    expect_error(
        conv__generation_version(con, "9.0.1"),
        class = "destep_unsupported_moisture_target"
    )
    DBI::dbExecute(
        con,
        "UPDATE ROOM_TYPE_DATA SET O_MAXNUMBER=0, E_MAX_HUM=0.2 WHERE ID=1"
    )
    expect_identical(
        as.character(conv__generation_version(con, "23.1")),
        "9.1.0"
    )
})

# The public eplusr updater must preserve external schedules, provenance and
# comments while adapting schemas; temporary IDF storage must not leak into
# the returned model's backing path or copy the user's CSV into temp storage.
test_that("upward transition preserves file dependencies and conversion metadata", {
    skip_if_not(all(c("9.1.0", "9.6.0") %in% eplusr::avail_eplus()))
    directory <- tempfile("destep-version-test-")
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE))
    path <- file.path(normalizePath(directory), "schedule.csv")
    writeLines(rep("1", 8760), path)
    before <- tools::md5sum(path)
    model <- eplusr::empty_idf("9.1")
    model$add(
        Building = list(name = "Version fixture"),
        GlobalGeometryRules = list(
            starting_vertex_position = "UpperLeftCorner",
            vertex_entry_direction = "Counterclockwise",
            coordinate_system = "Relative"
        ),
        `Schedule:File` = list(
            name = "External hourly input",
            file_name = path,
            column_number = 1,
            rows_to_skip_at_top = 0,
            number_of_hours_of_data = 8760,
            column_separator = "Comma",
            interpolate_to_timestep = "No",
            minutes_per_item = 60
        )
    )
    model$Version$comment("Source provenance comment")
    audit <- list(
        versions = list(generation = "9.1.0", target = "9.6.0"),
        schedules = list(files = path)
    )
    attr(model, "conversion") <- audit
    transitioned <- conv__transition(model, numeric_version("9.6.0"))
    expect_identical(as.character(transitioned$version()), "9.6.0")
    expect_true(transitioned$is_valid())
    expect_null(transitioned$path())
    expect_identical(attr(transitioned, "conversion"), audit)
    expect_identical(
        unname(unlist(transitioned$object("External hourly input")$value(
            "file_name"
        ))),
        path
    )
    expect_identical(tools::md5sum(path), before)
    expect_true(any(grepl(
        "Source provenance comment",
        unlist(transitioned$Version$comment()),
        fixed = TRUE
    )))
    expect_identical(list.files(directory), "schedule.csv")
})
