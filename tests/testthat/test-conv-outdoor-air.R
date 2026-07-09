# Build a compact ROOM/OCCUPANT_GAINS fixture for outdoor-air tests without
# requiring schedule or internal-gain fields that this converter does not read.
destep_test_outdoor_air_db <- function(fresh_air = c(25, 10), room_id = c(1L, 2L)) {
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

    DBI::dbWriteTable(dest, "ROOM", data.frame(
        ID = c(1L, 2L),
        NAME = c("Room 1", "Room 2")
    ))
    DBI::dbWriteTable(dest, "OCCUPANT_GAINS", data.frame(
        GAIN_ID = seq_along(fresh_air),
        OF_ROOM = room_id,
        MIN_REQUIRE_FRESH_AIR = fresh_air
    ))

    dest
}

test_that("can convert occupant minimum fresh air to DesignSpecification:OutdoorAir", {
    ep <- ensure_empty_idf()
    dest <- destep_test_outdoor_air_db()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    outdoor_air <- destep_conv_design_specification_outdoor_air(dest, ep)
    tab <- attr(outdoor_air, "table")
    value <- outdoor_air$value

    expect_equal(sum(outdoor_air$object$class_name == "DesignSpecification:OutdoorAir"), 2L)
    expect_equal(tab$ROOM_NAME, c("Room 1", "Room 2"))
    expect_equal(tab$OUTDOOR_AIR_FLOW_PER_PERSON, c(25, 10) / 3600)
    expect_equal(
        value$value_chr[
            value$class_name == "DesignSpecification:OutdoorAir" &
                value$field_name == "Outdoor Air Method"
        ],
        c("Flow/Person", "Flow/Person")
    )
    expect_equal(
        value$value_num[
            value$class_name == "DesignSpecification:OutdoorAir" &
                value$field_name == "Outdoor Air Flow per Person"
        ],
        c(25, 10) / 3600
    )
})

test_that("deduplicates matching occupant outdoor-air requirements by room", {
    ep <- ensure_empty_idf()
    dest <- destep_test_outdoor_air_db(
        fresh_air = c(25, 25),
        room_id = c(1L, 1L)
    )
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    outdoor_air <- destep_conv_design_specification_outdoor_air(dest, ep)

    expect_equal(sum(outdoor_air$object$class_name == "DesignSpecification:OutdoorAir"), 1L)
    expect_equal(attr(outdoor_air, "table")$ROOM_NAME, "Room 1")
})

test_that("stops on conflicting occupant outdoor-air requirements by room", {
    ep <- ensure_empty_idf()
    dest <- destep_test_outdoor_air_db(
        fresh_air = c(25, 30),
        room_id = c(1L, 1L)
    )
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    expect_error(
        destep_conv_design_specification_outdoor_air(dest, ep),
        "Conflicting OCCUPANT_GAINS.MIN_REQUIRE_FRESH_AIR"
    )
})

test_that("skips zero or missing occupant outdoor-air requirements", {
    ep <- ensure_empty_idf()
    dest <- destep_test_outdoor_air_db(fresh_air = c(0, NA_real_))
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    expect_null(destep_conv_design_specification_outdoor_air(dest, ep))
})

test_that("stops when occupant outdoor-air room references cannot be resolved", {
    ep <- ensure_empty_idf()
    dest <- destep_test_outdoor_air_db(fresh_air = 25, room_id = 999L)
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    expect_error(
        destep_conv_design_specification_outdoor_air(dest, ep),
        "Cannot resolve OCCUPANT_GAINS outdoor-air room reference"
    )
})

test_that("can convert occupant outdoor air from a real DeST model", {
    skip_on_cran()

    ep <- ensure_empty_idf()
    src <- ensure_dest_sqlite_file()
    on.exit(DBI::dbDisconnect(src), add = TRUE)

    path_tmp <- tempfile(fileext = ".sql")
    dest <- DBI::dbConnect(RSQLite::SQLite(), path_tmp)
    on.exit({
        DBI::dbDisconnect(dest)
        unlink(path_tmp)
    }, add = TRUE)
    RSQLite::sqliteCopyDatabase(src, dest)
    destep_update_name(dest)

    outdoor_air <- destep_conv_design_specification_outdoor_air(dest, ep)
    tab <- attr(outdoor_air, "table")

    expect_equal(nrow(tab), 36L)
    expect_equal(
        unique(tab$OUTDOOR_AIR_FLOW_PER_PERSON),
        25 / 3600
    )
    expect_equal(
        sum(outdoor_air$object$class_name == "DesignSpecification:OutdoorAir"),
        36L
    )
})
