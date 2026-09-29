# Build a compact ROOM/ROOM_TYPE_DATA fixture for outdoor-air tests without
# requiring the other room-function fields that this converter does not read.
destep_test_outdoor_air_db <- function(
    fresh_air = c(25, 10),
    room_type = seq_along(fresh_air),
    type_id = seq_along(fresh_air)
) {
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

    DBI::dbWriteTable(dest, "ROOM", data.frame(
        ID = seq_along(room_type),
        NAME = paste("Room", seq_along(room_type)),
        TYPE = room_type
    ))
    DBI::dbWriteTable(dest, "ROOM_TYPE_DATA", data.frame(
        ID = type_id,
        O_MIN_REQUIRE_FRESH_AIR = fresh_air
    ))

    dest
}

test_that("can convert occupant minimum fresh air to DesignSpecification:OutdoorAir", {
    ep <- eplusr::empty_idf(23.1)
    dest <- destep_test_outdoor_air_db()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    outdoor_air <- outdoor_air__convert(dest, ep)
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

test_that("reuses one room-type outdoor-air requirement across rooms", {
    ep <- eplusr::empty_idf(23.1)
    dest <- destep_test_outdoor_air_db(
        fresh_air = 25,
        room_type = c(1L, 1L),
        type_id = 1L
    )
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    outdoor_air <- outdoor_air__convert(dest, ep)

    expect_equal(sum(outdoor_air$object$class_name == "DesignSpecification:OutdoorAir"), 2L)
    expect_equal(attr(outdoor_air, "table")$ROOM_NAME, c("Room 1", "Room 2"))
})

test_that("skips zero or missing occupant outdoor-air requirements", {
    ep <- eplusr::empty_idf(23.1)
    dest <- destep_test_outdoor_air_db(fresh_air = c(0, NA_real_))
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    expect_null(outdoor_air__convert(dest, ep))
})

test_that("stops when ROOM.TYPE outdoor-air references cannot be resolved", {
    ep <- eplusr::empty_idf(23.1)
    dest <- destep_test_outdoor_air_db(
        fresh_air = 25,
        room_type = 999L,
        type_id = 1L
    )
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    expect_error(
        outdoor_air__convert(dest, ep),
        "Cannot resolve ROOM.TYPE outdoor-air reference"
    )
})

test_that("can convert occupant outdoor air from a real DeST model", {
    skip_on_cran()

    ep <- eplusr::empty_idf(23.1)
    src <- ensure_dest_sqlite_file()
    on.exit(DBI::dbDisconnect(src), add = TRUE)

    path_tmp <- tempfile(fileext = ".sql")
    dest <- DBI::dbConnect(RSQLite::SQLite(), path_tmp)
    on.exit({
        DBI::dbDisconnect(dest)
        unlink(path_tmp)
    }, add = TRUE)
    RSQLite::sqliteCopyDatabase(src, dest)
    conv__update_names(dest)

    outdoor_air <- outdoor_air__convert(dest, ep)
    tab <- attr(outdoor_air, "table")

    expect_equal(nrow(tab), 27L)
    expect_equal(
        sort(unique(tab$OUTDOOR_AIR_FLOW_PER_PERSON)),
        c(20, 30) / 3600
    )
    expect_equal(
        sum(outdoor_air$object$class_name == "DesignSpecification:OutdoorAir"),
        27L
    )
})
