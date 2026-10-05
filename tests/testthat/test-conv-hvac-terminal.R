# Minimal independent DBI sources isolate source definition and property errors.
hvac__terminal_fixture <- function(
    capacity = c(0, 10000, 5000),
    roots = c(0L, 0L, 10L)
) {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbWriteTable(
        con,
        "ROOM",
        data.frame(
            ID = seq_along(capacity),
            SET_TERMINAL_MAX = capacity,
            EXT_PROPERTY = roots
        )
    )
    DBI::dbWriteTable(
        con,
        "EXT_PROPERTY",
        data.frame(
            PROPERTY_ID = c(10L, 11L),
            NEXT_PROPERTY = c(11L, 0L),
            NAME = c("ROOM_REHEATER_TYPE", "unrelated"),
            DATA_LONG = c(0L, 4L)
        )
    )
    con
}

test_that("room capacity and explicit type override any central AHU state", {
    con <- hvac__terminal_fixture()
    on.exit(DBI::dbDisconnect(con))
    source <- hvac__room_terminal_source(con, c(3L, 2L, 1L))
    expect_warning(
        hvac__warn_terminal_defaults(source),
        class = "destep_assumed_hvac_terminal_type"
    )
    expect_identical(source$room_id, c(3L, 2L, 1L))
    expect_equal(source$terminal_capacity_w, c(5000, 10000, 0))
    expect_identical(source$terminal_type, c(0L, 1L, 0L))
    expect_identical(source$terminal_has_reheat, c(FALSE, TRUE, FALSE))
    expect_identical(
        source$terminal_type_origin,
        c(
            "source_room_property",
            "converter_default_electric",
            "zero_source_capacity"
        )
    )
    expect_equal(nrow(hvac__room_terminal_source(con, integer())), 0L)
    expect_error(
        hvac__room_terminal_source(con, 10L),
        class = "destep_unresolved_hvac_terminal"
    )
    expect_error(hvac__room_terminal_source(con, c(1L, 1L)))
})

test_that("invalid capacities and referenced property chains do not get defaults", {
    con <- hvac__terminal_fixture()
    on.exit(DBI::dbDisconnect(con))
    for (value in c(NA_real_, Inf, -1)) {
        DBI::dbExecute(
            con,
            "UPDATE ROOM SET SET_TERMINAL_MAX=? WHERE ID=1",
            params = list(value)
        )
        expect_error(
            hvac__room_terminal_source(con, 1L),
            class = "destep_invalid_hvac_terminal_capacity"
        )
    }
    DBI::dbExecute(con, "UPDATE ROOM SET SET_TERMINAL_MAX=0 WHERE ID=1")
    DBI::dbExecute(
        con,
        "UPDATE EXT_PROPERTY SET NEXT_PROPERTY=10 WHERE PROPERTY_ID=11"
    )
    expect_error(
        hvac__room_terminal_source(con, 3L),
        class = "destep_unresolved_hvac_property"
    )
    DBI::dbExecute(
        con,
        "UPDATE EXT_PROPERTY SET NEXT_PROPERTY=999 WHERE PROPERTY_ID=11"
    )
    expect_error(
        hvac__room_terminal_source(con, 3L),
        class = "destep_unresolved_hvac_property"
    )
    DBI::dbExecute(
        con,
        "UPDATE EXT_PROPERTY SET NEXT_PROPERTY=0 WHERE PROPERTY_ID=11"
    )
    DBI::dbExecute(
        con,
        "UPDATE EXT_PROPERTY SET DATA_LONG=3 WHERE PROPERTY_ID=10"
    )
    expect_error(
        hvac__room_terminal_source(con, 3L),
        class = "destep_invalid_hvac_terminal_type"
    )
    DBI::dbExecute(
        con,
        "UPDATE EXT_PROPERTY SET DATA_LONG=NULL WHERE PROPERTY_ID=10"
    )
    expect_error(
        hvac__room_terminal_source(con, 3L),
        class = "destep_unresolved_hvac_property"
    )
})

test_that("explicit water type remains distinct and unknown group ownership stops", {
    con <- hvac__terminal_fixture()
    on.exit(DBI::dbDisconnect(con))
    DBI::dbExecute(
        con,
        "UPDATE EXT_PROPERTY SET DATA_LONG=2 WHERE PROPERTY_ID=10"
    )
    source <- hvac__room_terminal_source(con, 3L)
    expect_identical(source$terminal_type, 2L)
    expect_true(source$terminal_has_reheat)
    DBI::dbExecute(con, "ALTER TABLE ROOM ADD COLUMN OF_ROOM_GROUP INTEGER")
    DBI::dbExecute(con, "UPDATE ROOM SET OF_ROOM_GROUP=100")
    DBI::dbWriteTable(
        con,
        "ROOM_GROUP",
        data.frame(ROOM_GROUP_ID = 100L, EXT_PROPERTY = 10L)
    )
    expect_error(
        hvac__room_terminal_source(con, 3L),
        class = "destep_unresolved_hvac_terminal"
    )
})

# The source system total, room minima and flow ceiling constrain independent
# allocation checks; no reference-model allocation is supplied.
test_that("default outdoor air preserves system total and declared room minima", {
    source <- list(
        system = data.table::data.table(outdoor_air_flow_m3_s = 0.3),
        zones = data.table::data.table(
            room_id = c(20L, 10L),
            maximum_supply_flow_m3_s = c(0.6, 0.4)
        )
    )
    result <- hvac__terminal_outdoor_air(source)
    expect_equal(result$zones$outdoor_air_flow_m3_s, c(0.18, 0.12))
    expect_equal(sum(result$zones$outdoor_air_flow_m3_s), 0.3)
    expect_false("outdoor_air_flow_m3_s" %in% names(source$zones))
    data.table::set(
        source$zones,
        NULL,
        "source_minimum_outdoor_air_flow_m3_s",
        c(0.2, 0)
    )
    result <- hvac__terminal_outdoor_air(source)
    expect_gte(result$zones$outdoor_air_flow_m3_s[[1L]], 0.2)
    expect_equal(sum(result$zones$outdoor_air_flow_m3_s), 0.3)
    expect_identical(
        unique(result$zones$outdoor_air_allocation_origin),
        "source_room_minima_plus_system_remainder"
    )
    expect_error(
        hvac__terminal_outdoor_air(source, c("10" = 0.2, "20" = 0.1)),
        "below the source ROOM minimum",
        class = "destep_invalid_hvac_air_balance"
    )
    explicit <- hvac__terminal_outdoor_air(source, c("10" = 0.1, "20" = 0.2))
    expect_equal(explicit$zones$outdoor_air_flow_m3_s, c(0.2, 0.1))
    expect_identical(
        unique(explicit$zones$outdoor_air_allocation_origin),
        "user_override"
    )
    data.table::set(
        source$zones,
        NULL,
        "source_minimum_outdoor_air_flow_m3_s",
        c(0.2, 0.2)
    )
    expect_error(
        hvac__terminal_outdoor_air(source),
        class = "destep_invalid_hvac_air_balance"
    )
    expect_error(hvac__terminal_outdoor_air(source, c("20" = 0.4, "10" = 0.2)))
    data.table::set(
        source$zones,
        NULL,
        "source_minimum_outdoor_air_flow_m3_s",
        c(NA_real_, 0)
    )
    expect_error(hvac__terminal_outdoor_air(source))
    source$zones <- source$zones[0L]
    expect_error(hvac__terminal_outdoor_air(source))
})

# A design-flow allocation would be 0.18/0.12, exceeding the first VAV
# minimum of 0.1. The source total must close inside both terminal limits.
test_that("VAV default allocation stays inside minimum operating flows", {
    source <- list(
        system = data.table::data.table(outdoor_air_flow_m3_s = 0.3),
        zones = data.table::data.table(
            room_id = c(1L, 2L),
            maximum_supply_flow_m3_s = c(0.6, 0.4),
            minimum_supply_flow_m3_s = c(0.1, 0.2)
        )
    )
    result <- hvac__terminal_outdoor_air(source)
    expect_equal(result$zones$outdoor_air_flow_m3_s, c(0.1, 0.2))
    expect_error(
        hvac__terminal_outdoor_air(source, c("1" = 0.18, "2" = 0.12)),
        class = "destep_invalid_hvac_air_balance"
    )
})
