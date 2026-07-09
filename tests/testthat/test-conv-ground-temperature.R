# Build a compact GROUND_DATA fixture whose monthly means are obvious.  The
# converter requires a full non-leap 8760-hour year, so tests vary the data
# content rather than shortening the table.
destep_test_ground_temperature_rows <- function(id, monthly) {
    month_hours <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L) * 24L
    data.frame(
        ID = rep(id, sum(month_hours)),
        HOUR = 0:8759,
        T = rep(monthly, month_hours)
    )
}

# Create an in-memory database with one or more complete GROUND_DATA series.
destep_test_ground_temperature_db <- function(ids = 2L, monthly = list(1:12)) {
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    rows <- do.call(rbind, Map(
        destep_test_ground_temperature_rows,
        ids,
        monthly
    ))
    DBI::dbWriteTable(dest, "GROUND_DATA", rows)
    dest
}

# Extract the generated monthly values from a converter output in IDD order.
destep_test_ground_temperature_values <- function(out) {
    value <- out$value
    as.numeric(value$value_num[
        value$class_name == "Site:GroundTemperature:BuildingSurface" &
            grepl("Ground Temperature", value$field_name, fixed = TRUE)
    ])
}

test_that("skips missing or empty ground data", {
    ep <- ensure_empty_idf()
    missing <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(missing), add = TRUE)

    expect_null(destep_conv_ground_temperature(missing, ep))

    empty <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(empty), add = TRUE)
    DBI::dbWriteTable(empty, "GROUND_DATA", data.frame(
        ID = integer(),
        HOUR = integer(),
        T = numeric()
    ))

    expect_null(destep_conv_ground_temperature(empty, ep))
})

test_that("can convert hourly ground data to monthly BuildingSurface temperatures", {
    ep <- ensure_empty_idf()
    dest <- destep_test_ground_temperature_db(monthly = list(1:12))
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    ground <- destep_conv_ground_temperature(dest, ep)

    expect_equal(unique(ground$object$class_name), "Site:GroundTemperature:BuildingSurface")
    expect_equal(destep_test_ground_temperature_values(ground), as.numeric(1:12))
    expect_equal(attr(ground, "table")$GROUND_TEMPERATURE, as.numeric(1:12))
})

test_that("uses SYS_CITY.GROUND_ID when multiple ground data IDs exist", {
    ep <- ensure_empty_idf()
    dest <- destep_test_ground_temperature_db(
        ids = c(1L, 2L),
        monthly = list(1:12, 101:112)
    )
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    DBI::dbWriteTable(dest, "ENVIRONMENT", data.frame(CITY_ID = 10L))
    DBI::dbWriteTable(dest, "SYS_CITY", data.frame(
        CITY_ID = 10L,
        GROUND_ID = 2L
    ))

    ground <- destep_conv_ground_temperature(dest, ep)

    expect_equal(destep_test_ground_temperature_values(ground), as.numeric(101:112))
    expect_equal(unique(attr(ground, "table")$GROUND_DATA_ID), 2)
})

test_that("falls back to the unique GROUND_DATA ID", {
    ep <- ensure_empty_idf()
    dest <- destep_test_ground_temperature_db(ids = 8L, monthly = list(11:22))
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    ground <- destep_conv_ground_temperature(dest, ep)

    expect_equal(destep_test_ground_temperature_values(ground), as.numeric(11:22))
    expect_equal(unique(attr(ground, "table")$GROUND_DATA_ID), 8)
})

test_that("stops when multiple ground data IDs cannot be selected", {
    ep <- ensure_empty_idf()
    dest <- destep_test_ground_temperature_db(
        ids = c(1L, 2L),
        monthly = list(1:12, 101:112)
    )
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    expect_error(
        destep_conv_ground_temperature(dest, ep),
        "Cannot choose GROUND_DATA ID"
    )
})

test_that("stops on invalid hourly ground data", {
    ep <- ensure_empty_idf()

    missing_hour <- destep_test_ground_temperature_db()
    on.exit(DBI::dbDisconnect(missing_hour), add = TRUE)
    DBI::dbExecute(missing_hour, "DELETE FROM GROUND_DATA WHERE HOUR = 8759")
    expect_error(
        destep_conv_ground_temperature(missing_hour, ep),
        "missing HOUR"
    )

    duplicate_hour <- destep_test_ground_temperature_db()
    on.exit(DBI::dbDisconnect(duplicate_hour), add = TRUE)
    DBI::dbExecute(duplicate_hour, "
        INSERT INTO GROUND_DATA (ID, HOUR, T)
        VALUES (2, 0, 99)
    ")
    expect_error(
        destep_conv_ground_temperature(duplicate_hour, ep),
        "duplicate HOUR"
    )

    missing_temperature <- destep_test_ground_temperature_db()
    on.exit(DBI::dbDisconnect(missing_temperature), add = TRUE)
    DBI::dbExecute(missing_temperature, "
        UPDATE GROUND_DATA
        SET T = NULL
        WHERE HOUR = 0
    ")
    expect_error(
        destep_conv_ground_temperature(missing_temperature, ep),
        "T contains missing values"
    )
})

test_that("can convert ground temperatures from a real DeST model", {
    skip_on_cran()

    ep <- ensure_empty_idf()
    src <- ensure_dest_sqlite_file()
    on.exit(DBI::dbDisconnect(src), add = TRUE)

    ground <- destep_conv_ground_temperature(src, ep)
    table <- attr(ground, "table")
    raw <- DBI::dbGetQuery(src, "
        SELECT HOUR, T
        FROM GROUND_DATA
        ORDER BY HOUR
    ")
    month_hours <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L) * 24L
    expected <- as.numeric(tapply(raw$T, rep(seq_along(month_hours), month_hours), mean))

    expect_equal(sum(ground$object$class_name == "Site:GroundTemperature:BuildingSurface"), 1L)
    expect_equal(table$GROUND_TEMPERATURE, expected)
    expect_equal(destep_test_ground_temperature_values(ground), expected)
})

test_that("to_eplus() includes valid ground temperatures", {
    skip_on_cran()

    src <- ensure_dest_sqlite_file()
    on.exit(DBI::dbDisconnect(src), add = TRUE)

    idf <- to_eplus(src, 23.1)
    ground <- idf$to_table(
        class = "Site:GroundTemperature:BuildingSurface",
        all = TRUE
    )

    expect_equal(
        length(ground$value[grepl("Ground Temperature", ground$field, fixed = TRUE)]),
        12L
    )
    expect_true(idf$is_valid())
})
