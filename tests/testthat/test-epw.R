# Build the site row used by synthetic EPW tests. PROPERTY encodes UTC+8.
epw_test__environment <- function(latitude = 0, longitude = 120) {
    data.frame(
        CITY_ID = 10L,
        CITY_NAME = "Test City",
        PROVINCE = "Test Province",
        COUNTRY = "Test Country",
        LATITUDE = latitude,
        LONGITUDE = longitude,
        ELEVATION = 100,
        PROPERTY = 12000L
    )
}

# Create a complete non-leap climate series with a hand-checkable equinox noon
# radiation row and otherwise simple constant weather.
epw_test__climate <- function(id = 1L, offset = 0) {
    hour <- 0:8759
    local_hour <- hour %% 24L
    daylight <- local_hour >= 10L & local_hour <= 14L
    data.frame(
        ID = id,
        HOUR = hour,
        DRY_BULB_T = 20 + offset,
        DAMP = 7.5,
        HORI_TOTAL_RAD = ifelse(daylight, 400 + offset, 0),
        HORI_SCATTER_RAD = ifelse(daylight, 100, 0),
        T_GROUND = 15,
        T_SKY = 273.15,
        WS = 2,
        WD = rep(0:16, length.out = length(hour)),
        B = 101325
    )
}

# Assemble an in-memory DeST weather subset for public API tests.
epw_test__database <- function(ids = 1L, offsets = 0, latitude = 0) {
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    rows <- do.call(rbind, Map(epw_test__climate, ids, offsets))
    DBI::dbWriteTable(dest, "ENVIRONMENT", epw_test__environment(latitude))
    DBI::dbWriteTable(dest, "CLIMATE_DATA", rows)
    dest
}

# Independently compute humidity expectations without calling production EPW
# humidity helpers.
epw_test__humidity <- function(dry_bulb, damp, pressure) {
    humidity_ratio <- damp / 1000
    vapor_pressure <- pressure * humidity_ratio / (0.621945 + humidity_ratio)
    saturation_pressure <- 611.2 * exp(
        17.67 * dry_bulb / (dry_bulb + 243.5)
    )
    gamma <- log(vapor_pressure / 611.2)
    list(
        relative_humidity = 100 * vapor_pressure / saturation_pressure,
        dew_point_temperature = 243.5 * gamma / (17.67 - gamma)
    )
}

test_that("to_epw() maps a complete CLIMATE_DATA year", {
    dest <- epw_test__database()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    epw <- to_epw(dest)
    data <- epw$data()
    location <- epw$location()
    period <- epw$period()
    humidity <- epw_test__humidity(20, 7.5, 101325)
    audit <- attr(epw, "destep_audit")

    expect_s3_class(epw, "Epw")
    expect_equal(nrow(data), 8760L)
    expect_equal(period$start_day_of_week, "Monday")
    expect_equal(location$city, "Test City")
    expect_equal(location$state_province, "Test Province")
    expect_equal(location$country, "Test Country")
    expect_equal(location$time_zone, 8)
    expect_equal(data$year, rep(2001L, 8760L))
    expect_equal(data$dry_bulb_temperature, rep(20, 8760L))
    expect_equal(data$atmospheric_pressure, rep(101325, 8760L))
    expect_equal(
        data$relative_humidity[[1L]],
        humidity$relative_humidity,
        tolerance = 1e-6
    )
    expect_equal(
        data$dew_point_temperature[[1L]],
        humidity$dew_point_temperature,
        tolerance = 1e-6
    )
    expect_equal(audit$hour_count, 8760L)
    expect_equal(audit$epw_minute, 60L)
    expect_equal(audit$epw_start_day_of_week, "Monday")
})

test_that("to_epw() derives finite direct normal radiation and source flags", {
    dest <- epw_test__database()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    epw <- to_epw(dest)
    data <- epw$data()
    # March 21 around local standard noon at the time-zone meridian has solar
    # altitude close to 90 degrees at the equator, so DNI is close to GHI-DHI.
    equinox_noon <- (80L - 1L) * 24L + 12L + 1L

    expect_true(all(is.finite(data$direct_normal_radiation)))
    expect_equal(data$direct_normal_radiation[[equinox_noon]], 300, tolerance = 5)
    expect_equal(nchar(data$data_source[[equinox_noon]]), 44L)
    expect_equal(substr(
        data$data_source[[equinox_noon]],
        13L,
        14L
    ), "D9")
    expect_match(data$data_source[[equinox_noon]], "D9", fixed = TRUE)
    expect_true(all(
        data$direct_normal_radiation[data$global_horizontal_radiation == 0] == 0
    ))
    expect_lt(attr(epw, "destep_audit")$maximum_derived_dni_w_m2, 1500)
})

test_that("to_epw() writes hour-ending minute 60", {
    dest <- epw_test__database()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    epw <- to_epw(dest)
    first_data_line <- readLines(epw$path(), n = 9L, warn = FALSE)[[9L]]
    fields <- strsplit(first_data_line, ",", fixed = TRUE)[[1L]]

    expect_equal(fields[[4L]], "1")
    expect_equal(fields[[5L]], "60")
})

test_that("to_epw() selects a city-linked climate series", {
    dest <- epw_test__database(ids = c(1L, 2L), offsets = c(0, 10))
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    DBI::dbWriteTable(dest, "SYS_CITY", data.frame(
        CITY_ID = 10L,
        CLIMATE_ID = 2L
    ))

    epw <- to_epw(dest)

    expect_equal(epw$data()$dry_bulb_temperature[[1L]], 30)
    expect_equal(attr(epw, "destep_audit")$climate_id, 2)
})

test_that("to_epw() rejects ambiguous or incomplete climate series", {
    ambiguous <- epw_test__database(ids = c(1L, 2L), offsets = c(0, 10))
    on.exit(DBI::dbDisconnect(ambiguous), add = TRUE)
    expect_error(to_epw(ambiguous), "Cannot choose CLIMATE_DATA ID")

    missing_hour <- epw_test__database()
    on.exit(DBI::dbDisconnect(missing_hour), add = TRUE)
    DBI::dbExecute(missing_hour, "DELETE FROM CLIMATE_DATA WHERE HOUR = 8759")
    expect_error(to_epw(missing_hour), "missing HOUR")

    duplicate_hour <- epw_test__database()
    on.exit(DBI::dbDisconnect(duplicate_hour), add = TRUE)
    row <- epw_test__climate()[1L, ]
    DBI::dbWriteTable(duplicate_hour, "CLIMATE_DATA", row, append = TRUE)
    expect_error(to_epw(duplicate_hour), "duplicate HOUR")
})

test_that("to_epw() audits small radiation rounding and rejects larger inversions", {
    rounded <- epw_test__database()
    on.exit(DBI::dbDisconnect(rounded), add = TRUE)
    DBI::dbExecute(rounded, paste(
        "UPDATE CLIMATE_DATA SET HORI_TOTAL_RAD = 99.5,",
        "HORI_SCATTER_RAD = 100 WHERE HOUR = 12"
    ))
    epw <- to_epw(rounded)
    expect_equal(attr(epw, "destep_audit")$dhi_above_ghi_rounding_hours, 1L)
    expect_equal(epw$data()$global_horizontal_radiation[[13L]], 100)

    invalid <- epw_test__database()
    on.exit(DBI::dbDisconnect(invalid), add = TRUE)
    DBI::dbExecute(invalid, paste(
        "UPDATE CLIMATE_DATA SET HORI_TOTAL_RAD = 90,",
        "HORI_SCATTER_RAD = 100 WHERE HOUR = 12"
    ))
    expect_error(to_epw(invalid), "exceeds HORI_TOTAL_RAD by more than 1")
})

test_that("to_epw() caps mild supersaturation and rejects larger violations", {
    mild <- epw_test__database()
    on.exit(DBI::dbDisconnect(mild), add = TRUE)
    # At 20 C and 101325 Pa, 14.8 g/kg is only mildly supersaturated.
    DBI::dbExecute(mild, "UPDATE CLIMATE_DATA SET DAMP = 14.8 WHERE HOUR = 0")
    epw <- to_epw(mild)
    expect_equal(attr(epw, "destep_audit")$supersaturation_hours, 1L)
    expect_equal(epw$data()$relative_humidity[[1L]], 100)
    expect_equal(epw$data()$dew_point_temperature[[1L]], 20)

    excessive <- epw_test__database()
    on.exit(DBI::dbDisconnect(excessive), add = TRUE)
    DBI::dbExecute(excessive, "UPDATE CLIMATE_DATA SET DAMP = 20 WHERE HOUR = 0")
    expect_error(
        to_epw(excessive),
        "supersaturation exceeds supported rounding bounds"
    )
})

test_that("to_epw() converts the real DeST climate series", {
    skip_on_cran()

    src <- ensure_dest_sqlite_file()
    on.exit(DBI::dbDisconnect(src), add = TRUE)
    epw <- to_epw(src)
    data <- epw$data()
    audit <- attr(epw, "destep_audit")
    raw <- data.table::as.data.table(DBI::dbReadTable(src, "CLIMATE_DATA"))
    data.table::setorder(raw, HOUR)

    expect_equal(nrow(data), 8760L)
    expect_equal(epw$location()$city, "Chongqin")
    expect_equal(epw$location()$time_zone, 8)
    expect_equal(data$dry_bulb_temperature, raw$DRY_BULB_T)
    expect_equal(data$atmospheric_pressure, raw$B)
    expect_equal(data$global_horizontal_radiation, raw$HORI_TOTAL_RAD)
    expect_equal(data$diffuse_horizontal_radiation, raw$HORI_SCATTER_RAD)
    expect_true(all(is.finite(data$direct_normal_radiation)))
    expect_equal(audit$supersaturation_hours, 125L)
    expect_equal(audit$maximum_derived_dni_w_m2, 1174.677, tolerance = 0.01)
    expect_equal(
        audit$positive_beam_horizontal_at_nonpositive_solar_altitude_hours,
        6L
    )
})
