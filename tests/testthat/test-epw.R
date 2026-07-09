# Build a compact ENVIRONMENT table used by to_epw() tests.  PROPERTY encodes a
# 120-degree east standard meridian in DeST's low 16 bits, i.e. UTC+8.
destep_test_epw_environment <- function(city_id = 10L) {
    data.frame(
        CITY_ID = city_id,
        CITY_NAME = "Test City",
        PROVINCE = "Test Province",
        COUNTRY = "Test Country",
        LATITUDE = 30,
        LONGITUDE = 120,
        ELEVATION = 100,
        PROPERTY = 12000L
    )
}

# Create one complete non-leap CLIMATE_DATA series.  Keeping the weather values
# mostly constant makes public EPW field assertions straightforward.
destep_test_epw_climate_rows <- function(id = 1L, offset = 0) {
    hour <- 0:8759
    epw_hour <- rep(1:24, times = 365L)
    daylight <- epw_hour >= 8L & epw_hour <= 18L

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

# Assemble an in-memory SQLite DeST subset for EPW conversion tests.
destep_test_epw_db <- function(ids = 1L, offsets = 0) {
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    rows <- do.call(rbind, Map(
        destep_test_epw_climate_rows,
        ids,
        offsets
    ))
    DBI::dbWriteTable(dest, "ENVIRONMENT", destep_test_epw_environment())
    DBI::dbWriteTable(dest, "CLIMATE_DATA", rows)
    dest
}

# Mirror the public psychrometric expectation without reaching into to_epw()
# internals from the test assertion.
destep_test_epw_humidity <- function(dry_bulb, damp, pressure) {
    humidity_ratio <- damp / 1000
    vapor_pressure <- pressure * humidity_ratio / (0.621945 + humidity_ratio)
    saturation_pressure <- 611.2 * exp((17.67 * dry_bulb) / (dry_bulb + 243.5))
    gamma <- log(pmax(vapor_pressure, 1e-6) / 611.2)

    list(
        relative_humidity = pmin(100, pmax(0, 100 * vapor_pressure / saturation_pressure)),
        dew_point_temperature = 243.5 * gamma / (17.67 - gamma)
    )
}

test_that("to_epw() converts CLIMATE_DATA to an eplusr Epw object", {
    dest <- destep_test_epw_db()
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    epw <- to_epw(dest)
    data <- epw$data()
    location <- epw$location()
    period <- epw$period()
    missing <- destep_epw_missing_codes()
    humidity <- destep_test_epw_humidity(20, 7.5, 101325)

    expect_s3_class(epw, "Epw")
    expect_equal(nrow(data), 8760L)
    expect_equal(period$start_day_of_week, "Sunday")
    expect_equal(location$city, "Test City")
    expect_equal(location$state_province, "Test Province")
    expect_equal(location$country, "Test Country")
    expect_equal(location$wmo_number, "10")
    expect_equal(location$time_zone, 8)
    expect_equal(data$dry_bulb_temperature[[1L]], 20)
    expect_equal(data$atmospheric_pressure[[1L]], 101325)
    expect_equal(data$global_horizontal_radiation[[13L]], 400)
    expect_equal(data$diffuse_horizontal_radiation[[13L]], 100)
    expect_equal(data$direct_normal_radiation[[13L]], missing$direct_normal_radiation)
    expect_equal(data$visibility[[1L]], missing$visibility)
    expect_equal(data$present_weather_codes[[1L]], missing$present_weather_codes)
    expect_equal(data$wind_direction[1:17], c(0, 0, seq(22.5, 337.5, by = 22.5)))
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
})

test_that("to_epw() accepts SQLite file paths", {
    path <- tempfile(fileext = ".sql")
    dest <- DBI::dbConnect(RSQLite::SQLite(), path)
    on.exit({
        DBI::dbDisconnect(dest)
        unlink(path)
    }, add = TRUE)
    DBI::dbWriteTable(dest, "ENVIRONMENT", destep_test_epw_environment())
    DBI::dbWriteTable(dest, "CLIMATE_DATA", destep_test_epw_climate_rows())

    epw <- to_epw(path)

    expect_s3_class(epw, "Epw")
    expect_equal(nrow(epw$data()), 8760L)
})

test_that("to_epw() uses SYS_CITY.CLIMATE_ID when multiple climate data IDs exist", {
    dest <- destep_test_epw_db(ids = c(1L, 2L), offsets = c(0, 10))
    on.exit(DBI::dbDisconnect(dest), add = TRUE)
    DBI::dbWriteTable(dest, "SYS_CITY", data.frame(
        CITY_ID = 10L,
        CLIMATE_ID = 2L
    ))

    epw <- to_epw(dest)

    expect_equal(epw$data()$dry_bulb_temperature[[1L]], 30)
})

test_that("to_epw() stops when CLIMATE_DATA ID cannot be selected", {
    dest <- destep_test_epw_db(ids = c(1L, 2L), offsets = c(0, 10))
    on.exit(DBI::dbDisconnect(dest), add = TRUE)

    expect_error(
        to_epw(dest),
        "Cannot choose CLIMATE_DATA ID"
    )
})

test_that("to_epw() stops on invalid CLIMATE_DATA hourly series", {
    missing_hour <- destep_test_epw_db()
    on.exit(DBI::dbDisconnect(missing_hour), add = TRUE)
    DBI::dbExecute(missing_hour, "DELETE FROM CLIMATE_DATA WHERE HOUR = 8759")
    expect_error(to_epw(missing_hour), "missing HOUR")

    duplicate_hour <- destep_test_epw_db()
    on.exit(DBI::dbDisconnect(duplicate_hour), add = TRUE)
    DBI::dbExecute(duplicate_hour, "
        INSERT INTO CLIMATE_DATA (
            ID, HOUR, DRY_BULB_T, DAMP, HORI_TOTAL_RAD,
            HORI_SCATTER_RAD, T_GROUND, T_SKY, WS, WD, B
        )
        VALUES (1, 0, 20, 7.5, 0, 0, 15, 273.15, 2, 0, 101325)
    ")
    expect_error(to_epw(duplicate_hour), "duplicate HOUR")

    missing_value <- destep_test_epw_db()
    on.exit(DBI::dbDisconnect(missing_value), add = TRUE)
    DBI::dbExecute(missing_value, "
        UPDATE CLIMATE_DATA
        SET DRY_BULB_T = NULL
        WHERE HOUR = 0
    ")
    expect_error(to_epw(missing_value), "DRY_BULB_T contains missing values")

    bad_wind_direction <- destep_test_epw_db()
    on.exit(DBI::dbDisconnect(bad_wind_direction), add = TRUE)
    DBI::dbExecute(bad_wind_direction, "
        UPDATE CLIMATE_DATA
        SET WD = 17
        WHERE HOUR = 0
    ")
    expect_error(to_epw(bad_wind_direction), "WD contains value")
})

test_that("to_epw() converts real DeST climate data", {
    skip_on_cran()

    src <- ensure_dest_sqlite_file()
    on.exit(DBI::dbDisconnect(src), add = TRUE)

    epw <- to_epw(src)
    data <- epw$data()
    location <- epw$location()
    raw <- DBI::dbGetQuery(src, "
        SELECT *
        FROM CLIMATE_DATA
        ORDER BY HOUR
        LIMIT 1
    ")
    out <- tempfile(fileext = ".epw")
    epw$save(out, overwrite = TRUE)
    reread <- eplusr::read_epw(out)

    expect_equal(nrow(data), 8760L)
    expect_equal(location$city, "Chongqin")
    expect_equal(location$state_province, "Chongqing")
    expect_equal(location$country, "P.R.China")
    expect_equal(location$wmo_number, "57516")
    expect_equal(data$dry_bulb_temperature[[1L]], raw$DRY_BULB_T[[1L]])
    expect_equal(data$atmospheric_pressure[[1L]], raw$B[[1L]])
    expect_equal(data$global_horizontal_radiation[[1L]], raw$HORI_TOTAL_RAD[[1L]])
    expect_equal(data$diffuse_horizontal_radiation[[1L]], raw$HORI_SCATTER_RAD[[1L]])
    expect_equal(
        data$horizontal_infrared_radiation_intensity_from_sky[[1L]],
        5.6697e-8 * raw$T_SKY[[1L]]^4,
        tolerance = 1e-6
    )
    expect_equal(nrow(reread$data()), 8760L)
})
