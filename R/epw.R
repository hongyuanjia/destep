#' Convert DeST climate data to an EnergyPlus weather object
#'
#' `to_epw()` converts the selected `CLIMATE_DATA` hourly series in a DeST
#' model to an eplusr `Epw` object. The returned object stays in memory;
#' call its `$save()` method to write an EPW file.
#'
#' @param dest A DBI connection, a path to a SQLite database produced by
#'   [read_dest()], or a path to a DeST Access `.accdb`/`.mdb` file.
#'
#' @return An `eplusr::Epw` object. The `destep_audit` attribute records input
#'   repairs and radiation diagnostics.
#'
#' @export
to_epw <- function(dest) {
    connection <- epw__connection(dest)
    con <- connection$con
    if (isTRUE(connection$disconnect)) {
        on.exit(DBI::dbDisconnect(con), add = TRUE)
    }

    climate <- epw__climate_data(con)
    environment <- epw__environment(con)
    missing <- epw__missing_codes()
    converted <- epw__data(climate, environment, missing)

    path <- tempfile("destep-", fileext = ".epw")
    epw__write(path, converted$data, environment)
    epw <- suppressWarnings(eplusr::read_epw(path))
    attr(epw, "destep_audit") <- converted$audit
    epw
}

# Open a supported DeST input and share read_dest()'s Access fallback behavior.
epw__connection <- function(dest) {
    if (inherits(dest, "DBIConnection")) {
        if (!DBI::dbIsValid(dest)) {
            stop("`dest` is not a valid DBI connection.", call. = FALSE)
        }
        return(list(con = dest, disconnect = FALSE))
    }
    if (!is.character(dest) || length(dest) != 1L || is.na(dest)) {
        stop(
            "`dest` must be a DBI connection or a path to a DeST database.",
            call. = FALSE
        )
    }
    if (!file.exists(dest)) {
        stop("DeST database does not exist: ", dest, call. = FALSE)
    }

    extension <- tolower(tools::file_ext(dest))
    if (extension %in% c("accdb", "mdb")) {
        return(list(con = read_dest(dest), disconnect = TRUE))
    }
    list(con = DBI::dbConnect(RSQLite::SQLite(), dest), disconnect = TRUE)
}

# Select and validate the single complete non-leap CLIMATE_DATA series.
epw__climate_data <- function(dest) {
    if (!db__has_rows(dest, "CLIMATE_DATA")) {
        stop("No CLIMATE_DATA rows found in the DeST model.", call. = FALSE)
    }
    required <- c(
        "ID", "HOUR", "DRY_BULB_T", "DAMP", "HORI_TOTAL_RAD",
        "HORI_SCATTER_RAD", "T_GROUND", "T_SKY", "WS", "WD", "B"
    )
    missing <- setdiff(required, DBI::dbListFields(dest, "CLIMATE_DATA"))
    if (length(missing)) {
        stop(sprintf(
            "CLIMATE_DATA is missing required field(s): %s",
            paste(missing, collapse = ", ")
        ), call. = FALSE)
    }

    climate_id <- epw__select_climate_id(dest)
    climate <- DBI::dbGetQuery(
        dest,
        sprintf(
            paste(
                "SELECT ID, HOUR, DRY_BULB_T, DAMP, HORI_TOTAL_RAD,",
                "HORI_SCATTER_RAD, T_GROUND, T_SKY, WS, WD, B",
                "FROM CLIMATE_DATA WHERE ID = %s ORDER BY HOUR"
            ),
            DBI::dbQuoteLiteral(dest, climate_id)
        )
    )
    data.table::setDT(climate)
    data__force_numeric(climate, names(climate))
    epw__validate_climate(climate, climate_id)
    climate
}

# Resolve the active climate through ENVIRONMENT/SYS_CITY when possible, then
# fall back only when CLIMATE_DATA itself contains one unambiguous ID.
epw__select_climate_id <- function(dest) {
    resolved <- epw__resolved_city_climate_ids(dest)
    if (length(resolved) == 1L) return(resolved[[1L]])
    if (length(resolved) > 1L) {
        stop(sprintf(
            "Multiple CLIMATE_DATA IDs are referenced by ENVIRONMENT/SYS_CITY: %s",
            paste(resolved, collapse = ", ")
        ), call. = FALSE)
    }

    ids <- DBI::dbGetQuery(
        dest,
        "SELECT DISTINCT ID FROM CLIMATE_DATA ORDER BY ID"
    )$ID
    ids <- ids[!is.na(ids)]
    if (length(ids) == 1L) return(ids[[1L]])
    stop(sprintf(
        paste(
            "Cannot choose CLIMATE_DATA ID; multiple IDs are present and",
            "ENVIRONMENT/SYS_CITY did not select one: %s"
        ),
        paste(ids, collapse = ", ")
    ), call. = FALSE)
}

# Return city-linked climate IDs without guessing when library rows are absent.
epw__resolved_city_climate_ids <- function(dest) {
    tables <- DBI::dbListTables(dest)
    if (!all(c("ENVIRONMENT", "SYS_CITY", "CLIMATE_DATA") %in% tables)) {
        return(numeric())
    }
    if (!db__has_fields(dest, "ENVIRONMENT", "CITY_ID") ||
        !db__has_fields(dest, "SYS_CITY", c("CITY_ID", "CLIMATE_ID"))) {
        return(numeric())
    }

    ids <- DBI::dbGetQuery(
        dest,
        paste(
            "SELECT DISTINCT C.CLIMATE_ID AS ID FROM ENVIRONMENT E",
            "INNER JOIN SYS_CITY C ON E.CITY_ID = C.CITY_ID",
            "INNER JOIN CLIMATE_DATA D ON C.CLIMATE_ID = D.ID",
            "WHERE C.CLIMATE_ID IS NOT NULL ORDER BY C.CLIMATE_ID"
        )
    )$ID
    ids[!is.na(ids)]
}

# Reject incomplete or physically unsupported weather before EPW serialization.
epw__validate_climate <- function(climate, climate_id) {
    issues <- character()
    hour <- climate$HOUR
    if (nrow(climate) != 8760L) {
        issues <- c(issues, sprintf("expected 8760 rows but found %i", nrow(climate)))
    }
    if (anyNA(hour)) {
        issues <- c(issues, "HOUR contains missing values")
    } else {
        missing_hour <- setdiff(0:8759, hour)
        duplicate_hour <- unique(hour[duplicated(hour)])
        unexpected_hour <- setdiff(hour, 0:8759)
        if (length(missing_hour)) {
            issues <- c(issues, sprintf(
                "missing HOUR value(s): %s",
                data__format_integer_sample(missing_hour)
            ))
        }
        if (length(duplicate_hour)) {
            issues <- c(issues, sprintf(
                "duplicate HOUR value(s): %s",
                data__format_integer_sample(duplicate_hour)
            ))
        }
        if (length(unexpected_hour)) {
            issues <- c(issues, sprintf(
                "unexpected HOUR value(s): %s",
                data__format_integer_sample(unexpected_hour)
            ))
        }
    }

    core <- c(
        "DRY_BULB_T", "DAMP", "HORI_TOTAL_RAD", "HORI_SCATTER_RAD",
        "T_SKY", "WS", "WD", "B"
    )
    for (field in core) {
        if (anyNA(climate[[field]])) {
            issues <- c(issues, sprintf("%s contains missing values", field))
        }
    }
    if (any(climate$DRY_BULB_T <= -70 | climate$DRY_BULB_T >= 70, na.rm = TRUE)) {
        issues <- c(issues, "DRY_BULB_T contains value(s) outside (-70, 70) C")
    }
    if (any(climate$DAMP < 0 | climate$DAMP > 100, na.rm = TRUE)) {
        issues <- c(issues, "DAMP contains value(s) outside 0:100 g/kg dry air")
    }
    if (any(climate$HORI_TOTAL_RAD < 0, na.rm = TRUE)) {
        issues <- c(issues, "HORI_TOTAL_RAD contains negative values")
    }
    if (any(climate$HORI_SCATTER_RAD < 0, na.rm = TRUE)) {
        issues <- c(issues, "HORI_SCATTER_RAD contains negative values")
    }
    if (any(climate$T_SKY < 100 | climate$T_SKY > 400, na.rm = TRUE)) {
        issues <- c(issues, "T_SKY contains value(s) outside 100:400 K")
    }
    if (any(climate$WS < 0 | climate$WS > 40, na.rm = TRUE)) {
        issues <- c(issues, "WS contains value(s) outside 0:40 m/s")
    }
    if (any(!(climate$WD %in% 0:16), na.rm = TRUE)) {
        issues <- c(issues, "WD contains value(s) outside integer codes 0:16")
    }
    if (any(climate$B <= 31000 | climate$B >= 120000, na.rm = TRUE)) {
        issues <- c(issues, "B contains value(s) outside (31000, 120000) Pa")
    }
    if (length(issues)) {
        stop(sprintf(
            "Invalid CLIMATE_DATA series for ID %s: %s",
            climate_id,
            paste(issues, collapse = "; ")
        ), call. = FALSE)
    }
    invisible(climate)
}

# Read the unique site metadata row required by the EPW LOCATION header.
epw__environment <- function(dest) {
    if (!db__has_rows(dest, "ENVIRONMENT")) {
        stop("No ENVIRONMENT rows found in the DeST model.", call. = FALSE)
    }
    required <- c(
        "CITY_ID", "CITY_NAME", "PROVINCE", "COUNTRY", "LATITUDE",
        "LONGITUDE", "ELEVATION", "PROPERTY"
    )
    missing <- setdiff(required, DBI::dbListFields(dest, "ENVIRONMENT"))
    if (length(missing)) {
        stop(sprintf(
            "ENVIRONMENT is missing required field(s): %s",
            paste(missing, collapse = ", ")
        ), call. = FALSE)
    }
    environment <- data.table::as.data.table(DBI::dbGetQuery(
        dest,
        paste(
            "SELECT CITY_ID, CITY_NAME, PROVINCE, COUNTRY, LATITUDE,",
            "LONGITUDE, ELEVATION, PROPERTY FROM ENVIRONMENT"
        )
    ))
    if (nrow(environment) != 1L) {
        stop(sprintf(
            "Expected one ENVIRONMENT row but found %i.",
            nrow(environment)
        ), call. = FALSE)
    }
    data__force_numeric(environment, c(
        "CITY_ID", "LATITUDE", "LONGITUDE", "ELEVATION", "PROPERTY"
    ))
    if (is.na(environment$LATITUDE) || abs(environment$LATITUDE) > 90 ||
        is.na(environment$LONGITUDE) || abs(environment$LONGITUDE) > 180) {
        stop("ENVIRONMENT contains an invalid latitude or longitude.", call. = FALSE)
    }
    environment
}

# Decode the standard meridian stored in the low 16 bits of PROPERTY.
epw__time_zone <- function(environment) {
    property <- environment$PROPERTY[[1L]]
    if (!is.na(property)) {
        code <- bitwAnd(as.integer(property), 65535L)
        if (!is.na(code) && code >= 0L && code <= 36000L) {
            longitude <- if (code <= 18000L) {
                code / 100
            } else {
                -(36000L - code) / 100
            }
            time_zone <- longitude / 15
            if (time_zone >= -12 && time_zone <= 14) return(time_zone)
        }
    }
    round(environment$LONGITUDE[[1L]] / 15)
}

# Convert DeST humidity ratio to EPW dew point and RH. Mild supersaturation is
# treated as source rounding only within the supported physical bounds.
epw__humidity <- function(dry_bulb, humidity_ratio_g_kg, pressure) {
    humidity_ratio <- humidity_ratio_g_kg / 1000
    vapor_pressure <- pressure * humidity_ratio / (0.621945 + humidity_ratio)
    saturation_pressure <- 611.2 * exp(
        17.67 * dry_bulb / (dry_bulb + 243.5)
    )
    raw_rh <- 100 * vapor_pressure / saturation_pressure
    gamma <- log(pmax(vapor_pressure, 1e-6) / 611.2)
    raw_dew_point <- 243.5 * gamma / (17.67 - gamma)
    supersaturated <- raw_rh > 100 | raw_dew_point > dry_bulb
    excessive <- raw_rh > 101.5 | raw_dew_point - dry_bulb > 0.25
    if (any(excessive)) {
        stop(sprintf(
            paste(
                "CLIMATE_DATA supersaturation exceeds supported rounding bounds at",
                "%i hour(s); max RH=%.6f%% and max dew-point excess=%.6f C."
            ),
            sum(excessive),
            max(raw_rh),
            max(raw_dew_point - dry_bulb)
        ), call. = FALSE)
    }

    capped_vapor_pressure <- pmin(vapor_pressure, saturation_pressure)
    capped_ratio <- 0.621945 * capped_vapor_pressure /
        (pressure - capped_vapor_pressure)
    list(
        dew_point_temperature = pmin(raw_dew_point, dry_bulb),
        relative_humidity = pmin(raw_rh, 100),
        audit = list(
            supersaturation_hours = sum(supersaturated),
            maximum_raw_relative_humidity_percent = max(raw_rh),
            maximum_dew_point_excess_c = max(raw_dew_point - dry_bulb),
            maximum_humidity_ratio_adjustment_g_kg =
                max((humidity_ratio - capped_ratio) * 1000)
        )
    )
}

# Approximate the sine of solar altitude at each DeST HOUR timestamp using the
# NOAA fractional-year equations and local standard time.
epw__solar_sine_altitude <- function(hour, latitude, longitude, time_zone) {
    day <- hour %/% 24L + 1L
    local_hour <- hour %% 24L
    gamma <- 2 * pi / 365 * (day - 1 + (local_hour - 12) / 24)
    equation_of_time <- 229.18 * (
        0.000075 + 0.001868 * cos(gamma) - 0.032077 * sin(gamma) -
            0.014615 * cos(2 * gamma) - 0.040849 * sin(2 * gamma)
    )
    declination <- 0.006918 - 0.399912 * cos(gamma) +
        0.070257 * sin(gamma) - 0.006758 * cos(2 * gamma) +
        0.000907 * sin(2 * gamma) - 0.002697 * cos(3 * gamma) +
        0.00148 * sin(3 * gamma)
    true_solar_minutes <- (
        local_hour * 60 + equation_of_time + 4 * longitude - 60 * time_zone
    ) %% 1440
    hour_angle <- (true_solar_minutes / 4 - 180) * pi / 180
    latitude <- latitude * pi / 180
    sin(latitude) * sin(declination) +
        cos(latitude) * cos(declination) * cos(hour_angle)
}

# Derive DNI from DeST GHI and DHI at the source HOUR timestamp. This timestamp
# is used because the development data produce nonphysical near-horizon DNI at
# the interval midpoint; the selected convention is recorded in diagnostics.
epw__radiation <- function(climate, environment) {
    discrepancy <- climate$HORI_SCATTER_RAD - climate$HORI_TOTAL_RAD
    if (any(discrepancy > 1)) {
        stop(sprintf(
            paste(
                "HORI_SCATTER_RAD exceeds HORI_TOTAL_RAD by more than 1 W/m2",
                "at %i hour(s); maximum excess is %.6f W/m2."
            ),
            sum(discrepancy > 1),
            max(discrepancy)
        ), call. = FALSE)
    }
    global <- pmax(climate$HORI_TOTAL_RAD, climate$HORI_SCATTER_RAD)
    beam_horizontal <- pmax(global - climate$HORI_SCATTER_RAD, 0)
    time_zone <- epw__time_zone(environment)
    sine_altitude <- epw__solar_sine_altitude(
        climate$HOUR,
        environment$LATITUDE[[1L]],
        environment$LONGITUDE[[1L]],
        time_zone
    )
    daylight <- sine_altitude > 0
    direct_normal <- numeric(nrow(climate))
    direct_normal[daylight] <- beam_horizontal[daylight] /
        sine_altitude[daylight]
    if (any(direct_normal > 1500)) {
        stop(sprintf(
            "Derived direct normal radiation exceeds 1500 W/m2; maximum is %.6f W/m2.",
            max(direct_normal)
        ), call. = FALSE)
    }

    list(
        global_horizontal = global,
        direct_normal = direct_normal,
        diffuse_horizontal = climate$HORI_SCATTER_RAD,
        daylight = daylight,
        audit = list(
            dhi_above_ghi_rounding_hours = sum(discrepancy > 0),
            maximum_dhi_above_ghi_w_m2 = max(discrepancy, 0),
            positive_beam_horizontal_at_nonpositive_solar_altitude_hours =
                sum(beam_horizontal > 0 & !daylight),
            maximum_discarded_beam_horizontal_w_m2 =
                max(beam_horizontal[!daylight], 0),
            maximum_derived_dni_w_m2 = max(direct_normal),
            solar_representative_time = "DeST HOUR timestamp"
        )
    )
}

# Mark derived meteorological quantities with E and derived DNI with D, using
# uncertainty 9 because the source database does not provide uncertainty data.
epw__source_flags <- function(daylight) {
    vapply(daylight, function(is_daylight) {
        paste0(
            "?9", "E9", "E9", "?9", "?9", "?9",
            if (is_daylight) "D9" else "?0",
            # EPW defines 22 positional source/uncertainty pairs. DNI is pair
            # seven; the remaining 15 cover DHI through days since snowfall.
            paste(rep("?9", 15L), collapse = "")
        )
    }, character(1L))
}

# Construct the 35 EPW data fields and attach machine-readable diagnostics.
epw__data <- function(climate, environment, missing) {
    humidity <- epw__humidity(
        climate$DRY_BULB_T,
        climate$DAMP,
        climate$B
    )
    radiation <- epw__radiation(climate, environment)
    datetime <- as.POSIXct("2001-01-01 01:00:00", tz = "UTC") +
        climate$HOUR * 3600
    data <- data.table::data.table(
        datetime = datetime,
        year = 2001L,
        data_source = epw__source_flags(radiation$daylight),
        dry_bulb_temperature = climate$DRY_BULB_T,
        dew_point_temperature = humidity$dew_point_temperature,
        relative_humidity = humidity$relative_humidity,
        atmospheric_pressure = climate$B,
        extraterrestrial_horizontal_radiation =
            missing$extraterrestrial_horizontal_radiation,
        extraterrestrial_direct_normal_radiation =
            missing$extraterrestrial_direct_normal_radiation,
        horizontal_infrared_radiation_intensity_from_sky =
            5.6697e-8 * climate$T_SKY^4,
        global_horizontal_radiation = radiation$global_horizontal,
        direct_normal_radiation = radiation$direct_normal,
        diffuse_horizontal_radiation = radiation$diffuse_horizontal,
        global_horizontal_illuminance = missing$global_horizontal_illuminance,
        direct_normal_illuminance = missing$direct_normal_illuminance,
        diffuse_horizontal_illuminance = missing$diffuse_horizontal_illuminance,
        zenith_luminance = missing$zenith_luminance,
        wind_direction = epw__wind_direction(climate$WD),
        wind_speed = climate$WS,
        total_sky_cover = missing$total_sky_cover,
        opaque_sky_cover = missing$opaque_sky_cover,
        visibility = missing$visibility,
        ceiling_height = missing$ceiling_height,
        present_weather_observation = missing$present_weather_observation,
        present_weather_codes = missing$present_weather_codes,
        precipitable_water = missing$precipitable_water,
        aerosol_optical_depth = missing$aerosol_optical_depth,
        snow_depth = missing$snow_depth,
        days_since_last_snow = missing$days_since_last_snow,
        albedo = missing$albedo,
        liquid_precip_depth = missing$liquid_precip_depth,
        liquid_precip_rate = missing$liquid_precip_rate
    )
    list(
        data = data,
        audit = c(
            list(
                climate_id = unique(climate$ID),
                hour_count = nrow(climate),
                hour_minimum = min(climate$HOUR),
                hour_maximum = max(climate$HOUR),
                epw_year = 2001L,
                epw_start_day_of_week = "Monday",
                epw_minute = 60L,
                time_zone = epw__time_zone(environment)
            ),
            humidity$audit,
            radiation$audit
        )
    )
}

# Convert the DeST 16-point wind code to degrees clockwise from north.
epw__wind_direction <- function(wind_direction) {
    ifelse(wind_direction == 0, 0, (wind_direction - 1) * 22.5)
}

# Ask eplusr for its version-matched EPW missing-code table.
epw__missing_codes <- function() {
    path <- tempfile("destep-missing-code-", fileext = ".epw")
    epw__write_missing_template(path)
    suppressWarnings(eplusr::read_epw(path)$missing_code())
}

# Write a valid placeholder year used only to initialize missing-code lookup.
epw__write_missing_template <- function(path) {
    header <- c(
        "LOCATION,DeST Template,NA,Unknown,DeST,000000,0,0,0,0",
        "DESIGN CONDITIONS,0",
        "TYPICAL/EXTREME PERIODS,0",
        "GROUND TEMPERATURES,0",
        "HOLIDAYS/DAYLIGHT SAVINGS,No,0,0,0",
        "COMMENTS 1,Generated by destep for EPW missing-code lookup",
        "COMMENTS 2,Weather data are placeholders",
        "DATA PERIODS,1,1,Data,Monday, 1/ 1,12/31"
    )
    dates <- seq(as.Date("2001-01-01"), as.Date("2001-12-31"), by = "day")
    month <- rep(as.integer(format(dates, "%m")), each = 24L)
    day <- rep(as.integer(format(dates, "%d")), each = 24L)
    hour <- rep(1:24, times = length(dates))
    data <- sprintf(
        paste0(
            "2001,%d,%d,%d,60,?,20,10,50,101325,9999,9999,300,0,",
            "9999,0,999999,999999,999999,9999,0,0,99,99,9999,",
            "99999,9,999999999,999,0.999,999,99,999,999,99"
        ),
        month,
        day,
        hour
    )
    writeLines(c(header, data), path, useBytes = TRUE)
}

# Serialize a complete EPW file with hour-ending timestamps and a Monday start
# that matches destep's Schedule:Week mapping of the first seven source days.
epw__write <- function(path, weather, environment) {
    header <- c(
        sprintf(
            "LOCATION,%s,%s,%s,DeST CLIMATE_DATA,%s,%s,%s,%s,%s",
            epw__header_value(environment$CITY_NAME, "DeST"),
            epw__header_value(environment$PROVINCE, "NA"),
            epw__header_value(environment$COUNTRY, "Unknown"),
            epw__header_value(environment$CITY_ID, "000000"),
            epw__header_value(environment$LATITUDE, 0),
            epw__header_value(environment$LONGITUDE, 0),
            epw__header_value(epw__time_zone(environment), 0),
            epw__header_value(environment$ELEVATION, 0)
        ),
        "DESIGN CONDITIONS,0",
        "TYPICAL/EXTREME PERIODS,0",
        "GROUND TEMPERATURES,0",
        "HOLIDAYS/DAYLIGHT SAVINGS,No,0,0,0",
        "COMMENTS 1,Generated by destep from DeST CLIMATE_DATA",
        paste(
            "COMMENTS 2,DNI derived from GHI and DHI at the DeST HOUR",
            "timestamp; mild supersaturation capped within supported bounds"
        ),
        "DATA PERIODS,1,1,Data,Monday, 1/ 1,12/31"
    )
    dates <- seq(as.Date("2001-01-01"), as.Date("2001-12-31"), by = "day")
    month <- rep(as.integer(format(dates, "%m")), each = 24L)
    day <- rep(as.integer(format(dates, "%d")), each = 24L)
    hour <- rep(1:24, times = length(dates))
    data <- sprintf(
        paste(
            "2001,%d,%d,%d,60,%s,",
            "%.10g,%.10g,%.10g,%.10g,%.10g,%.10g,%.10g,%.10g,",
            "%.10g,%.10g,%.10g,%.10g,%.10g,%.10g,%.10g,%.10g,",
            "%.10g,%.10g,%.10g,%.10g,%d,%s,%.10g,%.10g,%.10g,%d,",
            "%.10g,%.10g,%.10g",
            sep = ""
        ),
        month,
        day,
        hour,
        weather$data_source,
        weather$dry_bulb_temperature,
        weather$dew_point_temperature,
        weather$relative_humidity,
        weather$atmospheric_pressure,
        weather$extraterrestrial_horizontal_radiation,
        weather$extraterrestrial_direct_normal_radiation,
        weather$horizontal_infrared_radiation_intensity_from_sky,
        weather$global_horizontal_radiation,
        weather$direct_normal_radiation,
        weather$diffuse_horizontal_radiation,
        weather$global_horizontal_illuminance,
        weather$direct_normal_illuminance,
        weather$diffuse_horizontal_illuminance,
        weather$zenith_luminance,
        weather$wind_direction,
        weather$wind_speed,
        weather$total_sky_cover,
        weather$opaque_sky_cover,
        weather$visibility,
        weather$ceiling_height,
        weather$present_weather_observation,
        weather$present_weather_codes,
        weather$precipitable_water,
        weather$aerosol_optical_depth,
        weather$snow_depth,
        weather$days_since_last_snow,
        weather$albedo,
        weather$liquid_precip_depth,
        weather$liquid_precip_rate
    )
    writeLines(c(header, data), path, useBytes = TRUE)
}

# Normalize required LOCATION values and prevent comma-delimited header damage.
epw__header_value <- function(value, default) {
    value <- as.character(value[[1L]])
    if (is.na(value) || !nzchar(value)) value <- as.character(default)
    gsub(",", " ", value, fixed = TRUE)
}
