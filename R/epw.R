#' Convert DeST climate data to an EnergyPlus weather object
#'
#' `to_epw()` converts the selected `CLIMATE_DATA` hourly weather series in a
#' DeST model to an \{eplusr\} [`Epw`][eplusr::Epw] object. The returned object
#' is kept in memory; call `$save(path, overwrite = TRUE)` on it to write an EPW
#' file.
#'
#' @param dest A DBI connection, a path to a SQLite database produced by
#'   [read_dest()], or a path to a DeST Access `.accdb`/`.mdb` file.
#'
#' @return An `eplusr::Epw` object.
#'
#' @export
to_epw <- function(dest) {
    conn <- destep_epw_connection(dest)
    con <- conn$con
    if (isTRUE(conn$disconnect)) {
        on.exit(DBI::dbDisconnect(con), add = TRUE)
    }

    climate <- destep_climate_data_table(con)
    environment <- destep_epw_environment(con)
    weather <- destep_epw_data(climate, environment)

    path <- tempfile("destep-", fileext = ".epw")
    destep_write_epw(path, weather, environment)
    suppressWarnings(eplusr::read_epw(path))
}

# Open the supported DeST inputs used by to_epw().  Access databases are routed
# through read_dest() so the weather converter shares the package's existing
# ODBC/MDBTools behavior.
destep_epw_connection <- function(dest) {
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

    ext <- tolower(tools::file_ext(dest))
    if (ext %in% c("accdb", "mdb")) {
        return(list(con = read_dest(dest), disconnect = TRUE))
    }

    list(con = DBI::dbConnect(RSQLite::SQLite(), dest), disconnect = TRUE)
}

# Select, validate, and return the single CLIMATE_DATA series that should drive
# the EPW object.
destep_climate_data_table <- function(dest) {
    if (!destep_has_rows(dest, "CLIMATE_DATA")) {
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

    climate_id <- destep_select_climate_data_id(dest)
    climate <- DBI::dbGetQuery(
        dest,
        sprintf(
            "
            SELECT
                ID,
                HOUR,
                DRY_BULB_T,
                DAMP,
                HORI_TOTAL_RAD,
                HORI_SCATTER_RAD,
                T_GROUND,
                T_SKY,
                WS,
                WD,
                B
            FROM CLIMATE_DATA
            WHERE ID = %s
            ORDER BY HOUR
            ",
            DBI::dbQuoteLiteral(dest, climate_id)
        )
    )
    data.table::setDT(climate)
    destep_force_numeric(climate, names(climate))
    destep_validate_climate_data_table(climate, climate_id)

    climate
}

# Resolve CLIMATE_DATA through ENVIRONMENT/SYS_CITY when the DeST city library
# row is available; otherwise only a unique CLIMATE_DATA.ID can be selected
# without inventing a precedence rule.
destep_select_climate_data_id <- function(dest) {
    resolved <- destep_resolve_city_climate_data_ids(dest)
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
            "Cannot choose CLIMATE_DATA ID;",
            "multiple IDs are present and ENVIRONMENT/SYS_CITY did not select one: %s"
        ),
        paste(ids, collapse = ", ")
    ), call. = FALSE)
}

# The city-library bridge is optional because the current real fixture carries
# CLIMATE_DATA but does not resolve ENVIRONMENT.CITY_ID into SYS_CITY.
destep_resolve_city_climate_data_ids <- function(dest) {
    if (!all(c("ENVIRONMENT", "SYS_CITY", "CLIMATE_DATA") %in% DBI::dbListTables(dest))) {
        return(numeric())
    }
    if (!destep_table_has_fields(dest, "ENVIRONMENT", "CITY_ID") ||
        !destep_table_has_fields(dest, "SYS_CITY", c("CITY_ID", "CLIMATE_ID"))) {
        return(numeric())
    }

    ids <- DBI::dbGetQuery(
        dest,
        "
        SELECT DISTINCT C.CLIMATE_ID AS ID
        FROM ENVIRONMENT E
        INNER JOIN SYS_CITY C
        ON E.CITY_ID = C.CITY_ID
        INNER JOIN CLIMATE_DATA D
        ON C.CLIMATE_ID = D.ID
        WHERE C.CLIMATE_ID IS NOT NULL
        ORDER BY C.CLIMATE_ID
        "
    )$ID

    ids[!is.na(ids)]
}

# EPW generation needs one complete non-leap year and valid values for the DeST
# fields used in the mapping.
destep_validate_climate_data_table <- function(climate, climate_id) {
    issues <- character()
    hour <- climate$HOUR

    if (nrow(climate) != 8760L) {
        issues <- c(issues, sprintf(
            "expected 8760 rows but found %i",
            nrow(climate)
        ))
    }
    if (anyNA(hour)) {
        issues <- c(issues, "HOUR contains missing values")
    } else {
        missing_hours <- setdiff(0:8759, hour)
        duplicate_hours <- unique(hour[duplicated(hour)])
        unexpected_hours <- setdiff(hour, 0:8759)

        if (length(missing_hours)) {
            issues <- c(issues, sprintf(
                "missing HOUR value(s): %s",
                destep_format_integer_sample(missing_hours)
            ))
        }
        if (length(duplicate_hours)) {
            issues <- c(issues, sprintf(
                "duplicate HOUR value(s): %s",
                destep_format_integer_sample(duplicate_hours)
            ))
        }
        if (length(unexpected_hours)) {
            issues <- c(issues, sprintf(
                "unexpected HOUR value(s): %s",
                destep_format_integer_sample(unexpected_hours)
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

    if (any(!is.na(climate$WD) & !(climate$WD %in% 0:16))) {
        issues <- c(issues, "WD contains value(s) outside 0:16")
    }
    if (any(!is.na(climate$DAMP) & climate$DAMP < 0)) {
        issues <- c(issues, "DAMP contains negative values")
    }
    if (any(!is.na(climate$HORI_TOTAL_RAD) & climate$HORI_TOTAL_RAD < 0)) {
        issues <- c(issues, "HORI_TOTAL_RAD contains negative values")
    }
    if (any(!is.na(climate$HORI_SCATTER_RAD) & climate$HORI_SCATTER_RAD < 0)) {
        issues <- c(issues, "HORI_SCATTER_RAD contains negative values")
    }
    if (any(!is.na(climate$T_SKY) & climate$T_SKY <= 0)) {
        issues <- c(issues, "T_SKY contains non-positive values")
    }
    if (any(!is.na(climate$WS) & climate$WS < 0)) {
        issues <- c(issues, "WS contains negative values")
    }
    if (any(!is.na(climate$B) & climate$B <= 0)) {
        issues <- c(issues, "B contains non-positive values")
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

# Read the site metadata used by the EPW LOCATION header and time-zone fallback.
destep_epw_environment <- function(dest) {
    if (!destep_has_rows(dest, "ENVIRONMENT")) {
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

    env <- DBI::dbGetQuery(
        dest,
        "
        SELECT
            CITY_ID,
            CITY_NAME,
            PROVINCE,
            COUNTRY,
            LATITUDE,
            LONGITUDE,
            ELEVATION,
            PROPERTY
        FROM ENVIRONMENT
        LIMIT 1
        "
    )
    data.table::setDT(env)
    destep_force_numeric(env, c(
        "CITY_ID", "LATITUDE", "LONGITUDE", "ELEVATION", "PROPERTY"
    ))
    env[1L]
}

# Construct the full EPW data frame written by destep_write_epw(). DeST only
# stores weather variables needed by DeST, so EPW-specific illuminance, sky
# cover, and precipitation fields use documented EPW missing codes/defaults.
destep_epw_data <- function(climate, environment) {
    humidity <- destep_epw_humidity(
        dry_bulb = climate$DRY_BULB_T,
        humidity_ratio_g_per_kg = climate$DAMP,
        pressure = climate$B
    )
    time_zone <- destep_epw_time_zone(environment)
    datetime <- as.POSIXct("1999-01-01 01:00:00", tz = "UTC") +
        as.numeric(climate$HOUR) * 3600
    datetime_midpoint <- as.POSIXct("1999-01-01 00:30:00", tz = "UTC") +
        as.numeric(climate$HOUR) * 3600
    direct_normal <- destep_epw_direct_normal_radiation(
        global_horizontal = climate$HORI_TOTAL_RAD,
        diffuse_horizontal = climate$HORI_SCATTER_RAD,
        datetime = datetime_midpoint,
        latitude = environment$LATITUDE,
        longitude = environment$LONGITUDE,
        time_zone = time_zone
    )

    data.table::data.table(
        datetime = datetime,
        year = 1999L,
        data_source = "?",
        dry_bulb_temperature = climate$DRY_BULB_T,
        dew_point_temperature = humidity$dew_point_temperature,
        relative_humidity = humidity$relative_humidity,
        atmospheric_pressure = climate$B,
        extraterrestrial_horizontal_radiation = 9999,
        extraterrestrial_direct_normal_radiation = 9999,
        horizontal_infrared_radiation_intensity_from_sky = 5.6697e-8 * climate$T_SKY^4,
        global_horizontal_radiation = climate$HORI_TOTAL_RAD,
        direct_normal_radiation = direct_normal,
        diffuse_horizontal_radiation = climate$HORI_SCATTER_RAD,
        global_horizontal_illuminance = 999999,
        direct_normal_illuminance = 999999,
        diffuse_horizontal_illuminance = 999999,
        zenith_luminance = 9999,
        wind_direction = destep_epw_wind_direction(climate$WD),
        wind_speed = climate$WS,
        total_sky_cover = 99L,
        opaque_sky_cover = 99L,
        visibility = 9999,
        ceiling_height = 99999,
        present_weather_observation = 9L,
        present_weather_codes = "999999999",
        precipitable_water = 999,
        aerosol_optical_depth = 0.999,
        snow_depth = 999,
        days_since_last_snow = 99L,
        albedo = 999,
        liquid_precip_depth = 999,
        liquid_precip_rate = 99
    )
}

# Convert humidity ratio to the two EPW humidity fields using common Magnus
# saturation-pressure equations. DeST stores DAMP in g/kg dry air.
destep_epw_humidity <- function(dry_bulb, humidity_ratio_g_per_kg, pressure) {
    humidity_ratio <- humidity_ratio_g_per_kg / 1000
    vapor_pressure <- pressure * humidity_ratio / (0.621945 + humidity_ratio)
    saturation_pressure <- 611.2 * exp((17.67 * dry_bulb) / (dry_bulb + 243.5))
    relative_humidity <- pmin(100, pmax(0, 100 * vapor_pressure / saturation_pressure))

    gamma <- log(pmax(vapor_pressure, 1e-6) / 611.2)
    dew_point <- 243.5 * gamma / (17.67 - gamma)

    data.table::data.table(
        dew_point_temperature = dew_point,
        relative_humidity = relative_humidity
    )
}

# Derive direct normal radiation from DeST global and diffuse horizontal
# radiation.  The calculation uses the solar altitude at the middle of each
# hour; nighttime or very-low-sun beam components are written as zero.
destep_epw_direct_normal_radiation <- function(global_horizontal,
                                               diffuse_horizontal,
                                               datetime,
                                               latitude,
                                               longitude,
                                               time_zone) {
    beam_horizontal <- pmax(0, global_horizontal - diffuse_horizontal)
    cos_zenith <- destep_epw_cos_zenith(datetime, latitude, longitude, time_zone)
    ifelse(cos_zenith > 0.01 & beam_horizontal > 0, beam_horizontal / cos_zenith, 0)
}

# Compute the cosine of the solar zenith angle using a compact NOAA-style
# approximation.  This avoids a new dependency while keeping DNI deterministic.
destep_epw_cos_zenith <- function(datetime, latitude, longitude, time_zone) {
    day_of_year <- as.integer(format(datetime, "%j", tz = "UTC"))
    hour <- as.integer(format(datetime, "%H", tz = "UTC"))
    minute <- as.integer(format(datetime, "%M", tz = "UTC"))
    local_hour <- hour + minute / 60

    gamma <- 2 * pi / 365 * (day_of_year - 1 + (local_hour - 12) / 24)
    equation_of_time <- 229.18 * (
        0.000075 + 0.001868 * cos(gamma) - 0.032077 * sin(gamma) -
            0.014615 * cos(2 * gamma) - 0.040849 * sin(2 * gamma)
    )
    declination <- 0.006918 - 0.399912 * cos(gamma) +
        0.070257 * sin(gamma) - 0.006758 * cos(2 * gamma) +
        0.000907 * sin(2 * gamma) - 0.002697 * cos(3 * gamma) +
        0.00148 * sin(3 * gamma)

    time_offset <- equation_of_time + 4 * (longitude - 15 * time_zone)
    true_solar_time <- local_hour * 60 + time_offset
    hour_angle <- (true_solar_time / 4 - 180) * pi / 180
    latitude <- latitude * pi / 180

    sin(latitude) * sin(declination) +
        cos(latitude) * cos(declination) * cos(hour_angle)
}

# DeST stores wind direction as a 16-point compass code with 0 meaning calm.
# EPW stores degrees clockwise from north.
destep_epw_wind_direction <- function(wd) {
    ifelse(wd == 0, 0, (wd - 1) * 22.5)
}

# Decode the standard-meridian field stored in ENVIRONMENT.PROPERTY.  The low
# 16 bits store longitude in hundredths of a degree, using 18000-36000 for west.
destep_epw_time_zone <- function(environment) {
    property <- environment$PROPERTY[[1L]]
    if (!is.na(property)) {
        code <- bitwAnd(as.integer(property), 65535L)
        if (!is.na(code) && code >= 0L && code <= 36000L) {
            longitude <- if (code <= 18000L) code / 100 else -(36000L - code) / 100
            return(longitude / 15)
        }
    }

    round(environment$LONGITUDE[[1L]] / 15)
}

# Write a complete EPW file to a temporary path so to_epw() can return a normal
# eplusr::Epw object without using Epw$set(), which would require eplusr's
# optional 'units' dependency during R CMD check.
destep_write_epw <- function(path, weather, environment) {
    header <- c(
        sprintf(
            "LOCATION,%s,%s,%s,DeST CLIMATE_DATA,%s,%s,%s,%s,%s",
            destep_epw_header_value(environment$CITY_NAME, "DeST"),
            destep_epw_header_value(environment$PROVINCE, "NA"),
            destep_epw_header_value(environment$COUNTRY, "Unknown"),
            destep_epw_header_value(environment$CITY_ID, "000000"),
            destep_epw_header_value(environment$LATITUDE, 0),
            destep_epw_header_value(environment$LONGITUDE, 0),
            destep_epw_header_value(destep_epw_time_zone(environment), 0),
            destep_epw_header_value(environment$ELEVATION, 0)
        ),
        "DESIGN CONDITIONS,0",
        "TYPICAL/EXTREME PERIODS,0",
        "GROUND TEMPERATURES,0",
        "HOLIDAYS/DAYLIGHT SAVINGS,No,0,0,0",
        "COMMENTS 1,Generated by destep from DeST CLIMATE_DATA",
        "COMMENTS 2,Missing EPW-only fields use EPW missing codes or safe defaults",
        "DATA PERIODS,1,1,Data,Friday, 1/ 1,12/31"
    )
    dates <- seq(as.Date("1999-01-01"), as.Date("1999-12-31"), by = "day")
    month <- rep(as.integer(format(dates, "%m")), each = 24L)
    day <- rep(as.integer(format(dates, "%d")), each = 24L)
    hour <- rep(1:24, times = length(dates))
    data <- sprintf(
        paste(
            "1999,%d,%d,%d,0,%s,",
            "%.10g,%.10g,%.10g,%.10g,%.10g,%.10g,%.10g,%.10g,",
            "%.10g,%.10g,%.10g,%.10g,%.10g,%.10g,%.10g,%.10g,",
            "%.10g,%.10g,%.10g,%.10g,%d,%s,%.10g,%.10g,%.10g,%d,",
            "%.10g,%.10g,%.10g",
            sep = ""
        ),
        month, day, hour,
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

# Normalize LOCATION text fields because eplusr validates them as required
# non-empty EPW header fields.
destep_epw_location_value <- function(x, default) {
    x <- as.character(x[[1L]])
    if (is.na(x) || !nzchar(x)) default else x
}

# Header values are comma-delimited in EPW files, so text values are normalized
# before writing the LOCATION line.
destep_epw_header_value <- function(x, default) {
    x <- destep_epw_location_value(x, default)
    gsub(",", " ", x, fixed = TRUE)
}
