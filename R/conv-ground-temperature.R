# GROUND_DATA -> Site:GroundTemperature:BuildingSurface.
# EnergyPlus uses this object for building surfaces whose outside boundary
# condition is Ground, so DeST's hourly user-defined ground temperatures are
# reduced to the 12 monthly values required by the IDD.
destep_conv_ground_temperature <- function(dest, ep) {
    if (!destep_has_rows(dest, "GROUND_DATA")) return(NULL)

    ground <- destep_ground_temperature_table(dest)
    monthly <- destep_monthly_ground_temperature(ground)

    out <- destep_add(
        dest, ep,
        "Site:GroundTemperature:BuildingSurface" :=
            destep_ground_temperature_value(monthly)
    )
    attr(out, "table") <- monthly

    out
}

# Select one GROUND_DATA series and return the validated hourly table used by
# the converter.  The selection mirrors the DeST library path when available
# and keeps the unique-ID fallback explicit for models like the current fixture.
destep_ground_temperature_table <- function(dest) {
    ground_id <- destep_select_ground_data_id(dest)
    if (is.null(ground_id)) return(data.table::data.table())

    ground <- DBI::dbGetQuery(
        dest,
        sprintf(
            "
            SELECT
                ID,
                HOUR,
                T
            FROM GROUND_DATA
            WHERE ID = %s
            ORDER BY HOUR
            ",
            DBI::dbQuoteLiteral(dest, ground_id)
        )
    )
    data.table::setDT(ground)
    destep_force_numeric(ground, c("ID", "HOUR", "T"))
    destep_validate_ground_temperature_table(ground, ground_id)

    ground
}

# Resolve the ground-temperature data set from ENVIRONMENT/SYS_CITY first.
# If that bridge is absent or points outside GROUND_DATA, a single available
# GROUND_DATA.ID is safe; multiple IDs need a deliberate selection rule.
destep_select_ground_data_id <- function(dest) {
    resolved <- destep_resolve_city_ground_data_ids(dest)
    if (length(resolved) == 1L) return(resolved[[1L]])
    if (length(resolved) > 1L) {
        stop(sprintf(
            "Multiple GROUND_DATA IDs are referenced by ENVIRONMENT/SYS_CITY: %s",
            paste(resolved, collapse = ", ")
        ), call. = FALSE)
    }

    ids <- DBI::dbGetQuery(
        dest,
        "SELECT DISTINCT ID FROM GROUND_DATA ORDER BY ID"
    )$ID
    ids <- ids[!is.na(ids)]
    if (length(ids) == 0L) return(NULL)
    if (length(ids) == 1L) return(ids[[1L]])

    stop(sprintf(
        paste(
            "Cannot choose GROUND_DATA ID;",
            "multiple IDs are present and ENVIRONMENT/SYS_CITY did not select one: %s"
        ),
        paste(ids, collapse = ", ")
    ), call. = FALSE)
}

# Keep the ENVIRONMENT/SYS_CITY bridge optional because ad hoc fixtures and some
# DeST exports may carry GROUND_DATA without a resolvable city-library row.
destep_resolve_city_ground_data_ids <- function(dest) {
    if (!all(c("ENVIRONMENT", "SYS_CITY", "GROUND_DATA") %in% DBI::dbListTables(dest))) {
        return(numeric())
    }
    if (!destep_table_has_fields(dest, "ENVIRONMENT", "CITY_ID") ||
        !destep_table_has_fields(dest, "SYS_CITY", c("CITY_ID", "GROUND_ID"))) {
        return(numeric())
    }

    ids <- DBI::dbGetQuery(
        dest,
        "
        SELECT DISTINCT C.GROUND_ID AS ID
        FROM ENVIRONMENT E
        INNER JOIN SYS_CITY C
        ON E.CITY_ID = C.CITY_ID
        INNER JOIN GROUND_DATA G
        ON C.GROUND_ID = G.ID
        WHERE C.GROUND_ID IS NOT NULL
        ORDER BY C.GROUND_ID
        "
    )$ID

    ids[!is.na(ids)]
}

# Check a table's columns before running optional bridge SQL.  This avoids
# turning small unit-test fixtures into schema-completeness tests.
destep_table_has_fields <- function(dest, table, fields) {
    all(fields %in% DBI::dbListFields(dest, table))
}

# A BuildingSurface ground-temperature object has no room for gaps or duplicate
# hours, so the selected DeST series must be exactly one non-leap 8760-hour year.
destep_validate_ground_temperature_table <- function(ground, ground_id) {
    issues <- character()
    hour <- ground$HOUR

    if (nrow(ground) != 8760L) {
        issues <- c(issues, sprintf(
            "expected 8760 rows but found %i",
            nrow(ground)
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
    if (anyNA(ground$T)) {
        issues <- c(issues, "T contains missing values")
    }

    if (length(issues)) {
        stop(sprintf(
            "Invalid GROUND_DATA series for ID %s: %s",
            ground_id,
            paste(issues, collapse = "; ")
        ), call. = FALSE)
    }

    invisible(ground)
}

# Show enough hour IDs for a useful error while keeping long validation messages
# readable.
destep_format_integer_sample <- function(x, n = 10L) {
    x <- sort(unique(as.integer(x)))
    out <- paste(utils::head(x, n), collapse = ", ")
    if (length(x) > n) out <- paste0(out, ", ...")
    out
}

# Aggregate the validated hourly series using the standard non-leap calendar
# implied by DeST's HOUR = 0:8759 convention.
destep_monthly_ground_temperature <- function(ground) {
    month_hours <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L) * 24L
    month <- rep(seq_along(month_hours), month_hours)

    monthly <- data.table::data.table(
        GROUND_DATA_ID = unique(ground$ID),
        MONTH = seq_along(month_hours),
        GROUND_TEMPERATURE = as.numeric(tapply(ground$T, month, mean))
    )
    monthly
}

# Build the exact EnergyPlus field list for Site:GroundTemperature:BuildingSurface.
destep_ground_temperature_value <- function(monthly) {
    values <- as.list(monthly$GROUND_TEMPERATURE)
    names(values) <- paste0(
        tolower(month.name),
        "_ground_temperature"
    )
    values
}
