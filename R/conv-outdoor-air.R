# OCCUPANT_GAINS minimum fresh-air requirement -> DesignSpecification:OutdoorAir.
# DeST stores the requirement as m3/h per person; EnergyPlus expects m3/s per
# person for the Flow/Person method, so values are converted once here and then
# reused by zone equipment converters such as IdealLoads.
destep_conv_design_specification_outdoor_air <- function(dest, ep) {
    outdoor_air <- destep_occupant_outdoor_air_table(dest)
    if (nrow(outdoor_air) == 0L) return(NULL)

    values <- lapply(seq_len(nrow(outdoor_air)), function(i) {
        destep_outdoor_air_value(outdoor_air, i)
    })

    out <- eval(as.call(c(
        destep_add, dest, ep,
        lapply(values, function(val) bquote("DesignSpecification:OutdoorAir" := .(val)))
    )))
    attr(out, "table") <- outdoor_air

    out
}

# Build one row per room for positive occupant outdoor-air requirements. If a
# room has multiple occupant gain rows, their fresh-air requirements must agree;
# otherwise the model needs a more explicit conflict-resolution rule.
destep_occupant_outdoor_air_table <- function(dest) {
    cols <- c(
        "ROOM_ID", "ROOM_NAME", "MIN_REQUIRE_FRESH_AIR",
        "OUTDOOR_AIR_FLOW_PER_PERSON", "ENERGYPLUS_OUTDOOR_AIR_NAME"
    )
    empty <- data.table::data.table(
        ROOM_ID = integer(),
        ROOM_NAME = character(),
        MIN_REQUIRE_FRESH_AIR = numeric(),
        OUTDOOR_AIR_FLOW_PER_PERSON = numeric(),
        ENERGYPLUS_OUTDOOR_AIR_NAME = character()
    )
    data.table::setcolorder(empty, cols)

    if (!destep_has_rows(dest, "ROOM") || !destep_has_rows(dest, "OCCUPANT_GAINS")) {
        return(empty)
    }

    outdoor_air <- DBI::dbGetQuery(
        dest,
        "
        SELECT
            O.GAIN_ID,
            O.OF_ROOM AS ROOM_ID,
            R.NAME AS ROOM_NAME,
            O.MIN_REQUIRE_FRESH_AIR
        FROM OCCUPANT_GAINS O
        LEFT JOIN ROOM R
        ON O.OF_ROOM = R.ID
        WHERE O.MIN_REQUIRE_FRESH_AIR IS NOT NULL
            AND O.MIN_REQUIRE_FRESH_AIR > 0
        ORDER BY O.OF_ROOM, O.GAIN_ID
        "
    )
    data.table::setDT(outdoor_air)
    if (nrow(outdoor_air) == 0L) return(empty)

    destep_assert_outdoor_air_rooms(outdoor_air)
    destep_assert_outdoor_air_consistency(outdoor_air)

    outdoor_air <- unique(
        outdoor_air[, .(ROOM_ID, ROOM_NAME, MIN_REQUIRE_FRESH_AIR)]
    )
    destep_force_numeric(outdoor_air, "MIN_REQUIRE_FRESH_AIR")

    # Convert from DeST's m3/h-person to EnergyPlus' m3/s-person.
    data.table::set(
        outdoor_air, NULL, "OUTDOOR_AIR_FLOW_PER_PERSON",
        outdoor_air$MIN_REQUIRE_FRESH_AIR / 3600
    )
    data.table::set(
        outdoor_air, NULL, "ENERGYPLUS_OUTDOOR_AIR_NAME",
        make_unique_name(paste(outdoor_air$ROOM_NAME, "Outdoor Air"))
    )
    data.table::setcolorder(outdoor_air, cols)

    outdoor_air
}

# Positive fresh-air requirements must point to a known room; otherwise the
# generated DesignSpecification object would not have a usable zone context.
destep_assert_outdoor_air_rooms <- function(outdoor_air) {
    unresolved <- is.na(outdoor_air$ROOM_NAME)
    if (!any(unresolved)) {
        return(invisible(NULL))
    }

    detail <- paste(sprintf(
        "GAIN_ID=%s OF_ROOM=%s",
        outdoor_air$GAIN_ID[unresolved],
        outdoor_air$ROOM_ID[unresolved]
    ), collapse = "; ")

    stop(sprintf(
        "Cannot resolve OCCUPANT_GAINS outdoor-air room reference(s): %s",
        detail
    ), call. = FALSE)
}

# Each room is mapped to one DesignSpecification:OutdoorAir object in this
# converter, so conflicting per-person requirements are treated as invalid.
destep_assert_outdoor_air_consistency <- function(outdoor_air) {
    conflicts <- outdoor_air[, .(
        N = data.table::uniqueN(MIN_REQUIRE_FRESH_AIR),
        VALUES = paste(sort(unique(MIN_REQUIRE_FRESH_AIR)), collapse = ", ")
    ), by = .(ROOM_ID, ROOM_NAME)][N > 1L]
    if (nrow(conflicts) == 0L) {
        return(invisible(NULL))
    }

    detail <- paste(sprintf(
        "%s: %s",
        conflicts$ROOM_NAME,
        conflicts$VALUES
    ), collapse = "; ")

    stop(sprintf(
        "Conflicting OCCUPANT_GAINS.MIN_REQUIRE_FRESH_AIR values by room: %s",
        detail
    ), call. = FALSE)
}

# Build one DesignSpecification:OutdoorAir value list using the Flow/Person
# method so EnergyPlus multiplies the requirement by zone occupancy.
destep_outdoor_air_value <- function(outdoor_air, i) {
    list(
        name = outdoor_air$ENERGYPLUS_OUTDOOR_AIR_NAME[[i]],
        outdoor_air_method = "Flow/Person",
        outdoor_air_flow_per_person = outdoor_air$OUTDOOR_AIR_FLOW_PER_PERSON[[i]],
        outdoor_air_flow_per_zone_floor_area = 0,
        outdoor_air_flow_per_zone = 0,
        outdoor_air_flow_air_changes_per_hour = 0,
        outdoor_air_schedule_name = NULL
    )
}
