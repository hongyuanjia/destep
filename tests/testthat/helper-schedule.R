# Store DeST hourly schedule data in the BLOB shape that readBin() expects when
# conversion tests read SCHEDULE_YEAR.DATA from an in-memory SQLite fixture.
destep_test_schedule_blob <- function(values) {
    writeBin(as.double(values), raw(), size = 8L, endian = "little")
}

# Create a minimal source database without imposing weekday assumptions.
destep_test_schedule_db <- function(values) {
    dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbWriteTable(
        dest,
        "SCHEDULE_YEAR",
        data.frame(
            SCHEDULE_ID = seq_along(values),
            NAME = paste0("source ", seq_along(values)),
            TYPE = rep(4L, length(values)),
            DATA = I(lapply(values, destep_test_schedule_blob))
        )
    )
    DBI::dbWriteTable(
        dest,
        "SCHEDULE_USAGE",
        data.frame(SCHEDULE_ID = seq_along(values))
    )
    dest
}

# Independently interpret the generated Compact tokens onto a 365 x 24 calendar.
# Parsing is stateful; preallocation avoids growing the reconstructed output.
destep_test_expand_compact <- function(fields) {
    output <- matrix(NA_real_, nrow = 24L, ncol = 365L)
    first_day <- 1L
    last_day <- 0L
    first_hour <- 1L
    i <- 3L
    while (i <= length(fields)) {
        field <- fields[[i]]
        if (startsWith(field, "Through:")) {
            first_day <- last_day + 1L
            date <- as.Date(paste0("2001/", sub("Through: ", "", field)))
            last_day <- as.integer(date - as.Date("2001-01-01")) + 1L
            first_hour <- 1L
        } else if (startsWith(field, "Until:")) {
            last_hour <- as.integer(substr(field, 8L, 9L))
            output[
                first_hour:last_hour,
                first_day:last_day
            ] <- as.numeric(fields[[i + 1L]])
            first_hour <- last_hour + 1L
            i <- i + 1L
        }
        i <- i + 1L
    }
    as.vector(output)
}
