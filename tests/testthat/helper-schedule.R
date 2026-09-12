# Store DeST hourly schedule data in the BLOB shape that readBin() expects when
# conversion tests read SCHEDULE_YEAR.DATA from an in-memory SQLite fixture.
destep_test_schedule_blob <- function(values) {
    writeBin(as.double(values), raw(), size = 8L)
}
