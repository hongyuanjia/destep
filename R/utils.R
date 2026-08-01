is_scalar <- function(x) {
    length(x) == 1L
}

is_integerish <- function(x) {
    (is.integer(x) || (is.double(x) && all(x %% 1 == 0))) && all(!is.na(x))
}

is_character <- function(x) {
    is.character(x) && all(!is.na(x))
}

is_string <- function(x) {
    is_scalar(x) && is_character(x)
}

is_flag <- function(x) {
    is_scalar(x) && is.logical(x) && !is.na(x)
}

abort <- function(message, class = NULL, call = NULL, ...) {
    ori <- getOption("warning.length")
    options(warning.length = 8170L)
    on.exit(options(warning.length = ori), add = TRUE)

    stop(errorCondition(message, ..., class = class, call = call))
}

warn <- function(message, class = NULL, call = NULL, ...) {
    ori <- getOption("warning.length")
    options(warning.length = 8170L)
    on.exit(options(warning.length = ori), add = TRUE)

    warning(warningCondition(message, ..., class = class, call = call))
}

utils::globalVariables(c(
    ".", ".SD", ".N", "J", ":=", ".BY", # data.table

    "ACTIVITY_LEVEL", "ACTIVITY_SCHEDULE_NAME", "AZIMUTH", "BOUNDARY_MODE",
    "BOUNDARY_OBJECT", "BOUNDARY", "CONSTRUCTION", "COORDINATE_KEY",
    "DETAILED_CONSTRUCTION_ID", "EDGE", "END", "END_X", "END_Y", "ERROR",
    "FALLBACK_REASON", "FRACTION_RADIANT", "GROUP",
    "FRACTION_REPLACEABLE", "ID", "KIND_ENCLOSURE", "LAYER_NO",
    "INTERZONE", "i.CEILING_NAME", "i.FLOOR_NAME", "i.SNAP_X", "i.SNAP_Y",
    "i.SNAP_Z", "JUNCTION_KEY", "K", "LIGHT_TRANS_RATIO", "MATERIAL_NAME",
    "MATERIAL_ID", "NAME",
    "ORIGINAL_NAME", "OUTPUT_ID",
    "OUTPUT_PART_ID", "PART", "PART_AREA", "PART_COUNT", "PIECE", "PLANE",
    "PEER_OUTPUT_ID",
    "PRESERVE_BASE_NAME", "PROTECTED",
    "MIN_REQUIRE_FRESH_AIR", "N", "POINT_NO", "POINT_X", "POINT_Y",
    "POINT_Z", "ROOM", "ROOM_ID", "ROOM_NAME", "ROOMS", "SCHEDULE_NAME",
    "SC", "SENSIBLE_HEAT_FRACTION", "SHGC", "SIDE", "SIDE1_AZIMUTH",
    "SIDE1_SURFACE_ID",
    "SIDE1_SURFACE_NAME", "SIDE1_SURFACE_TYPE", "SIDE1_TILT",
    "SIDE2_AZIMUTH", "SIDE2_SURFACE_ID", "SIDE2_SURFACE_NAME",
    "SIDE2_SURFACE_TYPE", "SIDE2_TILT", "SOURCE_AREA", "SOURCE_ID",
    "SOURCE_JUNCTION_OUTPUT_ID", "SOURCE_NAME", "SOURCE_TABLE", "START",
    "START_X", "START_Y", "SUBPART", "SIMPLE_GLAZING_NAME", "STOREY_ID",
    "STOREY_MULTIPLIER", "SURFACE_NAME", "REBUILT_AREA", "TYPICAL_PAIR_ID",
    "TYPICAL_PART", "TYPICAL_PART_COUNT",
    "TYPE_CONSTRUCTION_NAME", "TYPE_DATA_VALID", "TYPE_ID", "TYPE_NAME",
    "TYPE_RECORD_FOUND", "TYPE_SURFACE", "TYPE", "WINDOW_ID", "surface", "z"
))
