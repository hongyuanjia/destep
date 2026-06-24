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

    "ACTIVITY_LEVEL", "ACTIVITY_SCHEDULE_NAME", "BOUNDARY_OBJECT", "BOUNDARY",
    "CONSTRUCTION", "FRACTION_RADIANT", "FRACTION_REPLACEABLE", "ID",
    "KIND_ENCLOSURE", "LAYER_NO", "MATERIAL_NAME", "MATERIAL_ID", "NAME",
    "POINT_NO", "POINT_X", "POINT_Y", "POINT_Z", "ROOM", "ROOM_NAME",
    "SCHEDULE_NAME", "SENSIBLE_HEAT_FRACTION", "SIDE", "SOURCE_TABLE",
    "STOREY_MULTIPLIER", "SURFACE_NAME", "TYPE_SURFACE", "TYPE",
    "surface", "z"
))
