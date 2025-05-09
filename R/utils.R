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
    ".SD", "J", ":=", ".BY", # data.table

    "BOUNDARY_OBJECT", "BOUNDARY", "CONSTRUCTION", "KIND_ENCLOSURE", "LAYER_NO",
    "MATERIAL_NAME", "MATERIAL_ID", "NAME", "POINT_NO", "POINT_X", "POINT_Y",
    "POINT_Z", "ROOM", "STOREY_MULTIPLIER", "TYPE_SURFACE", "TYPE"
))
