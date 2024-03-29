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

utils::globalVariables(c(
    ".SD", "J", ":=", ".BY", # data.table

    "BOUNDARY_OBJECT", "BOUNDARY", "CONSTRUCTION", "KIND_ENCLOSURE", "LAYER_NO",
    "MATERIAL_NAME", "MATERIAL_ID", "NAME", "POINT_NO", "POINT_X", "POINT_Y",
    "POINT_Z", "ROOM", "STOREY_MULTIPLIER", "TYPE_SURFACE", "TYPE"
))
