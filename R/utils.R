is_integerish <- function(x) {
    all(!is.na(x)) && (is.integer(x) || (is.double(x) && all(x %% 1 == 0)))
}

is_character <- function(x) {
    all(!is.na(x)) && is.character(x)
}

utils::globalVariables(c(
    ".SD", "J", ":=", ".BY", # data.table

    "BOUNDARY_OBJECT", "BOUNDARY", "CONSTRUCTION", "KIND_ENCLOSURE", "LAYER_NO",
    "MATERIAL_NAME", "MATERIAL_ID", "NAME", "POINT_NO", "POINT_X", "POINT_Y",
    "POINT_Z", "ROOM", "STOREY_MULTIPLIER", "TYPE_SURFACE", "TYPE"
))
