is_integerish <- function(x) {
    all(!is.na(x)) && (is.integer(x) || (is.double(x) && all(x %% 1 == 0)))
}

is_character <- function(x) {
    all(!is.na(x)) && is.character(x)
}

utils::globalVariables(c(".SD", "NAME", "MATERIAL_NAME"))
