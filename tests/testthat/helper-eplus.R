ensure_empty_idf <- function() {
    eplusr::use_idd(23.1, "auto")
    eplusr::empty_idf(23.1)
}
