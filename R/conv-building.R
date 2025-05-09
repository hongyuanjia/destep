# BUILDING -> Building
# TODO: one IDF per BUILDING?
destep_conv_building <- function(dest, ep, which = NULL) {
    bld <- DBI::dbGetQuery(dest, "SELECT BUILDING_ID AS ID, NAME FROM BUILDING")
    assert_unique_name(bld$NAME, "building")
    data.table::setDT(bld)

    if (!is.null(which)) {
        if (is_integerish(which)) {
            # check if the index is out of bound
            if (any(which < 1L) || any(which > nrow(bld))) {
                stop(sprintf(
                    "'which' out of bound. Should be in [1, %i], but found '%i'.",
                    nrow(bld), which
                ))
            }
            bld <- bld[unique(which)]
        } else if (is_character(which)) {
            # check if which is valid building name
            if (any(!which %in% bld$NAME)) {
                stop(sprintf(
                    "'%s' is not a valid building name in current DeST model. Available names: %s",
                    which, paste(unique(bld$NAME), collapse = ", ")
                ))
            }
            bld <- bld[J(unique(which)), on = "NAME"]
        } else {
            stop("'which' should be an integer or character vector.")
        }
    }

    out <- destep_add(dest, ep, "Building" := list(name = bld$NAME))

    # always attach the table to the output in case it is useful later
    attr(out, "table") <- bld

    out
}
