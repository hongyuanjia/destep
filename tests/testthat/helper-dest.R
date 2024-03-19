ensure_dest_test_file <- function() {
    p <- testthat::test_path("fixture", "CoA_Chongqin_2015.accdb")

    # directly return the path if the file exists
    if (file.exists(p)) return(p)

    # otherwise download a typical building model from the DeST website
    download_dest_model(
        "Commercial office A", "Chongqin", 2015,
        testthat::test_path("fixture")
    )
}
