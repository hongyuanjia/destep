#' Download a typical building model from the DeST website
#'
#' @details
#' The downloaded `7z` file will be unachieved using the `{archive}` package.
#'
#' @param building_type \[character\] Building type to download.
#'
#' @param location \[character\] Location of the building.
#'
#' @param year \[integer\] Year of the building model.
#'
#' @param dir \[string\] Directory to save the file. Default is the current
#'        workding directory.
#'
#' @note Information about all available models can be retrieved by sending a
#'       `GET` request to `https://svr.dest.net.cn/api/v1/all_model_names`.
#'
#' @return \[character\] Path to the downloaded file.
#'
#' \dontrun{
#' download_typical_dest("Commerical office A", "Chongqin", 2015)
#' }
#'
#' @export
download_dest_model <- function(building_type, location, year, dir = ".") {
    if (!requireNamespace("httr2", quietly = TRUE)) {
        stop(paste(
            "'download_dest_model()' relies on the 'httr2' package to send HTTP",
            "requests to the DeST website. Please add it to your library with",
            "'install.packages(\"httr2\")' and try agian."
        ))
    }

    if (!requireNamespace("archive", quietly = TRUE)) {
        stop(paste(
            "'download_dest_model()' relies on the 'archive' package to extract",
            "DeST model files from the downloaded 7z compression file. Please",
            "add it to your library with 'install.packages(\"archive\")' and",
            "try agian."
        ))
    }

    req <- httr2::request("https://svr.dest.net.cn/api/v1/load_model_file")
    # should send a OPTIONS request first
    req <- httr2::req_method(req, "OPTIONS")

    req <- httr2::req_headers(req,
        `Accept`                         = "*/*",
        `Access-Control-Request-Headers` = "content-type",
        `Access-Control-Request-Method`  = "POST",
        `Connection`                     = "keep-alive",
        `DNT`                            = "1",
        `HOST`                           = "svr.dest.net.cn",
        `Origin`                         = "https://cal.dest.net.cn",
        `Referer`                        = "https://cal.dest.net.cn/",
        `Sec-Fetch-Dest`                 = "empty",
        `Sec-Fetch-Mode`                 = "cors",
        `Sec-Fetch-Site`                 = "same-site",
        `Sec-GPC`                        = "1"
    )
    # perform the OPTIONS request
    httr2::req_perform(req)

    # now send the POST request
    req <- httr2::req_method(req, "POST")
    req <- httr2::req_body_json(req, list(
        data = list(
            building_type = building_type,
            location      = location,
            year          = year
        )
    ))
    req <- httr2::req_headers(req,
        `Accept`          = "application/json, text/plain, */*",
        `Content-Type`    = "application/json;charset=utf-8",
        `Connection`      = "keep-alive",
        `Host`            = "svr.dest.net.cn",
        `Origin`          = "https://cal.dest.net.cn",
        `Referer`         = "https://cal.dest.net.cn/",
        `Sec-Fetch-Dest`  = "empty",
        `Sec-Fetch-Mode`  = "cors",
        `Sec-Fetch-Site`  = "same-site",
        `DNT`             = "1",
        `Sec-GPC`         = "1"
    )

    # perform the POST request
    resp <- httr2::req_perform(req)

    # create the directory if it does not exist
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

    # extract the filename from the Content-Disposition header
    zipname <- sub('.+filename="(.+)"', "\\1",
        httr2::resp_header(resp, "Content-Disposition")
    )
    zippath <- file.path(dir, zipname)

    # save the response body to a file
    writeBin(httr2::resp_body_raw(resp), zippath)

    # get the actual DeST model name
    filename <- grep("\\.accdb$", archive::archive(zippath)$path, value = TRUE)
    # just in case
    if (length(filename) == 0) {
        stop(paste(
            "No '.accdb' file found in the downloaded archive.",
            sprintf("Please manually check the file at '%s'.", normalizePath(zippath))
        ))
    }

    # unarchive the file
    archive::archive_extract(zippath, dir, filename)

    # remove the downloaded archive
    unlink(zippath)

    # return the path to the extracted file
    normalizePath(file.path(dir, filename))
}
