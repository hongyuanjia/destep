# ENVIRONMENT -> Site:Location
# TODO: add time zone
destep_conv_location <- function(dest, ep) {
    loc <- DBI::dbGetQuery(dest,
        "SELECT
            ENVIRONMENT_ID       AS ID,
            NAME,
            round(LATITUDE, 2)  AS LATITUDE,
            round(LONGITUDE, 2) AS LONGITUDE,
            round(ELEVATION, 2) AS ELEVATION
        FROM ENVIRONMENT"
    )
    assert_unique_name(loc$NAME, "environment")
    data.table::setDT(loc)

    out <- destep_add(dest, ep,
        "Site:Location" := list(
            name      = loc$NAME,
            latitude  = loc$LATITUDE,
            longitude = loc$LONGITUDE,
            # TODO: handle time zone
            time_zone = NULL,
            elevation = loc$ELEVATION
        )
    )

    # always attach the table to the output in case it is useful later
    attr(out, "table") <- loc

    out
}
