# ENVIRONMENT -> Site:Location
# TODO: add time zone
location__convert <- function(dest, ep) {
    loc <- DBI::dbGetQuery(dest,
        "SELECT
            ENVIRONMENT_ID       AS ID,
            NAME,
            round(LATITUDE, 2)  AS LATITUDE,
            round(LONGITUDE, 2) AS LONGITUDE,
            round(ELEVATION, 2) AS ELEVATION
        FROM ENVIRONMENT"
    )
    name__assert_unique(loc$NAME, "environment")
    data.table::setDT(loc)

    out <- conv__add(dest, ep,
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
