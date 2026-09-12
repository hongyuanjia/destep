# ENVIRONMENT -> Site:Location
location__convert <- function(dest, ep) {
    loc <- DBI::dbGetQuery(dest,
        "SELECT
            ENVIRONMENT_ID       AS ID,
            NAME,
            LATITUDE,
            LONGITUDE,
            ELEVATION,
            PROPERTY
        FROM ENVIRONMENT"
    )
    assert_unique_name(loc$NAME, "environment")
    data.table::setDT(loc)
    dt_force_numeric(
        loc,
        c("LATITUDE", "LONGITUDE", "ELEVATION", "PROPERTY")
    )

    # PROPERTY stores the standard meridian used by DeST weather calculations.
    time_zone <- epw__time_zone(loc)

    out <- conv__add(dest, ep,
        "Site:Location" := list(
            name      = loc$NAME,
            latitude  = loc$LATITUDE,
            longitude = loc$LONGITUDE,
            time_zone = time_zone,
            elevation = loc$ELEVATION
        )
    )

    # always attach the table to the output in case it is useful later
    attr(out, "table") <- loc

    out
}
