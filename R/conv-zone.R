# ROOM -> Zone
# ROOM$OF_STOREY -> STOREY$ID + MULTIPLE
# ROOM$OF_STORY -> ZoneList
destep_conv_zone <- function(dest, ep) {
    room <- DBI::dbGetQuery(dest,
        "SELECT
            R.ID                 AS ID,
            R.NAME               AS NAME,
            ROUND(R.VOLUME, 3)   AS VOLUME,
            ROUND(R.AREA, 3)     AS AREA,
            S.NAME               AS STOREY_NAME,
            S.MULTIPLE           AS STOREY_MULTIPLIER
        FROM ROOM R
        INNER JOIN STOREY S
        ON R.OF_STOREY = S.ID
        "
    )
    assert_unique_name(room$NAME, "room")
    data.table::setDT(room)

    #  construct ZoneList input
    zonelist <- lapply(room[, by = "STOREY_NAME",
        list(value = list(c(.BY$STOREY_NAME, NAME)))
    ]$value, as.list)

    # construct ZoneGroup input
    # STOREY$MULTIPLE -> ZoneGroup$'Zone List Multipler'
    # NOTE: here we use the same name for ZoneList and ZoneGroup the name field
    # in ZoneGroup does not have an \reference property and thus is not referred
    # by any other objects
    zonegroup <- room[, by = "STOREY_NAME",
        list(
            name = .BY$STOREY_NAME,
            zone_list_name = .BY$STOREY_NAME,
            zone_list_multiplier = STOREY_MULTIPLIER[[1L]]
        )
    ]

    out <- eval(as.call(c(
        destep_add, dest, ep,
        # Zone
        quote("Zone" := list(
            name = room$NAME,
            # NOTE: DeST use room types to refer to default loads and etc.
            # However, 'type' field is not used by EnergyPlus and is always set
            # to 1.
            type = NULL,
            # NOTE: we did not set zone multiplier here, but instead follow DeST
            #       convention, i.e., setting the multiplier in ZoneGroup
            multiplier = NULL,
            # STOREY$HEIGHT
            ceiling_height = room$HEIGHT,
            volume         = room$VOLUME,
            floor_area     = room$AREA
        )),

        # ZoneList
        lapply(zonelist, function(zl) bquote("ZoneList" := .(zl))),

        # ZoneGroup
        bquote(
            "ZoneGroup" := list(
                name = .(zonegroup$name),
                zone_list_name = .(zonegroup$zone_list_name),
                zone_list_multiplier = .(zonegroup$zone_list_multiplier)
            )
        )
    )))

    # always attach the table to the output in case it is useful later
    attr(out, "table") <- room

    out
}
