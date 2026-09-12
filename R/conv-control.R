# Resolve the room-function control schedules that DeST Calload serializes for
# each room. ROOM_GROUP retains zone membership and the air-conditioned flag,
# while ROOM.TYPE selects the effective ROOM_TYPE_DATA schedule definitions.
control__room_table <- function(dest) {
    control <- DBI::dbGetQuery(
        dest,
        "
        SELECT
            R.ID AS ROOM_ID,
            R.NAME AS ROOM_NAME,
            R.TYPE AS ROOM_TYPE_ID,
            T.ID AS ROOM_TYPE_DATA_ID,
            T.NAME AS ROOM_TYPE_NAME,
            R.OF_ROOM_GROUP,
            G.ROOM_GROUP_ID,
            G.NAME AS ROOM_GROUP_NAME,
            G.OF_AC_SYS,
            G.IS_AC_ROOM,
            T.AC_SCHEDULE_ID,
            S_AC.NAME AS AC_SCHEDULE_NAME,
            T.SET_T_MIN_SCHEDULE,
            S_T_MIN.NAME AS HEATING_SCHEDULE_NAME,
            T.SET_T_MAX_SCHEDULE,
            S_T_MAX.NAME AS COOLING_SCHEDULE_NAME,
            T.SET_RH_MIN_SCHEDULE,
            S_RH_MIN.NAME AS HUMIDIFYING_SCHEDULE_NAME,
            T.SET_RH_MAX_SCHEDULE,
            S_RH_MAX.NAME AS DEHUMIDIFYING_SCHEDULE_NAME,
            T.AC_T_MIN_SCHEDULE,
            T.AC_T_MAX_SCHEDULE,
            G.AC_SCHEDULE_ID AS ROOM_GROUP_AC_SCHEDULE_ID,
            G.SET_T_MIN_SCHEDULE AS ROOM_GROUP_SET_T_MIN_SCHEDULE,
            G.SET_T_MAX_SCHEDULE AS ROOM_GROUP_SET_T_MAX_SCHEDULE,
            G.SET_RH_MIN_SCHEDULE AS ROOM_GROUP_SET_RH_MIN_SCHEDULE,
            G.SET_RH_MAX_SCHEDULE AS ROOM_GROUP_SET_RH_MAX_SCHEDULE,
            G.AC_T_MIN_SCHEDULE AS ROOM_GROUP_AC_T_MIN_SCHEDULE,
            G.AC_T_MAX_SCHEDULE AS ROOM_GROUP_AC_T_MAX_SCHEDULE
        FROM ROOM R
        LEFT JOIN ROOM_GROUP G
        ON R.OF_ROOM_GROUP = G.ROOM_GROUP_ID
        LEFT JOIN ROOM_TYPE_DATA T
        ON R.TYPE = T.ID
        LEFT JOIN SCHEDULE_YEAR S_AC
        ON T.AC_SCHEDULE_ID = S_AC.SCHEDULE_ID
        LEFT JOIN SCHEDULE_YEAR S_T_MIN
        ON T.SET_T_MIN_SCHEDULE = S_T_MIN.SCHEDULE_ID
        LEFT JOIN SCHEDULE_YEAR S_T_MAX
        ON T.SET_T_MAX_SCHEDULE = S_T_MAX.SCHEDULE_ID
        LEFT JOIN SCHEDULE_YEAR S_RH_MIN
        ON T.SET_RH_MIN_SCHEDULE = S_RH_MIN.SCHEDULE_ID
        LEFT JOIN SCHEDULE_YEAR S_RH_MAX
        ON T.SET_RH_MAX_SCHEDULE = S_RH_MAX.SCHEDULE_ID
        ORDER BY R.ID
        "
    )
    data.table::setDT(control)
    control[, HUMIDIFYING_SCHEDULE_NAME :=
        schedule__relative_humidity_reference_names(
            dest,
            SET_RH_MIN_SCHEDULE,
            HUMIDIFYING_SCHEDULE_NAME
        )]
    control[, DEHUMIDIFYING_SCHEDULE_NAME :=
        schedule__relative_humidity_reference_names(
            dest,
            SET_RH_MAX_SCHEDULE,
            DEHUMIDIFYING_SCHEDULE_NAME
        )]
    control
}
