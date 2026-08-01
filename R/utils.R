is_scalar <- function(x) {
    length(x) == 1L
}

is_integerish <- function(x) {
    (is.integer(x) || (is.double(x) && all(x %% 1 == 0))) && all(!is.na(x))
}

is_character <- function(x) {
    is.character(x) && all(!is.na(x))
}

is_string <- function(x) {
    is_scalar(x) && is_character(x)
}

is_flag <- function(x) {
    is_scalar(x) && is.logical(x) && !is.na(x)
}

# Test whether a DeST table exists and contains at least one row without
# exposing SQL identifier construction to individual converters.
db__has_rows <- function(dest, table) {
    if (!table %in% DBI::dbListTables(dest)) return(FALSE)
    table <- as.character(DBI::dbQuoteIdentifier(dest, table))
    DBI::dbGetQuery(
        dest, paste0("SELECT COUNT(*) AS N FROM ", table)
    )$N[[1L]] > 0L
}

# Check optional DeST schema fields before a converter builds dependent SQL.
db__has_fields <- function(dest, table, fields) {
    all(fields %in% DBI::dbListFields(dest, table))
}

# Normalize driver-dependent numeric column types in place for data.table
# conversion pipelines.
data__force_numeric <- function(dt, cols) {
    for (col in cols) {
        data.table::set(dt, NULL, col, as.numeric(dt[[col]]))
    }
    invisible(dt)
}

# Reject duplicate EnergyPlus object names before object assembly obscures the
# originating DeST records.
name__assert_unique <- function(names, type) {
    if (anyDuplicated(names)) {
        stop(sprintf(
            "Duplicated %s names found: [%s]. This should already be handled when updating the names.",
            type, paste(unique(names[duplicated(names)]), collapse = ", ")
        ))
    }
}

# Add stable ordinal suffixes to repeated names while preserving input order.
name__make_unique <- function(name) {
    spl_name <- collapse::gsplit(name, name)
    spl_name <- .mapply(
        function(name, len) if (len == 1L) name else sprintf("%s (%d)", name, seq_len(len)),
        list(name = spl_name, len = collapse::vlengths(spl_name)),
        NULL
    )
    collapse::greorder(list__flatten(spl_name), name)
}

# Flatten list values with package-wide defaults that avoid accidental names.
list__flatten <- function(lst, recursive = FALSE, use.names = FALSE) {
    unlist(lst, recursive = recursive, use.names = use.names)
}

# Combine partial EnergyPlus expansion results and rebase their object ids so
# independently generated sections can be appended without collisions.
conv__combine_outputs <- function(outputs, table = NULL) {
    outputs <- Filter(Negate(is.null), outputs)
    if (length(outputs) == 0L) return(NULL)

    num_obj <- 0L
    for (i in seq_along(outputs)) {
        data.table::set(outputs[[i]]$object, NULL, "rleid", outputs[[i]]$object$rleid + num_obj)
        data.table::set(outputs[[i]]$value, NULL, "rleid", outputs[[i]]$value$rleid + num_obj)
        num_obj <- max(outputs[[i]]$object$rleid)
    }

    out <- list(
        object = data.table::rbindlist(lapply(outputs, .subset2, "object")),
        value = data.table::rbindlist(lapply(outputs, .subset2, "value"))
    )

    if (is.null(table)) {
        table <- data.table::rbindlist(
            lapply(names(outputs), function(name) {
                tbl <- attr(outputs[[name]], "table")
                if (is.null(tbl)) return(NULL)
                data.table::set(data.table::copy(tbl), NULL, "SOURCE_TABLE", name)
            }),
            fill = TRUE
        )
    }
    # Preserve the source snapshot for diagnostics and downstream converters.
    attr(out, "table") <- table
    out
}

abort <- function(message, class = NULL, call = NULL, ...) {
    ori <- getOption("warning.length")
    options(warning.length = 8170L)
    on.exit(options(warning.length = ori), add = TRUE)

    stop(errorCondition(message, ..., class = class, call = call))
}

warn <- function(message, class = NULL, call = NULL, ...) {
    ori <- getOption("warning.length")
    options(warning.length = 8170L)
    on.exit(options(warning.length = ori), add = TRUE)

    warning(warningCondition(message, ..., class = class, call = call))
}

utils::globalVariables(c(
    ".", ".SD", ".N", "J", ":=", ".BY", # data.table

    "ACTIVITY_LEVEL", "ACTIVITY_SCHEDULE_NAME", "AZIMUTH", "BOUNDARY_MODE",
    "BOUNDARY_OBJECT", "BOUNDARY", "CONSTRUCTION", "COORDINATE_KEY",
    "DETAILED_CONSTRUCTION_ID", "EDGE", "END", "END_X", "END_Y", "ERROR",
    "FALLBACK_REASON", "FRACTION_RADIANT", "GROUP",
    "FRACTION_REPLACEABLE", "ID", "KIND_ENCLOSURE", "LAYER_NO",
    "INTERZONE", "i.CEILING_NAME", "i.FLOOR_NAME", "i.SNAP_X", "i.SNAP_Y",
    "i.SNAP_Z", "JUNCTION_KEY", "K", "LIGHT_TRANS_RATIO", "MATERIAL_NAME",
    "MATERIAL_ID", "NAME",
    "ORIGINAL_NAME", "OUTPUT_ID",
    "OUTPUT_PART_ID", "PART", "PART_AREA", "PART_COUNT", "PIECE", "PLANE",
    "PEER_OUTPUT_ID",
    "PRESERVE_BASE_NAME", "PROTECTED",
    "MIN_REQUIRE_FRESH_AIR", "N", "POINT_NO", "POINT_X", "POINT_Y",
    "POINT_Z", "ROOM", "ROOM_ID", "ROOM_NAME", "ROOMS", "SCHEDULE_NAME",
    "SC", "SENSIBLE_HEAT_FRACTION", "SHGC", "SIDE", "SIDE1_AZIMUTH",
    "SIDE1_SURFACE_ID",
    "SIDE1_SURFACE_NAME", "SIDE1_SURFACE_TYPE", "SIDE1_TILT",
    "SIDE2_AZIMUTH", "SIDE2_SURFACE_ID", "SIDE2_SURFACE_NAME",
    "SIDE2_SURFACE_TYPE", "SIDE2_TILT", "SOURCE_AREA", "SOURCE_ID",
    "SOURCE_JUNCTION_OUTPUT_ID", "SOURCE_NAME", "SOURCE_TABLE", "START",
    "START_X", "START_Y", "SUBPART", "SIMPLE_GLAZING_NAME", "STOREY_ID",
    "STOREY_MULTIPLIER", "SURFACE_NAME", "REBUILT_AREA", "TYPICAL_PAIR_ID",
    "TYPICAL_PART", "TYPICAL_PART_COUNT",
    "TYPE_CONSTRUCTION_NAME", "TYPE_DATA_VALID", "TYPE_ID", "TYPE_NAME",
    "TYPE_RECORD_FOUND", "TYPE_SURFACE", "TYPE", "WINDOW_ID", "surface", "z"
))
