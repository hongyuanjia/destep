# Use the project's 9.0.1 reference-validation baseline. Only effective source
# features may raise it; transition cannot downgrade an unsupported feature.
conv__generation_version <- function(dest, target) {
    target <- numeric_version(as.character(target))
    if (target < "9.0.1") {
        abort(
            "destep uses EnergyPlus 9.0.1 as its minimum maintained generation baseline.",
            class = "destep_unsupported_target_version"
        )
    }
    moisture <- people__has_moisture(dest) || equipment__has_moisture(dest)
    if (moisture && target < "9.1") {
        abort(
            paste(
                "Nonzero people or equipment moisture requires EnergyPlus 9.1.0",
                "or newer for BeginZoneTimestepBeforeInitHeatBalance;",
                "transition cannot downgrade this source feature."
            ),
            class = "destep_unsupported_moisture_target"
        )
    }
    baseline <- if (moisture) {
        numeric_version("9.1.0")
    } else {
        numeric_version("9.0.1")
    }
    baseline
}

# Delegate release-specific object/field changes to eplusr. Its updater needs
# a saved input; use a disposable file without copying schedule CSVs. Detach
# its backing path afterwards without serializing the entire model again.
conv__transition <- function(model, target, verbose = FALSE) {
    if (model$version() == target) {
        return(model)
    }
    metadata <- attributes(model)
    metadata$class <- NULL
    path <- tempfile("destep-transition-", fileext = ".idf")
    on.exit(unlink(path), add = TRUE)
    model$save(path, overwrite = TRUE, copy_external = FALSE)
    transitioned <- if (verbose) {
        eplusr::transition(model, target)
    } else {
        eplusr::with_silent(eplusr::transition(model, target))
    }
    checkmate::assert_true(
        transitioned$version() == target,
        .var.name = "eplusr transition reached the requested version"
    )
    checkmate::assert_true(
        transitioned$is_valid(),
        .var.name = "transitioned EnergyPlus model"
    )
    result <- conv__detach_temporary_path(transitioned)
    for (name in names(metadata)) {
        attr(result, name) <- metadata[[name]]
    }
    result
}

# eplusr exposes no public path-reset method. Clear only the backing path via
# its existing private-environment accessor; IDD data, object identities and
# absolute external-file references remain intact, avoiding a costly reparse.
conv__detach_temporary_path <- function(model) {
    private <- eplusr::get_priv_env(model)
    private$m_path <- NULL
    model
}
