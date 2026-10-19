# Resolve one incident angle of the recovered positive-TYPE N2 optical stack.
# Angles are radians; the native grazing branch has no absorbed solar energy.
window__n2_angle <- function(parameter, angle = 0) {
    if (abs(angle) >= pi / 2 - 1e-6) {
        return(c(tau = 0, rho = 1))
    }
    index <- c(1, 1.526, 1, parameter, 1.526, 1)
    theta <- asin(sin(angle) / index)
    attenuation <- exp(-c(0, .0588, 0, 0, .0588, 0) / cos(theta))
    # DeST uses normal Fresnel reflection below 30 degrees, while retaining
    # the refracted angle in attenuation. Do not replace this with EP's fit.
    reflection <- if (abs(angle) < pi / 6) {
        ((index[-6L] - index[-1L]) / (index[-6L] + index[-1L]))^2
    } else {
        difference <- theta[-6L] - theta[-1L]
        total <- theta[-6L] + theta[-1L]
        .5 *
            ((sin(difference) / sin(total))^2 +
                (tan(difference) / tan(total))^2)
    }
    effective <- numeric(6L)
    transmission <- numeric(5L)
    # Multiple reflections require a backward recurrence over five interfaces.
    for (i in 5:1) {
        tail <- attenuation[i + 1L]^2 * effective[i + 1L]
        transmission[i] <- (1 - reflection[i]) / (1 - reflection[i] * tail)
        effective[i] <- 1 - (1 - tail) * transmission[i]
    }
    c(tau = prod(transmission * attenuation[-1L]), rho = effective[1L])
}

# Reconstruct the positive-TYPE two-layer optical subroutine recovered from the
# native binary. These are effective solar properties, not a SYS_WINDOW stack.
window__n2_optics <- function(sc) {
    solve <- window__n2_angle
    # Match the native search bounds, initial trials and stopping rule. A target
    # above achievable transmission is retained as a bounded-fit diagnostic.
    target <- .87 * sc
    low <- 1.526
    high <- 20
    left <- 8.58244006874
    right <- 12.943559931260001
    error_left <- abs(solve(left)[[1L]] - target)
    error_right <- abs(solve(right)[[1L]] - target)
    for (iteration in seq_len(100L)) {
        if (abs(left - right) <= .001) {
            break
        }
        if (error_right > error_left) {
            high <- right
            right <- left
            error_right <- error_left
            left <- low + (high - low) * .38196601
            error_left <- abs(solve(left)[[1L]] - target)
        } else {
            low <- left
            left <- right
            error_left <- error_right
            right <- low + (high - low) * .61803399
            error_right <- abs(solve(right)[[1L]] - target)
        }
    }
    parameter <- (left + right) / 2
    normal <- solve(parameter)
    diffuse <- solve(parameter, pi / 3)
    c(
        tau = unname(normal[1L]),
        rho = unname(normal[2L]),
        diffuse_tau = unname(diffuse[1L]),
        diffuse_alpha = 1 - sum(diffuse),
        parameter = parameter,
        nominal_tau = target
    )
}

# Source angle tables use the maintained Table:Lookup schema introduced in 9.2.
# Only an actually emitted detailed N2 window raises the generation baseline;
# unused catalogue types and explicit aggregate fallbacks do not require it.
window__angle_generation_version <- function(dest, target) {
    if (!DBI::dbExistsTable(dest, "WINDOW")) {
        return(NULL)
    }
    windows <- const__window_type_performance(dest)
    if (!any(windows$DETAILED_EQUIVALENT)) {
        return(NULL)
    }
    minimum <- numeric_version("9.2.0")
    if (numeric_version(as.character(target)) < minimum) {
        abort(
            paste(
                "SC-derived positive-TYPE N2 angle optics require EnergyPlus",
                "9.2.0 or newer for Table:Lookup; normal-only fallback would",
                "discard verified source angular inputs."
            ),
            class = "destep_unsupported_window_angle_target"
        )
    }
    minimum
}

# Emit a 91-angle grey equivalent at two wavelengths. EnergyPlus table ordering
# makes the last dimension vary fastest, so every angle repeats at .1 and 4 um.
# The source is not spectral: the wavelength copies and reciprocal back curve
# are declared approximations, independent of native fixed-60-degree diffuse.
window__angle_objects <- function(dest, ep, name, sc) {
    solve <- window__n2_angle
    optics <- window__n2_optics(sc)
    curve <- vapply(
        (0:90) * pi / 180,
        function(angle) solve(optics[["parameter"]], angle),
        numeric(2L)
    )
    if (
        any(!is.finite(curve)) ||
            any(curve < 0) ||
            any(curve > 1) ||
            any(colSums(curve) > 1 + 1e-12)
    ) {
        abort("Recovered N2 angle optics violate the optical energy budget.")
    }
    angle <- paste0(name, " Angle")
    wave <- paste0(name, " Grey Wavelength")
    axes <- paste0(name, " Axes")
    # Construct one sorted axis with explicit limits and inline numeric values.
    independent <- function(name, values, unit) {
        # Positional fields expand all extensible groups in one call; named
        # future groups cannot resolve before preceding groups are present.
        c(
            list(
                name,
                "Linear",
                "Constant",
                min(values),
                max(values),
                "",
                unit,
                "",
                "",
                ""
            ),
            as.list(values)
        )
    }
    names <- paste0(name, c(" Tau", " Front Rho", " Back Rho"))
    tables <- lapply(seq_along(names), function(index) {
        values <- rep(curve[if (index == 1L) 1L else 2L, ], each = 2L)
        c(
            list(
                names[index],
                axes,
                "None",
                1,
                0,
                1,
                "Dimensionless",
                "",
                "",
                ""
            ),
            as.list(values)
        )
    })
    output <- conv__combine_outputs(list(
        conv__add_objects(
            dest,
            ep,
            "Table:IndependentVariable",
            list(
                # Older maintained engines validate this metadata using output
                # unit names and reject Angle despite accepting it in the IDD.
                # WindowManager supplies degrees directly; no unit conversion
                # occurs in CurveManager, so Dimensionless retains 0:90 degrees.
                independent(angle, 0:90, "Dimensionless"),
                independent(wave, c(.1, 4), "Dimensionless")
            )
        ),
        conv__add_objects(
            dest,
            ep,
            "Table:IndependentVariableList",
            list(list(
                name = axes,
                independent_variable_1_name = angle,
                independent_variable_2_name = wave
            ))
        ),
        conv__add_objects(dest, ep, "Table:Lookup", tables)
    ))
    list(output = output, names = names)
}

# Resolve per-window inputs before both construction and geometry conversion.
# Missing optional schema remains an explicit aggregate approximation; invalid
# saved K values are never silently replaced by a default.
window__effective_performance <- function(dest, value) {
    value <- data.table::copy(value)
    count <- nrow(value)
    fields <- list(
        ROW_K = rep(1, count),
        LAYER_NUM = rep(NA_real_, count),
        LIGHT_REFLECT_RATIO = rep(NA_real_, count),
        SIDE1_BLACKNESS = rep(NA_real_, count),
        SIDE2_BLACKNESS = rep(NA_real_, count),
        SOURCE_ROW_K_PRESENT = rep(FALSE, count)
    )
    for (name in names(fields)) {
        data.table::set(value, NULL, name, fields[[name]])
    }
    if (count) {
        if (db_has_fields(dest, "WINDOW", "K")) {
            rows <- DBI::dbGetQuery(dest, "SELECT ID, K FROM WINDOW")
            index <- match(value$WINDOW_ID, rows$ID)
            if (anyDuplicated(rows$ID) || anyNA(index)) {
                abort("Ambiguous WINDOW.ID while resolving window K.")
            }
            data.table::set(value, NULL, "ROW_K", as.double(rows$K[index]))
            data.table::set(value, NULL, "SOURCE_ROW_K_PRESENT", TRUE)
        }
        invalid <- which(!is.finite(value$ROW_K) | value$ROW_K <= 0)
        if (length(invalid)) {
            abort(
                sprintf(
                    "Invalid WINDOW.K for WINDOW.ID=%s; expected finite positive factors.",
                    paste(value$WINDOW_ID[invalid], collapse = ",")
                ),
                class = "destep_invalid_window_factor"
            )
        }
        available <- intersect(
            c("LAYER_NUM", "LIGHT_REFLECT_RATIO"),
            DBI::dbListFields(dest, "WINDOW_TYPE_DATA")
        )
        if (length(available)) {
            rows <- DBI::dbGetQuery(
                dest,
                paste0(
                    "SELECT ID, ",
                    paste(available, collapse = ", "),
                    " FROM WINDOW_TYPE_DATA"
                )
            )
            index <- match(value$TYPE_ID, rows$ID)
            for (name in available) {
                data.table::set(
                    value,
                    NULL,
                    name,
                    as.double(rows[[name]][index])
                )
            }
        }
        if (
            DBI::dbExistsTable(dest, "SURFACE") &&
                db_has_fields(dest, "SURFACE", c("SURFACE_ID", "BLACKNESS")) &&
                db_has_fields(dest, "WINDOW", c("SIDE1", "SIDE2"))
        ) {
            rows <- DBI::dbGetQuery(
                dest,
                "SELECT W.ID,
                S1.BLACKNESS AS SIDE1_BLACKNESS, S2.BLACKNESS AS SIDE2_BLACKNESS
                FROM WINDOW W LEFT JOIN SURFACE S1 ON W.SIDE1=S1.SURFACE_ID
                LEFT JOIN SURFACE S2 ON W.SIDE2=S2.SURFACE_ID"
            )
            index <- match(value$WINDOW_ID, rows$ID)
            if (anyDuplicated(rows$ID) || anyNA(index)) {
                abort("Ambiguous window face emissivity references.")
            }
            for (name in c("SIDE1_BLACKNESS", "SIDE2_BLACKNESS")) {
                data.table::set(
                    value,
                    NULL,
                    name,
                    as.double(rows[[name]][index])
                )
            }
            n2 <- is.finite(value$LAYER_NUM) &
                value$LAYER_NUM == 2 &
                value$TYPE_ID > 0
            invalid <- which(
                n2 &
                    (!is.finite(value$SIDE1_BLACKNESS) |
                        value$SIDE1_BLACKNESS < 0 |
                        value$SIDE1_BLACKNESS > 1 |
                        !is.finite(value$SIDE2_BLACKNESS) |
                        value$SIDE2_BLACKNESS < 0 |
                        value$SIDE2_BLACKNESS > 1)
            )
            if (length(invalid)) {
                abort(
                    sprintf(
                        "Invalid or unresolved two-layer window BLACKNESS for WINDOW.ID=%s.",
                        paste(value$WINDOW_ID[invalid], collapse = ",")
                    ),
                    class = "destep_invalid_window_emissivity"
                )
            }
        }
    }
    native_n2 <- value$TYPE_ID > 0 &
        is.finite(value$LAYER_NUM) &
        value$LAYER_NUM == 2
    # Saved visible tau/rho are not consumed by the verified positive N2
    # thermal/solar/GUI daylighting paths. They are retained without imposing
    # a target optical budget on dormant source metadata.
    data.table::set(
        value,
        NULL,
        "SOURCE_LIGHT_REFLECT_RATIO",
        value$LIGHT_REFLECT_RATIO
    )
    data.table::set(
        value,
        NULL,
        "SOURCE_VISIBLE_USAGE",
        data.table::fifelse(
            native_n2,
            "not_used_in_verified_positive_N2_branch",
            "unverified_native_visible_path"
        )
    )
    detailed <- native_n2 &
        is.finite(value$SIDE1_BLACKNESS) &
        value$SIDE1_BLACKNESS > 0 &
        value$SIDE1_BLACKNESS <= 1 &
        is.finite(value$SIDE2_BLACKNESS) &
        value$SIDE2_BLACKNESS > 0 &
        value$SIDE2_BLACKNESS <= 1
    data.table::set(
        value,
        NULL,
        "REPRESENTATION",
        data.table::fifelse(
            detailed,
            "EquivalentGlazing: SC-derived N2 source angle solar/grey-visible optics",
            "SimpleGlazing: nominal SHGC = 0.87 * SC"
        )
    )
    data.table::set(value, NULL, "DETAILED_EQUIVALENT", detailed)
    # For the recovered N2 branch, G/A = row_K / max(1/type_U - .157861, 1e-6).
    # This alters conductivity, never fenestration geometry or multiplier.
    # The same native resistance applies when missing target emissivity support
    # requires SimpleGlazing; target representation cannot choose source films.
    resistance <- data.table::fifelse(
        native_n2,
        pmax(1 / value$K - .157861, 1e-6) / value$ROW_K,
        (1 / value$K - 1 / 8.7 - 1 / 23.3) / value$ROW_K
    )
    data.table::set(value, NULL, "GLASS_RESISTANCE", resistance)
    for (name in c(
        "NORMAL_SOLAR_TAU",
        "NORMAL_SOLAR_RHO",
        "NORMAL_VISIBLE_TAU",
        "NORMAL_VISIBLE_RHO",
        "NATIVE_DIFFUSE_TAU",
        "NATIVE_DIFFUSE_ALPHA"
    )) {
        data.table::set(value, NULL, name, rep(NA_real_, count))
    }
    for (type_id in unique(value$TYPE_ID[native_n2])) {
        rows <- which(native_n2 & value$TYPE_ID == type_id)
        optics <- window__n2_optics(value$SC[rows[1L]])
        # The GUI daylighting path shares the SC optical stack. Also use its
        # normal VT if thermal constraints retain SimpleGlazing for this N2
        # type, so a dormant saved VT cannot gain new target meaning.
        data.table::set(value, rows, "NORMAL_VISIBLE_TAU", optics[["tau"]])
        data.table::set(value, rows, "NORMAL_VISIBLE_RHO", optics[["rho"]])
        data.table::set(value, rows, "LIGHT_TRANS_RATIO", optics[["tau"]])
        rows <- rows[detailed[rows]]
        columns <- c(
            "NORMAL_SOLAR_TAU",
            "NORMAL_SOLAR_RHO",
            "NATIVE_DIFFUSE_TAU",
            "NATIVE_DIFFUSE_ALPHA"
        )
        for (i in seq_along(columns)) {
            data.table::set(value, rows, columns[i], optics[i])
        }
    }
    if (any(detailed)) {
        rows <- which(detailed)
        # Share objects only when type, material-area factor and both ordered
        # emissivities agree; identical TYPE alone cannot merge different windows.
        signature <- sprintf(
            "%s|%.17g|%.17g|%.17g",
            value$TYPE_ID[rows],
            value$ROW_K[rows],
            value$SIDE1_BLACKNESS[rows],
            value$SIDE2_BLACKNESS[rows]
        )
        variant <- match(signature, sort(unique(signature)))
        stem <- sprintf(
            "%s Equivalent Glazing %d",
            value$TYPE_NAME[rows],
            variant
        )
        data.table::set(value, rows, "SIMPLE_GLAZING_NAME", stem)
        data.table::set(
            value,
            rows,
            "TYPE_CONSTRUCTION_NAME",
            paste0(stem, " Construction")
        )
    }
    rows <- which(!detailed & value$ROW_K != 1)
    if (length(rows)) {
        for (column in c("SIMPLE_GLAZING_NAME", "TYPE_CONSTRUCTION_NAME")) {
            data.table::set(
                value,
                rows,
                column,
                sprintf(
                    "%s [row K=%.17g]",
                    value[[column]][rows],
                    value$ROW_K[rows]
                )
            )
        }
    }
    value
}

# Emit source-angle and reciprocal single-layer equivalent glass. Reverse objects
# swap the ordered IR emissivities; solar back reflection uses an explicit
# symmetric assumption. Ordinary EP glazing does not represent native glass C.
window__equivalent_objects <- function(dest, ep, windows) {
    if (!nrow(windows)) {
        return(NULL)
    }
    if (numeric_version(as.character(ep$version())) < "9.2") {
        abort(
            "SC-derived N2 angle optics require EnergyPlus 9.2.0 or newer.",
            class = "destep_unsupported_window_angle_target"
        )
    }
    parts <- vector("list", 3L * nrow(windows))
    for (i in seq_len(nrow(windows))) {
        optical <- window__angle_objects(
            dest,
            ep,
            paste0(windows$SIMPLE_GLAZING_NAME[i], " Source Optics"),
            windows$SC[i]
        )
        parts[[3L * i - 2L]] <- optical$output
        for (reverse in c(FALSE, TRUE)) {
            name <- paste0(
                windows$SIMPLE_GLAZING_NAME[i],
                if (reverse) " [Reverse]" else ""
            )
            front <- if (reverse) {
                windows$SIDE2_BLACKNESS[i]
            } else {
                windows$SIDE1_BLACKNESS[i]
            }
            back <- if (reverse) {
                windows$SIDE1_BLACKNESS[i]
            } else {
                windows$SIDE2_BLACKNESS[i]
            }
            values <- list(
                name = name,
                optical_data_type = "SpectralAndAngle",
                thickness = .006,
                solar_transmittance_at_normal_incidence = windows$NORMAL_SOLAR_TAU[
                    i
                ],
                front_side_solar_reflectance_at_normal_incidence = windows$NORMAL_SOLAR_RHO[
                    i
                ],
                back_side_solar_reflectance_at_normal_incidence = windows$NORMAL_SOLAR_RHO[
                    i
                ],
                visible_transmittance_at_normal_incidence = windows$NORMAL_VISIBLE_TAU[
                    i
                ],
                front_side_visible_reflectance_at_normal_incidence = windows$NORMAL_VISIBLE_RHO[
                    i
                ],
                back_side_visible_reflectance_at_normal_incidence = windows$NORMAL_VISIBLE_RHO[
                    i
                ],
                infrared_transmittance_at_normal_incidence = 0,
                front_side_infrared_hemispherical_emissivity = front,
                back_side_infrared_hemispherical_emissivity = back,
                conductivity = .006 / windows$GLASS_RESISTANCE[i],
                dirt_correction_factor_for_solar_and_visible_transmittance = 1,
                solar_diffusing = "No",
                window_glass_spectral_and_incident_angle_transmittance_data_set_table_name = optical$names[
                    1L
                ],
                window_glass_spectral_and_incident_angle_front_reflectance_data_set_table_name = optical$names[
                    2L
                ],
                window_glass_spectral_and_incident_angle_back_reflectance_data_set_table_name = optical$names[
                    3L
                ]
            )
            output <- eval(bquote(conv__add(
                .(dest),
                .(ep),
                "WindowMaterial:Glazing" := .(values)
            )))
            data.table::set(
                output$object,
                NULL,
                "comment",
                list(c(
                    sprintf(
                        "Equivalent positive-TYPE N%d glass; not the SYS_WINDOW physical stack.",
                        windows$LAYER_NUM[i]
                    ),
                    sprintf(
                        "DeST TYPE=%s; row K=%.9g; glass R=%.12g m2 K/W; source side1/side2 BLACKNESS=%.9g/%.9g.",
                        windows$TYPE_ID[i],
                        windows$ROW_K[i],
                        windows$GLASS_RESISTANCE[i],
                        windows$SIDE1_BLACKNESS[i],
                        windows$SIDE2_BLACKNESS[i]
                    ),
                    "Native 0-90 degree solar angle curves reconstructed from SC at 1-degree intervals; nominal 0.87*SC is the source index-search target, not an SHGC mapping.",
                    sprintf(
                        "Saved LIGHT_TRANS_RATIO=%.12g and LIGHT_REFLECT_RATIO=%.12g are not used in the verified positive TYPE N2 branch; retained as source metadata.",
                        windows$SOURCE_LIGHT_TRANS_RATIO[i],
                        windows$SOURCE_LIGHT_REFLECT_RATIO[i]
                    ),
                    "Visible reflectance uses the recovered stack as a declared grey equivalent; native daylighting room reflection uses separate area weights.",
                    "Two-wavelength grey spectral equivalent; EP angular polynomial fit and hemispherical diffuse integration remain, rather than DeST fixed-60-degree diffuse values. Back reflectance assumed equal to front.",
                    "Native solar absorption node placement and glass heat capacity are not represented."
                ))
            )
            parts[[3L * i - as.integer(!reverse)]] <- output
        }
    }
    conv__combine_outputs(parts)
}
