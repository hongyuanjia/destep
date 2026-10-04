# Read the unambiguous constant sensible-recovery subset. Current DeST labels
# describe seasonal maximum coefficients, not separate sensible/latent values.
# Equal seasonal values avoid inventing a season-selection algorithm.
hvac__heat_recovery_source <- function(dest, handler) {
    code <- handler$HEAT_RECOVER[[1L]]
    checkmate::assert_choice(as.character(code), as.character(0:2))
    result <- list(type = "None", status = "absent")
    if (code == 0L) {
        return(result)
    }
    coefficients <- unlist(
        handler[c(
            "MIN_T_EX_COEF",
            "MAX_T_EX_COEF",
            "MIN_D_EX_COEF",
            "MAX_D_EX_COEF"
        )],
        use.names = FALSE
    )
    checkmate::assert_numeric(
        coefficients,
        len = 4L,
        finite = TRUE,
        any.missing = FALSE,
        lower = 0,
        upper = 1
    )
    if (
        code != 2L ||
            coefficients[[1L]] != 0 ||
            coefficients[[3L]] != 0 ||
            coefficients[[2L]] != coefficients[[4L]]
    ) {
        abort(
            sprintf(
                paste(
                    "AHU %s has unsupported heat recovery (HEAT_RECOVER=%s).",
                    "Only sensible recovery with equal seasonal maxima and zero",
                    "minima is mapped; total-heat coefficients cannot be used as latent effectiveness."
                ),
                handler$AHU_ID[[1L]],
                code
            ),
            class = "destep_unsupported_hvac_heat_recovery"
        )
    }
    properties <- hvac__read_ahu_properties(dest, handler$AHU_ID[[1L]])
    resistance <- properties[properties$name == "AHU_HEAT_RECOVER_RESISTANCE"]
    checkmate::assert_integerish(
        handler$AHURES,
        len = 1L,
        lower = 0,
        any.missing = FALSE
    )
    # A specified resistance must not vanish into the existing generic fan
    # defaults. Duct-network ownership and pressure/power mapping need their
    # own implementation; do not claim support for those source inputs here.
    if (
        handler$AHURES[[1L]] != 0L ||
            nrow(resistance) > 0L &&
                (anyNA(resistance$data_double) ||
                    anyNA(resistance$data_long) ||
                    any(resistance$data_double != 0) ||
                    any(resistance$data_long != 0))
    ) {
        abort(
            "Heat recovery with a source duct network or nonzero pressure loss is not yet mapped.",
            class = "destep_unsupported_hvac_heat_recovery"
        )
    }
    list(
        type = "Sensible",
        status = "constant_sensible",
        sensible_effectiveness = coefficients[[2L]],
        latent_effectiveness = 0,
        effectiveness_origin = "equal DeST seasonal maxima; no flow-dependence data",
        target_assumptions = paste(
            "Plate exchanger; same effectiveness at 75 and 100 percent flow;",
            "native economizer lockout; no added outlet temperature limit, frost protection or auxiliary power;",
            "heat-recovery pressure loss is not separately mapped; existing target fan defaults are used"
        )
    )
}

# Let ExpandObjects create the outdoor-air equipment and mixer graph. After
# expansion, refinement connects recovery to this converter's zone exhaust.
hvac__configure_recovery_template <- function(model, system_id, recovery) {
    if (recovery$type == "None") {
        return(invisible(model))
    }
    model$object(paste0("DeST AC_SYS ", system_id))$set(
        heat_recovery_type = "Sensible",
        sensible_heat_recovery_effectiveness = recovery$sensible_effectiveness,
        latent_heat_recovery_effectiveness = 0,
        heat_recovery_heat_exchanger_type = "Plate",
        heat_recovery_frost_control_type = "None"
    )
    invisible(model)
}

# ExpandObjects invents a 75-percent-flow efficiency increment and 250 W of
# auxiliary power, plus a fixed 5 C outlet controller. Replace those
# template assumptions with the disclosed source-efficiency representation.
hvac__refine_heat_recovery <- function(model, system_id, recovery, zone_name) {
    if (recovery$type == "None") {
        return(invisible(model))
    }
    name <- paste0("DeST AC_SYS ", system_id, " Heat Recovery")
    exchanger <- model$object(name)
    fields <- c(
        "sensible_effectiveness_at_100_heating_air_flow",
        "sensible_effectiveness_at_75_heating_air_flow",
        "sensible_effectiveness_at_100_cooling_air_flow",
        "sensible_effectiveness_at_75_cooling_air_flow"
    )
    values <- stats::setNames(
        rep(list(recovery$sensible_effectiveness), length(fields)),
        fields
    )
    values$nominal_electric_power <- 0
    # No source field in this supported subset specifies an HX outlet limit.
    # Use the native exchanger default instead of the template's fixed 5 C
    # target, which otherwise clips recovery in mild outdoor conditions.
    values$supply_air_outlet_temperature_control <- "No"
    manager <- model$object(paste0(
        "DeST AC_SYS ",
        system_id,
        " Heat Recovery Air Temp Manager"
    ))
    checkmate::assert_true(identical(
        unname(unlist(manager$value("setpoint_node_or_nodelist_name"))),
        unname(unlist(exchanger$value("supply_air_outlet_node_name")))
    ))
    model$del(manager$id())
    # This converter balances the outdoor supply with a zone exhaust fan. The
    # template relief stream is then empty, so recover from the actual exhaust
    # outlet instead. Multizone exhaust mixing is deliberately not guessed.
    exhaust <- model$object(paste(zone_name, "Exhaust Fan"))
    values$exhaust_air_inlet_node_name <- unname(unlist(
        exhaust$value("air_outlet_node_name")
    ))
    do.call(exchanger$set, values)
    exchanger$comment(
        c(
            "DeST HEAT_RECOVER=2; equal seasonal sensible maxima retained.",
            recovery$target_assumptions
        ),
        append = TRUE
    )
    invisible(model)
}
