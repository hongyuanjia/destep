# Read emitted positional values in IDD order. Only generated objects are
# consumed; source results and research callbacks never enter saved controls.
state_controls__rows <- function(model, class) {
    table <- model$to_table()
    selected <- which(table$class == class)
    rows <- table[selected]
    if (!nrow(rows)) {
        return(list())
    }
    lapply(split(rows, rows$id), function(row) {
        stats::setNames(as.list(row$value), as.character(row$index))
    })
}

# Add heterogeneous positional records through the actual generation IDD,
# including extensible EMS program fields, in one model mutation.
state_controls__add <- function(model, rows) {
    if (!length(rows)) {
        return(invisible(NULL))
    }
    args <- lapply(rows, function(row) {
        fields <- vapply(
            seq_len(length(row) - 1L),
            function(i) {
                conv__idd_field_name(model, row[[1L]], i)
            },
            character(1L)
        )
        values <- as.list(row[-1L])
        if (row[[1L]] == "Schedule:Constant") {
            values[[3L]] <- as.numeric(values[[3L]])
        }
        stats::setNames(values, fields)
    })
    names(args) <- vapply(rows, `[[`, character(1L), 1L)
    invisible(do.call(model$add, args))
}

# Apply explicit target-state choices without changing shared walls, windows,
# thermostat schedules, moisture companions, or independent fresh air.
state_controls__apply <- function(model, options, hvac) {
    vents <- state_controls__rows(model, "ZoneVentilation:DesignFlowRate")
    if (options$ventilation_density == "standard") {
        for (row in vents) {
            model$object(row[["1"]])$set(density_basis = "Standard")
        }
    }
    people <- if (options$occupant_sensible == "temperature_feedback") {
        state_controls__people(model)
    } else {
        list()
    }
    prediction <- list()
    if (options$ventilation_control == "target_prediction") {
        if (hvac != "ideal_loads") {
            stop(
                "target_prediction currently requires hvac = 'ideal_loads'.",
                call. = FALSE
            )
        }
        prediction <- state_controls__ventilation(model)
        if (length(prediction)) {
            warn(
                "Standalone target-state natural ventilation uses one-zone-step-lagged non-air drivers; it is not DeST coupled-solver control and annual improvement is not guaranteed.",
                class = "destep_approximate_ventilation_control"
            )
        }
    }
    list(
        ventilation_density = options$ventilation_density,
        occupant_sensible = options$occupant_sensible,
        ventilation_control = options$ventilation_control,
        people = people,
        ventilation = prediction,
        source_results_consumed = FALSE,
        prediction_site_policy = if (length(prediction)) {
            "source_location_retained_including_elevation"
        } else {
            "EnergyPlus_location_policy"
        },
        limitations = c(
            "People moisture remains nominal",
            "Target hourly endpoint is last zone-step mean",
            "Ventilation non-air drivers lag one zone step",
            "No native coupled-network prediction or mode equivalence"
        )
    )
}

# Clone activity per zone and level, sharing it only between minimum and
# scheduled pieces of the same physical room. Preserve every count/fraction.
state_controls__people <- function(model) {
    people <- state_controls__rows(model, "People")
    constants <- state_controls__rows(model, "Schedule:Constant")
    levels <- stats::setNames(
        lapply(constants, `[[`, "3"),
        vapply(constants, `[[`, character(1L), "1")
    )
    registers <- list()
    rows <- list()
    updates <- list()
    for (person in people) {
        if (
            as.numeric(person[["9"]]) != 1 || is.null(levels[[person[["10"]]]])
        ) {
            stop(
                "Temperature feedback requires generated sensible-only People with constant nominal activity.",
                call. = FALSE
            )
        }
        level <- as.numeric(levels[[person[["10"]]]])
        key <- paste(person[["2"]], format(level, digits = 17), sep = "\r")
        if (is.null(registers[[key]])) {
            prefix <- paste0("DeST_People_Feedback_", length(registers) + 1L)
            activity <- paste0(prefix, "_Activity")
            p <- function(text) {
                gsub("P_", paste0(prefix, "_"), text, fixed = TRUE)
            }
            rows <- c(
                rows,
                list(
                    c(
                        "Schedule:Constant",
                        activity,
                        "",
                        format(level, digits = 17)
                    ),
                    c(
                        "EnergyManagementSystem:Sensor",
                        p("P_T"),
                        person[["2"]],
                        "Zone Mean Air Temperature"
                    ),
                    c("EnergyManagementSystem:GlobalVariable", p("P_Last")),
                    c(
                        "EnergyManagementSystem:Actuator",
                        p("P_Activity"),
                        activity,
                        "Schedule:Constant",
                        "Schedule Value"
                    ),
                    c(
                        "EnergyManagementSystem:Program",
                        p("P_Init"),
                        p("SET P_Last = 26")
                    ),
                    c(
                        "EnergyManagementSystem:Program",
                        p("P_Begin"),
                        "IF TimeStepNum == 1",
                        p(sprintf(
                            "SET P_Activity = @MAX 0 (%.17g + 5.536 * (26 - P_Last))",
                            level
                        )),
                        "ENDIF"
                    ),
                    c(
                        "EnergyManagementSystem:Program",
                        p("P_End"),
                        p("SET P_Last = P_T")
                    ),
                    c(
                        "EnergyManagementSystem:ProgramCallingManager",
                        p("P_Init_Manager"),
                        "BeginNewEnvironment",
                        p("P_Init")
                    ),
                    c(
                        "EnergyManagementSystem:ProgramCallingManager",
                        p("P_Begin_Manager"),
                        "BeginZoneTimestepBeforeInitHeatBalance",
                        p("P_Begin")
                    ),
                    c(
                        "EnergyManagementSystem:ProgramCallingManager",
                        p("P_End_Manager"),
                        "EndOfZoneTimestepAfterZoneReporting",
                        p("P_End")
                    )
                )
            )
            registers[[key]] <- list(
                zone = person[["2"]],
                nominal_W_person = level,
                schedule = activity
            )
        }
        updates[[length(updates) + 1L]] <- list(
            name = person[["1"]],
            activity = registers[[key]]$schedule
        )
    }
    # eplusr validates references immediately: add schedules and their EMS
    # before pointing existing People at those new schedules.
    state_controls__add(model, rows)
    for (update in updates) {
        model$object(update$name)$set(
            activity_level_schedule_name = update$activity
        )
        comments <- model$object(update$name)$comment()
        comments <- comments[
            !grepl(
                "temperature feedback is not reproduced",
                comments,
                fixed = TRUE
            )
        ]
        model$object(update$name)$comment(c(
            comments,
            "Standalone EMS sensible feedback uses own previous-hour endpoint approximation; moisture remains nominal."
        ))
    }
    unname(registers)
}

# Reconstruct the ThirdOrder free-air prediction from the target's own full
# predictor and three completed zone-average temperatures. K/S retain a one
# zone-step delay; no DeST temperatures, loads, decisions or fitted factors.
state_controls__ventilation <- function(model) {
    vents <- state_controls__rows(model, "ZoneVentilation:DesignFlowRate")
    supplements <- Filter(
        function(v) {
            grepl("Documented Range Supplement", v[["1"]], fixed = TRUE)
        },
        vents
    )
    if (!length(supplements)) {
        return(list())
    }
    algorithms <- state_controls__rows(model, "ZoneAirHeatBalanceAlgorithm")
    if (
        length(algorithms) &&
            algorithms[[1L]][["1"]] != "ThirdOrderBackwardDifference"
    ) {
        stop(
            "target_prediction requires ThirdOrderBackwardDifference.",
            call. = FALSE
        )
    }
    # The reconstructed coefficients assume the normal single-node air balance and
    # unit sensible storage multiplier emitted by this converter.
    air <- state_controls__rows(model, "RoomAirModelType")
    if (any(vapply(air, function(x) x[["3"]] != "Mixing", logical(1L)))) {
        stop("target_prediction requires well-mixed room air.", call. = FALSE)
    }
    capacity <- state_controls__rows(
        model,
        "ZoneCapacitanceMultiplier:ResearchSpecial"
    )
    if (
        any(vapply(
            capacity,
            function(x) !is.null(x[["3"]]) && as.numeric(x[["3"]]) != 1,
            logical(1L)
        ))
    ) {
        stop(
            "target_prediction currently requires unit sensible air capacitance multiplier.",
            call. = FALSE
        )
    }
    location <- state_controls__rows(model, "Site:Location")[[1L]]
    # StdRhoAir has no EMS interface. Bind the engine's reference density to
    # the same source location used in the predictor instead of letting a
    # different EPW header silently replace its elevation and standard density.
    model$object(location[["1"]])$set(keep_site_location_information = "Yes")
    elevation <- as.numeric(location[["5"]])
    standard_pressure <- 101325 * (1 - 2.25577e-5 * elevation)^5.2559
    zones <- state_controls__rows(model, "Zone")
    ideal <- state_controls__rows(model, "ZoneHVAC:IdealLoadsAirSystem")
    thermostats <- state_controls__rows(model, "ZoneControl:Thermostat")
    pairs <- state_controls__rows(model, "ThermostatSetpoint:DualSetpoint")
    rows <- list()
    registers <- list()
    for (i in seq_along(supplements)) {
        v <- supplements[[i]]
        zone <- v[["2"]]
        z <- Filter(function(x) x[["1"]] == zone, zones)[[1L]]
        system <- Filter(
            function(x) x[["1"]] == paste(zone, "Ideal Loads"),
            ideal
        )
        control <- Filter(function(x) x[["2"]] == zone, thermostats)
        if (length(system) != 1L || length(control) != 1L) {
            stop("Cannot resolve prediction zone controls.")
        }
        pair <- Filter(function(x) x[["1"]] == control[[1L]][["5"]], pairs)
        if (length(pair) != 1L) {
            stop("target_prediction requires ordinary DualSetpoint controls.")
        }
        volume <- as.numeric(z[["9"]])
        prefix <- paste0("DeST_Vent_Predict_", i)
        p <- function(text) gsub("P_", paste0(prefix, "_"), text, fixed = TRUE)
        base <- Filter(
            function(x) {
                x[["2"]] == zone &&
                    !grepl(
                        "Documented Range Supplement",
                        x[["1"]],
                        fixed = TRUE
                    )
            },
            vents
        )
        sensors <- list(
            Tin = c(zone, "Zone Air Temperature"),
            Tav = c(zone, "Zone Mean Air Temperature"),
            Win = c(zone, "Zone Air Humidity Ratio"),
            Tout = c(zone, "Zone Outdoor Air Drybulb Temperature"),
            Wout = c("Environment", "Site Outdoor Air Humidity Ratio"),
            Pressure = c("Environment", "Site Outdoor Air Barometric Pressure"),
            Heat = c(pair[[1L]][["2"]], "Schedule Value"),
            Cool = c(pair[[1L]][["3"]], "Schedule Value"),
            Available = c(system[[1L]][["2"]], "Schedule Value"),
            Bound = c(v[["3"]], "Schedule Value"),
            Qh = c(
                zone,
                "Zone Predicted Sensible Load to Heating Setpoint Heat Transfer Rate"
            ),
            Qc = c(
                zone,
                "Zone Predicted Sensible Load to Cooling Setpoint Heat Transfer Rate"
            ),
            Demand = c(
                zone,
                "Zone Predicted Sensible Load to Setpoint Heat Transfer Rate"
            ),
            ActualHeat = c(
                zone,
                "Zone Thermostat Heating Setpoint Temperature"
            ),
            ActualCool = c(zone, "Zone Thermostat Cooling Setpoint Temperature")
        )
        globals <- c(
            "T1",
            "T2",
            "T3",
            "Count",
            "K",
            "S",
            "Ready",
            "Mode",
            "Active",
            "Step",
            "Seen",
            "Base",
            "Limit",
            "Cv",
            "C",
            "History",
            "A",
            "B",
            "Plo",
            "Phi",
            "Target",
            "Flow",
            "D",
            "BB"
        )
        rows <- c(
            rows,
            lapply(names(sensors), function(n) {
                c(
                    "EnergyManagementSystem:Sensor",
                    p(paste0("P_", n)),
                    sensors[[n]]
                )
            }),
            list(
                c(
                    "EnergyManagementSystem:GlobalVariable",
                    p(paste0("P_", globals))
                ),
                c(
                    "EnergyManagementSystem:Actuator",
                    p("P_FlowAct"),
                    v[["1"]],
                    "Zone Ventilation",
                    "Air Exchange Flow Rate"
                )
            )
        )
        init <- p(paste0("SET P_", globals, " = 0"))
        begin <- c(
            p("SET P_Step = P_Step + 1"),
            p("SET P_Flow = 0"),
            p("SET P_Base = 0")
        )
        for (b in seq_along(base)) {
            bv <- base[[b]]
            if (bv[["4"]] != "AirChanges/Hour") {
                stop("Prediction supports generated ACH base flows only.")
            }
            sensor <- p(paste0("P_BaseSch", b))
            rows <- c(
                rows,
                list(c(
                    "EnergyManagementSystem:Sensor",
                    sensor,
                    bv[["3"]],
                    "Schedule Value"
                ))
            )
            begin <- c(
                begin,
                p(sprintf(
                    "SET P_Base = P_Base + %.17g * %s",
                    volume * as.numeric(bv[["8"]]) / 3600,
                    sensor
                ))
            )
        }
        begin <- c(
            begin,
            p(sprintf(
                "SET P_Limit = %.17g * P_Bound",
                volume * as.numeric(v[["8"]]) / 3600
            )),
            p(sprintf(
                "SET P_RhoStd = @RhoAirFnPbTdbW %.17g 20 0",
                standard_pressure
            )),
            # Use the verified psychrometric equation directly: older target
            # releases spell the Cp Erl function differently, and transition
            # does not rewrite a function embedded in a program string.
            p("SET P_WoutCp = @MAX P_Wout 0.00001"),
            p("SET P_Cv = (1004.84 + 1858.95 * P_WoutCp) * P_RhoStd"),
            p("SET P_RhoRoom = @RhoAirFnPbTdbW P_Pressure P_Tin P_Win"),
            p("SET P_WinCp = @MAX P_Win 0.00001"),
            p("SET P_CpRoom = 1004.84 + 1858.95 * P_WinCp"),
            p(sprintf(
                "SET P_C = %.17g * P_RhoRoom * P_CpRoom / (ZoneTimeStep * 3600)",
                volume
            )),
            p("SET P_History = 3 * P_T1 - 1.5 * P_T2 + P_T3 / 3"),
            p("SET P_Target = (3 * P_Heat + P_Cool) / 4"),
            p("IF P_Available <= 0.5"),
            p("SET P_Active = 0"),
            "ENDIF",
            p("SET P_Gate = 0"),
            p("IF (P_Ready == 1) && (P_Count >= 3) && (P_Cool > P_Heat)"),
            p("SET P_A = 11 * P_C / 6 + P_K + P_Cv * P_Base"),
            p("SET P_B = P_C * P_History + P_S + P_Cv * P_Base * P_Tout"),
            p("IF (P_A > 0) && (P_Limit > 0)"),
            p("SET P_Plo = P_B / P_A"),
            p(
                "SET P_Phi = (P_B + P_Cv * P_Limit * P_Tout) / (P_A + P_Cv * P_Limit)"
            ),
            p("IF (P_Phi < P_Plo) && (P_Tout < P_Plo)"),
            p("SET P_Gate = 1"),
            "ENDIF",
            "ENDIF",
            "ENDIF",
            # Flatten the eligibility checks: Erl limits IF nesting to five.
            p("IF P_Gate == 1"),
            p("IF (P_Available <= 0.5) && (P_Tout >= P_Heat)"),
            p("SET P_Flow = P_Limit"),
            p("ELSEIF P_Available > 0.5"),
            p(
                "SET P_Eligible = ((P_Mode == 1) && (P_Phi <= P_Cool)) || ((P_Active == 0) && (P_Plo >= P_Heat) && (P_Phi <= P_Cool))"
            ),
            p("IF (P_Eligible == 1) && (P_Target <= P_Phi)"),
            p("SET P_Flow = P_Limit"),
            p("ELSEIF (P_Eligible == 1) && (P_Tout <= P_Heat)"),
            p(
                "SET P_Candidate = (P_Target * P_A - P_B) / (P_Cv * (P_Tout - P_Target))"
            ),
            p("SET P_Flow = @MAX 0 P_Candidate"),
            p("SET P_Flow = @MIN P_Limit P_Flow"),
            "ENDIF",
            "ENDIF",
            "ENDIF",
            p("SET P_FlowAct = P_Flow")
        )
        capture <- p(c(
            "IF (SystemTimeStep == ZoneTimeStep) && (P_Seen <> P_Step)",
            "SET P_Seen = P_Step",
            "IF P_ActualCool > P_ActualHeat",
            "SET P_D = (P_Qc - P_Qh) / (P_ActualCool - P_ActualHeat)",
            "SET P_BB = P_D * P_ActualHeat - P_Qh",
            "IF (P_Count >= 3) && (P_D > 0)",
            "SET P_K = P_D - 11 * P_C / 6 - P_Cv * (P_Base + P_Flow)",
            "SET P_S = P_BB - P_C * P_History - P_Cv * (P_Base + P_Flow) * P_Tout",
            "SET P_Ready = 1",
            "ENDIF",
            "ENDIF",
            "ENDIF"
        ))
        end <- p(c(
            "SET P_T3 = P_T2",
            "SET P_T2 = P_T1",
            "SET P_T1 = P_Tav",
            "SET P_Count = P_Count + 1",
            "SET P_Mode = 0",
            "IF P_Available > 0.5",
            "IF P_Demand > 0",
            "SET P_Mode = 2",
            "ELSEIF P_Demand < 0",
            "SET P_Mode = 1",
            "ENDIF",
            "ENDIF",
            "IF (P_Available > 0.5) && (P_Flow > 0)",
            "SET P_Active = 0",
            "ENDIF",
            "IF P_Mode <> 0",
            "SET P_Active = 1",
            "ENDIF"
        ))
        programs <- list(
            Init = init,
            Begin = begin,
            Capture = capture,
            End = end
        )
        points <- c(
            Init = "BeginNewEnvironment",
            Begin = "BeginTimestepBeforePredictor",
            Capture = "AfterPredictorBeforeHVACManagers",
            End = "EndOfZoneTimestepAfterZoneReporting"
        )
        for (n in names(programs)) {
            rows <- c(
                rows,
                list(
                    c(
                        "EnergyManagementSystem:Program",
                        p(paste0("P_", n)),
                        programs[[n]]
                    ),
                    c(
                        "EnergyManagementSystem:ProgramCallingManager",
                        p(paste0("P_", n, "_Manager")),
                        points[[n]],
                        p(paste0("P_", n))
                    )
                )
            )
        }
        rows <- c(
            rows,
            list(c(
                "EnergyManagementSystem:OutputVariable",
                paste(prefix, "Supplement"),
                p("P_Flow"),
                "Averaged",
                "ZoneTimestep",
                "",
                "m3/s"
            ))
        )
        registers[[i]] <- list(
            zone = zone,
            object = v[["1"]],
            prefix = prefix,
            volume_m3 = volume
        )
    }
    state_controls__add(model, rows)
    registers
}
