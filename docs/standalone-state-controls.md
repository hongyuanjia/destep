# Standalone sensible-gain and natural-ventilation controls

The converter can emit target-state controls entirely inside the IDF. No DeST result, Python callback, fitted climate factor, or external control file is required to run these objects with EnergyPlus CLI. Physical shared walls, the selected window representation, independent outdoor air and the nominal moisture companions remain in the normal target heat balance.

## Selecting the representation

For the independently validated standard-density and sensible-occupant expression:

```r
options <- destep_opts(
    hvac = "ideal_loads",
    ventilation_density = "standard",
    occupant_sensible = "temperature_feedback"
)
model <- to_idf(dest, "26.1", options = options)
```

For the explicitly approximate own-state natural-ventilation predictor:

```r
options <- destep_opts(
    hvac = "ideal_loads",
    ventilation_density = "standard",
    occupant_sensible = "temperature_feedback",
    ventilation_control = "target_prediction"
)
model <- to_idf(dest, "26.1", options = options)
```

`ventilation_density = "outdoor"`, `occupant_sensible = "nominal"` and `ventilation_control = "outdoor_band"` remain the constructor defaults. The recovered occupant rule depends on a native execution mode that cannot be universally inferred from the Access input. The prediction approximation has not improved every climate's annual totals, so it is not automatically enabled or selected by city. Save the chosen options with each comparison.

## What the options express

**Standard density** uses the target's site-standard reference density for natural ventilation. It is closer to the audited fixed-density native air-exchange convention than outdoor-temperature-varying density. EnergyPlus reference density and humidity-dependent specific heat still differ from DeST's fixed volumetric heat capacity. Independent mechanical outdoor air is not duplicated. This option currently requires EnergyPlus26.1 or newer, whose maintained IDD contains the density field.

**Occupant sensible feedback** computes `max(0, S0 + 5.536 * (26 - Tprevious))` watts/person once per target hour. `S0` is the nominal source sensible heat, not total metabolism. `Tprevious` is the last completed target zone-step mean, used as an approximation to the native hourly endpoint. Each room and nominal level has an independent activity schedule; minimum and scheduled pieces of that room share it. Count schedules and radiant fractions stay unchanged. Nominal occupant moisture remains a separate unmetered source; default native moisture feedback is not reproduced. The generated program requires the pre-heat-balance initialization point; the maintained minimum for this selected expression is9.2. Actual physical regression in this implementation used26.1.

**Target prediction** reconstructs ordinary ThirdOrder free-air response from the target's first full-zone heating/cooling predictions and three completed zone-average temperatures. It updates current air capacitance, declared base flow, flow bounds and outdoor state, while non-air coefficients lag one zone step. The control retains the independently recovered mode/active distinction and intermediate-flow equation, but substitutes its own target state for the native coupled network. It can admit useful cold outdoor air below the heating lower bound while limiting predicted overcooling. Availability, saved enhancement switch, room conditioning and source schedules retain their distinct roles; availability is not a universal on/off gate for natural ventilation.

Prediction currently requires IdealLoads, standard density, a well-mixed room-air model, ordinary dual setpoints, ThirdOrder, and unit sensible air-capacitance multiplier. Unsupported requested paths stop explicitly. The converter does not add another air-history solver or change the normal wall/glass heat balance. To keep the actual standard density and predictor consistent when a weather header differs, this option sets `Site:Location`'s `Keep Site Location Information` to `Yes`; the source latitude, longitude, time zone and elevation take precedence. The maintained baseline's six-day minimum warmup is explicit across generation versions.

The `conversion$state_controls` audit identifies the selected representation, per-room generated schedules and controllers, site policy and limitations. Saved Version comments record options and EMS purposes. `attr(model, "ventilation")` distinguishes the documented temperature-band rule from target-state prediction. The predicted supplement is available as an EMS output variable but is not automatically requested at high frequency.

## Practical boundary

These explicit options preserve the stated input/control expression, not the native coupled solver. Annual improvement is not guaranteed. Defaults remain outdoor density, nominal sensible gain and the outdoor-band rule. Unexplained annual residuals remain unresolved; peak matching is not promised.
