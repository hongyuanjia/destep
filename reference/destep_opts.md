# Configure DeST-to-EnergyPlus conversion

Build a reusable options object for [`to_eplus()`](to_eplus.md). A
preset supplies defaults; explicitly named settings override those
defaults. The constructor checks option values and dependencies;
conversion checks model and target version requirements. Aggregate
windows use a documented SimpleGlazing approximation and do not promise
full DeST solver equivalence.

## Usage

``` r
destep_opts(
  preset = "objects",
  ...,
  schedule_format = "compact",
  schedule_directory = NULL,
  run_period = c(1L, 365L),
  hvac = "ideal_loads",
  hvac_options = NULL,
  surface_convection = "dest",
  terrain = NULL,
  solar_distribution = NULL,
  shadow_update_days = NULL
)
```

## Arguments

- preset:

  \[string\] `"objects"` selects source input conversion. Necessary
  moisture EMS remains available.

- ...:

  Must be empty. Supply custom settings by their full names.

- schedule_format:

  \[string\] `"compact"` (default) writes date-based Schedule:Compact
  objects, losslessly merging consecutive identical days and equal
  hourly values. `"file"` writes all 8760 hourly values to a CSV
  referenced by Schedule:File objects. Neither uses weekdays to select
  source values. Both preserve the required unit conversions.

- schedule_directory:

  \[string or NULL\] Persistent output directory, required with
  `schedule_format = "file"`. Each conversion writes a unique CSV and
  returns absolute references; keep this file with the IDF. This
  directory is independent of the source weather file.

- run_period:

  \[integer vector\] Inclusive, one-based simulation start and end days,
  from 1 to 365. Defaults to `c(1L, 365L)` because a saved DeST
  simulation range has not been established in the supported schema.
  Supply the days used for the source simulation when known. Dates are
  mapped to a non-leap calendar with daylight saving disabled. Schedules
  always cover the full year, including for a partial run period.
  Conversion guarantees the initial correspondence only; later user
  edits to the IDF, files or calendar are not monitored.

- hvac:

  \[string\] HVAC representation. `"ideal_loads"`, the default,
  preserves the established load-only conversion. `"physical"` groups
  conditioned rooms by their referenced `AC_SYS` and generates each
  supported air loop independently. A one-room `AC_SYS_TYPE = 0` system
  uses a dedicated constant-volume graph. Type-0 systems with multiple
  rooms use constant-volume fans and fix every terminal minimum flow to
  its maximum flow. Type-1 systems with one or more rooms retain
  variable-volume fans and source terminal bounds. Multiple referenced
  systems may be generated in one model when each has exactly one `AHU`
  and satisfies the same documented constraints. All paths accept
  verified `FRESH_AIR_TYPE` values 1, 5, or 6. Outdoor-air type 1 uses
  the DeST minimum as a fixed flow and retains the maximum as a source
  capacity boundary. Types 5 and 6 map the source minimum and maximum
  flows to differential dry-bulb and differential enthalpy economizers.
  The type-1 path proportionally reconciles terminal minimum flows when
  their sum falls below the system minimum outdoor-air flow by no more
  than 0.01%, with a warning; larger conflicts stop conversion.
  Multizone paths require one shared availability schedule per system
  and map matching `AC_SYS.SUPPLY_T_MIN/MAX` schedules to the
  cooling-coil setpoint and use their minimum value for cooling sizing;
  distinct minimum and maximum trajectories remain unsupported. Physical
  paths require `ver = "9.0.1"`, an installed matching EnergyPlus
  version, and explicit `hvac_options` for parameters absent from DeST.
  All HVAC representations reject models containing `AC_SYS_TYPE` values
  other than 0 and 1 instead of silently omitting unsupported systems.
  The current physical path cannot be combined with nonzero people or
  equipment moisture: its 9.0.1 output predates the required EMS calling
  point. Use `"ideal_loads"` on EnergyPlus 9.1 or newer for these
  sources.

- hvac_options:

  \[list or NULL\] Named equipment parameters required by the selected
  `hvac = "physical"` paths. Common fan fields are
  `supply_fan_total_efficiency`, `supply_fan_delta_pressure_pa`,
  `supply_fan_motor_efficiency`, `supply_fan_motor_in_air_fraction`, the
  corresponding four `return_fan_*` fields,
  `zone_exhaust_fan_total_efficiency`, and
  `zone_exhaust_fan_pressure_rise_pa`. Common coil and plant fields are
  `chilled_water_design_setpoint_c`,
  `condenser_water_design_setpoint_c`, `chiller_type`,
  `chiller_nominal_cop`, and `tower_type`. A model with any single-zone
  type-0 path also requires five `return_fan_power_coefficient_*`
  fields, `cooling_coil_design_setpoint_c`,
  `heating_coil_design_setpoint_c`,
  `heating_coil_rated_air_water_convection_ratio`,
  `hot_water_design_setpoint_c`, `boiler_type`, `boiler_efficiency`, and
  `boiler_fuel_type`. Type-1 paths also require the five fan power
  coefficients; multizone type-0 paths use constant-volume fans and do
  not. All terminal-reheat paths require
  `cooling_coil_type = "ChilledWater"`,
  `preheat_coil_type = "Electric"`, `preheat_coil_design_setpoint_c`,
  `reheat_coil_type = "Electric"`, and a named numeric
  `zone_outdoor_air_flow_m3_s` vector. Its names must be exactly the
  DeST `ROOM.ID` values served by terminal-reheat paths. Allocations are
  checked per `AC_SYS`, and each system sum must equal that source
  system's minimum outdoor-air flow. One flat option list currently
  applies the same equipment and shared plant assumptions to all
  generated systems. Only fields required by the selected paths need to
  be supplied. These values are never inferred from reference models.

- surface_convection:

  \[string\] `"dest"` (default) retains fixed source coefficients on
  walls, floors, roofs, windows and doors using ordinary EnergyPlus
  surface-property objects. `"energyplus"` omits these overrides so
  EnergyPlus selects its own coefficients. Furniture's internal-mass
  exchange definition is retained separately.

- terrain:

  \[string or NULL\] EnergyPlus terrain: `"Country"`, `"Suburbs"`,
  `"City"`, `"Ocean"` or `"Urban"`. `NULL` retains the converter/IDD
  default.

- solar_distribution:

  \[string or NULL\] EnergyPlus solar method: `"MinimalShadowing"`,
  `"FullExterior"`, `"FullInteriorAndExterior"`,
  `"FullExteriorWithReflections"` or
  `"FullInteriorAndExteriorWithReflections"`. `NULL` retains the
  default. Choose a method suitable for the geometry; interior beam
  distribution has enclosure/convexity requirements. A `WithReflections`
  method enables exterior reflections from preserved opaque
  `SHADING.ROU` values; no visible or specular reflectance is inferred.

- shadow_update_days:

  \[integer or NULL\] Positive periodic shading update interval in days.
  EnergyPlus warns above 31. `NULL` retains the default. Selections are
  recorded in `attr(model, "conversion")$simulation` and IDF comments.

## Value

An object of class `destep_options`, accepted by `to_eplus(options = )`.

## People inputs and interzone surfaces

Conversion retains nominal people sensible heat and `O_DAMP_PER_PERSON`;
DeST occupant temperature feedback is not reproduced. Nonzero people
moisture requires EnergyPlus 9.1 or newer. An unmetered OtherEquipment
companion uses target vapor enthalpy to express nominal g/h/person as
latent watts. Rapid temperature changes can leave a one-zone-step mass
residual. People carries sensible heat only: its activity is not a
comfort-model metabolic input and its latent-gain reports exclude the
independent moisture source. Interzone surfaces retain their paired
surface references and constructions; no neighbor-air
equivalent-temperature boundary is generated.

## See also

[`to_eplus()`](to_eplus.md)

## Examples

``` r
destep_opts()
#> $preset
#> [1] "objects"
#> 
#> $schedule_format
#> [1] "compact"
#> 
#> $schedule_directory
#> NULL
#> 
#> $run_period
#> [1]   1 365
#> 
#> $hvac
#> [1] "ideal_loads"
#> 
#> $hvac_options
#> NULL
#> 
#> $surface_convection
#> [1] "dest"
#> 
#> $terrain
#> NULL
#> 
#> $solar_distribution
#> NULL
#> 
#> $shadow_update_days
#> NULL
#> 
#> attr(,"class")
#> [1] "destep_options"
# File schedules remain annual even when simulating days 59 through 61.
destep_opts(schedule_format = "file", schedule_directory = "schedules",
    run_period = c(59L, 61L))
#> $preset
#> [1] "objects"
#> 
#> $schedule_format
#> [1] "file"
#> 
#> $schedule_directory
#> [1] "schedules"
#> 
#> $run_period
#> [1] 59 61
#> 
#> $hvac
#> [1] "ideal_loads"
#> 
#> $hvac_options
#> NULL
#> 
#> $surface_convection
#> [1] "dest"
#> 
#> $terrain
#> NULL
#> 
#> $solar_distribution
#> NULL
#> 
#> $shadow_update_days
#> NULL
#> 
#> attr(,"class")
#> [1] "destep_options"
opts <- destep_opts(
    "objects",
    terrain = "Country",
    shadow_update_days = 1L
)
# to_eplus(dest, "23.1", options = opts)
```
