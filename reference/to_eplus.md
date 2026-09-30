# Convert a DeST model to EnergyPlus model

Convert a DeST model to EnergyPlus model

## Usage

``` r
to_eplus(
  dest,
  ver = "latest",
  copy = TRUE,
  verbose = FALSE,
  hvac = c("ideal_loads", "physical"),
  hvac_options = NULL,
  people_heat = c("constant", "temperature_dependent"),
  window_optics = c("simple_glazing", "dest_solar"),
  source_distribution = c("energyplus", "dest"),
  source_options = NULL
)
```

## Arguments

- dest:

  A \[string or DBIConnection\] path to a DeST model file or a
  DBIConnection object.

- ver:

  \[string\] A character string specifying the EnergyPlus version. It
  can be `"latest"`, which is the default, to indicate using the latest
  EnergyPlus version supported by the
  {[eplusr](https://cran.r-project.org/package=eplusr)} package.
  Geometry compatibility has been validated against EnergyPlus 23.1;
  other versions currently reuse that profile with an explicit warning.

- copy:

  \[logical\] Whether to copy the input DeST database to a temporary
  SQLite database. Note that if `FALSE`, the input database will be
  modified during the conversion. Default is `TRUE`.

- verbose:

  \[logical\] Whether to show verbose messages. Default is `FALSE`.

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

- people_heat:

  \[string\] People sensible-heat mode. `"constant"` preserves the
  existing conversion and corresponds to DeST bshell's
  `--const_occupant` option. Use `"temperature_dependent"` for the
  default behavior verified with DeST 0.2.230705: per-person sensible
  heat is `max(0, input + 5.536 * (26 - previous temperature))` W. This
  mode requires EnergyPlus 9.1 or newer and uses the preceding
  EnergyPlus zone-step air temperature. Record both engines' time steps
  when comparing results. The source database does not store the DeST
  execution option. Moisture input is unchanged; radiant recipient
  fractions and full-building numerical equivalence remain limitations.

- window_optics:

  \[string\] Aggregate-window optical representation. `"simple_glazing"`
  preserves the existing default. The opt-in `"dest_solar"` mode uses SC
  and pane count to derive solar angle tables for the simplified model
  verified with DeST 0.2.230705. It supports exterior two/three-pane
  aggregate windows and EnergyPlus 23.1 or newer. SC specifies a
  normal-transmittance objective of `0.87 * SC`, not SHGC. Glass
  resistance and each window face's source blackness are preserved.
  EnergyPlus retains its own diffuse integration and heat-balance
  solver; native glass storage, sky exchange and room solar distribution
  are not added by this option. The tables are solar-only:
  visible/daylighting optics are not represented, and existing
  daylighting objects are rejected.

- source_distribution:

  \[string\] `"energyplus"` retains the default surface allocation.
  `"dest"` preserves the literal DeST air, wall, floor and roof
  fractions, including sums below one. This opt-in mode currently
  requires EnergyPlus 26.1 and `hvac = "ideal_loads"`. With windows it
  also requires `window_optics = "dest_solar"` and runs a
  weather-specific solar prepass. Unsupported moisture combinations,
  doors, daylighting and dynamic shading fail explicitly. Exterior
  radiation retains EnergyPlus defaults unless selected separately in
  `source_options`; native time-integration algorithms are not
  reproduced.

- source_options:

  \[list or NULL\] Options for `source_distribution = "dest"`. Models
  with windows require `weather`, an existing EPW path, and `directory`,
  a persistent directory for generated time tables and prepass records.
  Cache reuse checks the converted model, weather, external files,
  engine and generated data. `partition_boundary` defaults to
  `"energyplus"`, retaining coupled interzone surfaces; `"dest_air"`
  explicitly selects the neighbor-air plus prescribed radiation boundary
  verified with DeST 0.2.230705. The latter is a version-specific
  approximation, not a general interzone equivalence. Generated solar
  time tables use a full non-leap year at five-minute resolution.
  Reconvert after changing weather, geometry, optics, schedules or
  timestep; running or editing the returned `Idf` does not refresh them.
  Keep the directory or copy external files when saving.
  `exterior_boundary` defaults to `"energyplus"`; `"dest_sky"` selects
  the linear sky-only boundary verified with DeST 0.2.230705 for
  vertical walls/windows and horizontal roofs/exposed floors.
  Single-layer constructions and existing local environments are
  unsupported. This reads `OPTION.CAL_SKY_RADIATION`; the optional
  logical `sky_radiation` explicitly overrides that saved switch for a
  particular run. Missing switches require an explicit override. This
  boundary mode needs `weather` and `directory`, even with sky exchange
  disabled, to preserve dry-bulb convection when the weather indicates
  rain. An additional weather prepass preserves the target's height
  corrections and time grid; Windows receive the current time-table
  value through surface EMS actuators because the local-environment
  object excludes windows in the official input schema. No
  surface-temperature feedback is used. The target's default longwave
  exchange is suppressed with an outdoor emissivity of `1e-8`, leaving a
  small numerical residual. The returned `exterior_boundary` attribute
  records source coefficients, switch overrides and generated time
  tables. Reconvert when inputs change.

## Value

\[eplusr::Idf\] The converted EnergyPlus model. The opt-in source mode
attaches a `source_distribution` attribute containing its input and
cache audit. It does not establish whole-building equivalence.
