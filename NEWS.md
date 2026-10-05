# destep 0.0.0.9000

- Validate supply-temperature schedule IDs before integer coercion and use the
  shared decoder to reject incomplete or oversized annual payloads. Missing
  hourly temperatures and duplicate selected schedule IDs now fail at the
  source reader (#38).

- Enforce source room ventilation minima for explicit outdoor-air allocations,
  reject unused single-zone allocations, and keep return-fan power overrides
  independent of the supply-fan curve. Centralize linked integer properties in
  the source-reader module (#38).

- Map supported DeST terminal capacities, airflow bounds, selected products
  and prescribed water boundaries to connected EnergyPlus HVAC objects. Resolve
  source ownership and linked schedules, retain required moisture inputs, and
  diagnose unsupported selected equipment explicitly (#38).

- Select relative-humidity schedules only from effective conditioned-room types.
  Unused catalogue entries and legacy room-group RH fields no longer block
  conversion or change the units of unrelated schedules. Resolve shared RH
  names for both bounds in one database pass (#37).

- Use percent-compatible target schedule limits after RH conversion while
  preserving shared fractional schedules and decoded source inputs (#37).

- Keep development validation records local and exclude them from distributed
  package files (#37).

- Unified source conversion settings in `to_eplus(options = )` and
  `destep_opts()`. The `"objects"` preset preserves source inputs, with explicit
  schedule, HVAC, surface-convection and target simulation settings (#37).

- Replaced weekday-dependent source schedules with date-based compact or file
  schedules that preserve all 8760 hourly values and unit conversions. Added
  explicit simulation day ranges on a non-leap calendar (#37).

- Split people, lighting, equipment and prescribed moisture conversion into
  dedicated modules. Retained radiant-fraction precision and represented
  nominal people sensible heat and moisture independently. Moisture requires
  EnergyPlus 9.1 or newer and remains incompatible with the current 9.0.1
  physical HVAC path; rapid temperature changes can leave a one-step residual.
  Temperature-dependent occupant feedback and humidity caps are not reproduced
  (#37).

- Retired DeST-specific window angle tables, surface heat/solar distribution,
  sky-boundary prepasses, occupant feedback and neighbor-air approximations,
  together with the `"dest"` preset and their options. Conversion uses native
  EnergyPlus solar and exterior heat-balance calculations, retains coupled
  interzone constructions and fixed source convection, and records the
  remaining window, furniture and ground-depth assumptions (#37).

- Diagnose unsupported or invalid window inputs with source identifiers.
  Aggregate K/SC windows retain the documented SimpleGlazing approximation;
  unexpressed face emissivities remain visible in conversion metadata. Apply
  automatic soil per construction and reject unverified non-floor
  ground-contact stacks (#37).

- Honor saved `VARIANT_VENT = 0` by retaining minimum ventilation and omitting
  its variable increment. Record the fallback when the saved switch is absent.
  Preserve opaque `SHADING.ROU` in generated shading reflectance properties;
  reflection remains controlled by the selected solar-distribution method (#37).

- Fixed exposed-floor outside absorptance conversion to preserve literal zero
  values. The converter no longer substitutes unrelated exterior-wall or
  default material properties, which could introduce ground-reflected solar
  gains absent from the DeST input (#36).

- Added explicit `source_options$exterior_boundary = "dest_sky"` for the
  verified vertical/horizontal DeST sky boundary. It preserves per-face
  coefficients and the saved sky switch, records explicit run overrides, and
  uses target-weather time tables without surface-temperature feedback.
  Window boundaries use EMS to copy the current time-table value through
  the supported surface weather inputs. Check and coverage workflows now
  install EnergyPlus 26.1 alongside 23.1 (#36).

- Fixed `window_optics = "dest_solar"` to preserve each window's inside and
  outside surface blackness instead of using a fixed emissivity of 0.84.
  Windows sharing one optical type retain separate thermal properties when needed
  (#36).

- Added opt-in `source_distribution = "dest"` to `to_eplus()` for EnergyPlus
  26.1 ideal-loads models. Source converters supply their own heat metadata;
  net receiving areas and window distribution modes are extracted automatically.
  Weather-specific solar prepasses are reused only after checking model, file,
  engine and output identities. Generated time tables retain literal source
  fractions and deduplicate identical columns. Coupled interzone surfaces retain
  their original surface references. Whole-school equivalence and updated ASHRAE 140 acceptance remain
  separate validation tasks (#36).

- Fixed CI initialization by replacing the removed Homebrew Actions `master`
  reference with the upstream recommended pinned release (#34).

- Extended thermal-source conversion and supported physical HVAC assembly,
  with explicit validation limits for whole-building comparisons (#34).

- Simplified the README to project background, features, installation, and
  a usage example showing IDF and EPW object summaries, with links to
  conversion help and release notes (#34).

- Kept source metadata with the owning people, lighting, and equipment
  converters in `conv-people.R`, and furniture validation in
  `conv-furniture.R`. The internal `conv-source.R` module now handles generic
  source allocation and inventory checks.

- Added opt-in `window_optics = "dest_solar"` for exterior two/three-pane
  aggregate windows in EnergyPlus 23.1 or newer. It derives solar angle tables
  from DeST SC and pane count, preserving the normal-transmittance meaning of
  `0.87 * SC` rather than treating it as SHGC. The legacy default is unchanged.
  The solar-only representation retains glass resistance and exposed
  emissivity; daylighting, native glass storage, sky exchange, and room solar
  recipient allocation remain outside this option.

- Applied `ROOM_TYPE_DATA.L_HEAT_RATE` to room lighting heat while preserving
  the original lighting electricity consumption. An unmetered signed sensible
  source follows the same minimum/variable schedules and convective/radiant
  fractions. Native zero-source and independent ratio checks confirm the
  multiplication rule; this correction does not require EMS.

- Converted effective `ROOM_TYPE_DATA.FURNITURE_COEF` to a two-sided storage
  slab using the area-dependent rule verified against DeST 0.2.230705.
  Coefficients at or below one add no storage. Independent capacity and
  free-floating temperature checks support the EnergyPlus `InternalMass`
  representation; cross-engine whole-building equivalence remains unresolved.

- Preserved glass thermal resistance when converting aggregate DeST window
  K values: DeST's nominal surface films and the EnergyPlus simple-glazing
  winter films differ. Values outside the simple-glazing model's representable
  range now fail explicitly instead of silently changing material resistance.

- Converted nonzero `ROOM_TYPE_DATA.E_MIN_HUM`/`E_MAX_HUM` equipment moisture
  using the source equipment schedule and total/per-area basis, retaining
  independent sensible `ElectricEquipment` gains. An unmetered, all-latent
  `OtherEquipment` source uses EMS vapor-enthalpy correction to preserve the
  kg/h input without adding electricity consumption. The correction uses the
  latest available zone temperature; abrupt changes can leave a one-zone-step
  residual. This mapping requires EnergyPlus 9.1 or newer; older targets and
  invalid negative, non-finite, or inverted source ranges still fail explicitly.
- Preserved full `ROOM.AREA` precision in `Zone` floor area, avoiding rounding
  bias in per-area sensible and moisture gains.

- Extended DeST-to-EnergyPlus translation across Calload controls, weather,
  constructions, openings, and supported physical HVAC paths, while retaining
  explicit errors for unsupported `AC_SYS_TYPE` values (#33).

- Stopped `to_eplus()` with an explicit list of source systems when a DeST
  model contains an unsupported `AC_SYS_TYPE` other than 0 or 1, preventing
  unsupported HVAC families from being omitted silently.

- Corrected the `AHU.FAN` schema description: the field stores the rated air
  flow of the selected cooling or dehumidification device in m3/h, rather than
  a fan or equipment identifier.

- Fixed DeST lighting heat-gain conversion so `L_DIST_MODE` is represented
  without introducing an unrelated EnergyPlus visible-light fraction, and
  stopped mapping the DeST heat-to-electricity ratio to daylighting
  replaceability.

- Reconciled type-1 terminal minimum flows when their sum falls less
  than 0.01% below the DeST system minimum outdoor-air flow, preserving zone
  shares with an explicit warning while rejecting larger air-balance conflicts.
- Warned when aggregate `WindowMaterial:SimpleGlazingSystem` objects target
  EnergyPlus 9.0 through 9.3, whose angular-reflectance implementation was
  corrected in EnergyPlus 9.4.
- Mapped ordinary-glass `SYS_WINDOW` fallback layers to
  `WindowMaterial:Glazing:RefractionExtinctionMethod` using DeST thickness,
  conductivity, refractive index, extinction coefficient, and emissivity.
- Preserved full DeST site-coordinate precision and decoded the standard
  meridian in `ENVIRONMENT.PROPERTY` into the EnergyPlus site time zone.
- Preserved DeST schedules shared by humidity and non-humidity fields by
  generating a separate percent-valued copy for `ZoneControl:Humidistat`.
- Fixed `People` field generation for EnergyPlus 9.0.1 by resolving the
  version-specific design-level field names from the selected target IDD.
- Converted supported DeST exterior window overhangs and side fins to
  `Shading:Zone:Detailed` polygons.
- Resolved missing exterior absorptance sentinels on exposed DeST air floors
  without overwriting their independent room-side surface properties.
- Converted DeST thermally massless material sentinels to
  `Material:NoMass`, preserving their resistance-only representation.
- Converted DeST zero and near-zero specific-heat material encodings to
  `Material:NoMass`, preserving their source thermal resistance without
  inventing heat capacity.
- Normalized surface and window vertex starts to EnergyPlus's declared
  upper-left corner.
- Preserved `ENVIRONMENT.GROUND_REFLECT_COEF` by writing the same DeST ground
  reflectance to all twelve `Site:GroundReflectance` monthly fields.
- Fixed exterior-wall and roof layer order by retaining DeST's stored
  outside-to-inside sequence, while keeping reversed constructions for
  room-to-ground and reciprocal interzone faces.
- Fixed direct-normal radiation derived from hourly DeST weather by using the
  centered-hour mean solar altitude, avoiding nonphysical sunrise and sunset
  values in official prototype models.
- Preserved DeST surface convection coefficients, solar absorptance, and
  blackness using per-surface convection objects and deduplicated exposed-layer
  construction clones, while retaining reciprocal interzone layer order.
- Preserved independent DeST window convection coefficients by reading the
  window `SIDE1` and `SIDE2` surface records. Converted doors as opaque
  subsurfaces with reciprocal construction order while inheriting the effective
  face properties and convection coefficients of their owning enclosures.
- Warned when a DeST window references transmitted-solar `DIST_MODE` fractions,
  because the per-window distribution has no established direct EnergyPlus
  projection and would otherwise be omitted silently.
- Warned when `ROOM_GROUP.OF_AC_SYS` references a physical DeST
  air-conditioning system, reporting its raw system type, outdoor-air control,
  and available flow limits when the default `hvac = "ideal_loads"` conversion
  omits its fans, coils, air loop, plant equipment, and system-level outputs.
- Expanded `hvac = "physical"` conversion to group conditioned rooms by
  `AC_SYS`, generate multiple supported air loops in one model, accept any
  positive room count for type-1 VAV terminal-reheat systems, and accept
  multizone type-0 constant-volume terminal-reheat systems. One-room type-0
  systems retain their dedicated constant-volume path.
- Type-1 paths preserve each room's source minimum and maximum terminal flow in
  direct `AirTerminal:SingleDuct:VAV:Reheat` objects and use variable supply and
  return fans. Multizone type-0 paths fix every terminal minimum to its maximum
  and use constant-volume supply and return fans. Each air loop balances its
  explicit per-zone outdoor-air allocation through zone exhaust fans.
- Physical paths accept `FRESH_AIR_TYPE` 1, 5, or 6. Outdoor-air type 1 uses
  the DeST minimum as a fixed flow and retains the maximum as a source capacity
  boundary; types 5 and 6 map the source limits to EnergyPlus differential
  dry-bulb and differential enthalpy economizers.
- Physical paths preserve DeST system ownership, airflow limits, availability,
  and room temperature schedules in direct EnergyPlus 9.0.1 objects. Equipment
  performance and zone allocation values missing from the DeST model must be
  supplied through path-specific `hvac_options`. One flat option list and one
  synthesized shared plant currently apply to all generated air loops.
- Existing real-model numerical regression coverage remains one-room type 0,
  two-room type 0, and two-room type 1; larger zone counts and multiple-system
  assembly require additional real-model numerical evidence.
- Multizone physical HVAC paths use matching
  `AC_SYS.SUPPLY_T_MIN` and `SUPPLY_T_MAX` trajectories for the cooling-coil
  setpoint and their minimum for cooling sizing; distinct trajectories stop
  with an explicit unsupported error.
- Fixed Calload control mapping to resolve availability, temperature, and
  humidity schedules through `ROOM.TYPE` and `ROOM_TYPE_DATA`, while retaining
  `ROOM_GROUP.IS_AC_ROOM` as the room-conditioning eligibility flag.
- Restored the 1.2 m soil layer that DeST automatically appends to serialized
  ground-floor constructions but does not store in the source ACCDB tables.
- Mapped `ROOM_RELATION.VENT_TYPE=1` to a minimum-ACH
  `ZoneVentilation:DesignFlowRate` plus a normalized max-minus-min supplement
  enabled while outdoor temperature lies between the room-type heating and
  cooling setpoint schedules. This implements DeST's documented range rule but
  deliberately does not infer an HVAC-availability gate or claim equivalence
  to DeST's undocumented solver-state coupling.
- Removed the practical IdealLoads supply-humidity limitation from load-only
  models, and set an explicit 12-timestep-per-hour resolution so converted
  hourly relative-humidity bounds converge reproducibly.
- Reduced converted geometry fragmentation by preserving planar convex surfaces
  and rectangular windows, while making reciprocal interzone-window partitions
  deterministic and centralizing EnergyPlus geometry tolerances (#32).
- Fixed `Schedule:Week:Compact` semantic corruption by keeping day-type groups
  paired with their `Schedule:Day` IDs when moving `AllOtherDays` to the final
  field group, and anchored converted annual run periods to DeST's Monday-first
  calendar (#32).
- Added a traceable typical-storey approximation for multiplied DeST storeys,
  pairing lower and upper repeated-floor boundaries at equal EnergyPlus zone
  multipliers while making the first/top cut interfaces adiabatic (#32).
- Added `to_epw()` to convert a uniquely selected, complete DeST
  `CLIMATE_DATA` year into an EnergyPlus weather object, including
  relative-humidity and solar-radiation derivations (#32).
- Preserved aggregate window area across partitioned host surfaces by choosing
  window-aware surface triangulations and applying an infinitesimal inward
  boundary offset accepted by EnergyPlus geometry checks (#32).
- Preserved reciprocal DeST construction direction by emitting explicit
  reversed constructions for room-to-peer `SIDE1` surfaces and interzone
  windows (#32).
- Preserved distinct opaque and transparent door constructions, including
  thickness-dependent material identities and transparent-door material lookup
  through DeST application identifiers (#32).
- Added humidity schedule and control conversion for `ZoneControl:Humidistat`
  (#32).
- Preserved positive minimum people, lighting, and electric-equipment gains as
  separate always-on objects, and rejected source minimums that exceed their
  corresponding maximum values (#32).
- Made DeST name normalization dependency-aware and prefixed storeys and rooms
  with their owning building in multi-building models, keeping converted
  EnergyPlus references unique and resolvable (#32).
- Hardened Access-to-SQLite failure cleanup so failed ODBC or MDBTools attempts
  close partially created SQLite targets before fallback or error propagation
  (#32).
- Added dedicated `Schedule:Week:Compact` generation when December 31 has a
  unique daily profile, allowing `schedule__convert_week()` to preserve
  schedules that cannot reuse one of the first 52 weeks (#30).
- Preserved DeST aggregate window thermal and optical performance by converting
  `WINDOW_TYPE_DATA` records to `WindowMaterial:SimpleGlazingSystem` objects,
  with targeted `SYS_WINDOW` fallback handling for unavailable or invalid type
  data (#28).
- Ignored missing and zero-valued DeST schedule references during conversion,
  preventing nullable reserved fields from producing invalid SQL (#26).
- Preserved DeST enclosure geometry during EnergyPlus conversion by correcting
  surface types, outward normals, reciprocal boundary references, true-north
  rotation, EnergyPlus-tolerance vertex handling, shared-edge topology,
  reciprocal interzone windows, and windows crossing host partitions (#24).
- Fixed EnergyPlus simulation initialization for converted DeST models by adding
  an annual run period and normalizing schedule day types, surface polygons,
  material thicknesses, and zone thermostat coverage (#22).
- Added `GROUND_DATA` conversion to
  `Site:GroundTemperature:BuildingSurface` using monthly averages of the
  selected hourly ground-temperature series (#20).
- Added occupant outdoor-air conversion from `OCCUPANT_GAINS.MIN_REQUIRE_FRESH_AIR`
  to `DesignSpecification:OutdoorAir`, with IdealLoads systems referencing the
  converted outdoor-air objects (#19).
- Added `ROOM_GROUP` ideal-loads conversion to create
  `ZoneHVAC:IdealLoadsAirSystem` zone equipment using DeST air-conditioning
  availability schedules (#18).
- Added `ROOM_GROUP` thermostat/setpoint conversion to shared
  `ThermostatSetpoint:DualSetpoint` objects and per-zone
  `ZoneControl:Thermostat` controls (#17).
- Added CI coverage for the full real DeST ACCDB to SQLite to EnergyPlus IDF
  conversion path using a cached GitHub release fixture (#16).
- Fixed `Schedule:Week:Compact` day schedule references generated from
  `SCHEDULE_YEAR`, preventing missing `Schedule:Day Name` values in converted
  EnergyPlus schedules (#15).
- Added `ROOM_RELATION` outdoor ventilation conversion to
  `ZoneVentilation:DesignFlowRate`, using the referenced air-change schedule and
  keeping inter-zone mixing deferred (#14).
- Added normalized DeST schema catalog TSV assets and schema coverage diagnostics
  for comparing real SQLite models against the catalog (#10).
- Cataloged `ROOM_TYPE_DATA` as the room-type template table behind `ROOM.TYPE`,
  including internal-gain defaults and setpoint schedule metadata (#11).
- Cataloged `ROOM_RELATION` as the observed room ventilation/air-exchange
  relation table, using Access field descriptions as the field-semantics source
  of truth (#12).
- Added `fields_cn.tsv` with Access field descriptions extracted from a real
  DeST model, and refreshed English field semantics from those comments (#13).
