# Changelog

## destep 0.0.0.9000

- Reduced converted geometry fragmentation by preserving planar convex
  surfaces and rectangular windows, while making reciprocal
  interzone-window partitions deterministic and centralizing EnergyPlus
  geometry tolerances (#32).
- Fixed `Schedule:Week:Compact` semantic corruption by keeping day-type
  groups paired with their `Schedule:Day` IDs when moving `AllOtherDays`
  to the final field group, and anchored converted annual run periods to
  DeST’s Monday-first calendar (#32).
- Added a traceable typical-storey approximation for multiplied DeST
  storeys, pairing lower and upper repeated-floor boundaries at equal
  EnergyPlus zone multipliers while making the first/top cut interfaces
  adiabatic (#32).
- Added [`to_epw()`](../reference/to_epw.md) to convert a uniquely
  selected, complete DeST `CLIMATE_DATA` year into an EnergyPlus weather
  object, including relative-humidity and solar-radiation derivations
  (#32).
- Preserved aggregate window area across partitioned host surfaces by
  choosing window-aware surface triangulations and applying an
  infinitesimal inward boundary offset accepted by EnergyPlus geometry
  checks (#32).
- Preserved DeST construction layer direction by emitting explicit
  reversed constructions for `SIDE1` surfaces and reciprocal interzone
  windows (#32).
- Preserved distinct opaque and transparent door constructions,
  including thickness-dependent material identities and transparent-door
  material lookup through DeST application identifiers (#32).
- Added humidity schedule and control conversion for
  `ZoneControl:Humidistat`, while rejecting nonzero equipment moisture
  gains that do not yet have an equivalent EnergyPlus mapping (#32).
- Preserved positive minimum people, lighting, and electric-equipment
  gains as separate always-on objects, and rejected source minimums that
  exceed their corresponding maximum values (#32).
- Made DeST name normalization dependency-aware and prefixed storeys and
  rooms with their owning building in multi-building models, keeping
  converted EnergyPlus references unique and resolvable (#32).
- Hardened Access-to-SQLite failure cleanup so failed ODBC or MDBTools
  attempts close partially created SQLite targets before fallback or
  error propagation (#32).
- Added dedicated `Schedule:Week:Compact` generation when December 31
  has a unique daily profile, allowing `schedule__convert_week()` to
  preserve schedules that cannot reuse one of the first 52 weeks (#30).
- Preserved DeST aggregate window thermal and optical performance by
  converting `WINDOW_TYPE_DATA` records to
  `WindowMaterial:SimpleGlazingSystem` objects, with targeted
  `SYS_WINDOW` fallback handling for unavailable or invalid type data
  (#28).
- Ignored missing and zero-valued DeST schedule references during
  conversion, preventing nullable reserved fields from producing invalid
  SQL (#26).
- Preserved DeST enclosure geometry during EnergyPlus conversion by
  correcting surface types, outward normals, reciprocal boundary
  references, true-north rotation, EnergyPlus-tolerance vertex handling,
  shared-edge topology, reciprocal interzone windows, and windows
  crossing host partitions (#24).
- Fixed EnergyPlus simulation initialization for converted DeST models
  by adding an annual run period and normalizing schedule day types,
  surface polygons, material thicknesses, and zone thermostat coverage
  (#22).
- Added `GROUND_DATA` conversion to
  `Site:GroundTemperature:BuildingSurface` using monthly averages of the
  selected hourly ground-temperature series (#20).
- Added occupant outdoor-air conversion from
  `OCCUPANT_GAINS.MIN_REQUIRE_FRESH_AIR` to
  `DesignSpecification:OutdoorAir`, with IdealLoads systems referencing
  the converted outdoor-air objects (#19).
- Added `ROOM_GROUP` ideal-loads conversion to create
  `ZoneHVAC:IdealLoadsAirSystem` zone equipment using DeST
  air-conditioning availability schedules (#18).
- Added `ROOM_GROUP` thermostat/setpoint conversion to shared
  `ThermostatSetpoint:DualSetpoint` objects and per-zone
  `ZoneControl:Thermostat` controls (#17).
- Added CI coverage for the full real DeST ACCDB to SQLite to EnergyPlus
  IDF conversion path using a cached GitHub release fixture (#16).
- Fixed `Schedule:Week:Compact` day schedule references generated from
  `SCHEDULE_YEAR`, preventing missing `Schedule:Day Name` values in
  converted EnergyPlus schedules (#15).
- Added `ROOM_RELATION` outdoor ventilation conversion to
  `ZoneVentilation:DesignFlowRate`, using the referenced air-change
  schedule and keeping inter-zone mixing deferred (#14).
- Added normalized DeST schema catalog TSV assets and schema coverage
  diagnostics for comparing real SQLite models against the catalog
  (#10).
- Cataloged `ROOM_TYPE_DATA` as the room-type template table behind
  `ROOM.TYPE`, including internal-gain defaults and setpoint schedule
  metadata (#11).
- Cataloged `ROOM_RELATION` as the observed room
  ventilation/air-exchange relation table, using Access field
  descriptions as the field-semantics source of truth (#12).
- Added `fields_cn.tsv` with Access field descriptions extracted from a
  real DeST model, and refreshed English field semantics from those
  comments (#13).
