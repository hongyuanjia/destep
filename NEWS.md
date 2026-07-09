# destep 0.0.0.9000

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
