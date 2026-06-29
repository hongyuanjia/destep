# DeST schema catalog

This directory records observed DeST database tables, inferred semantics, and
current EnergyPlus conversion coverage.

The catalog is split into three tab-separated files:

- `tables.tsv`: one row per DeST table, including category, conversion status,
  EnergyPlus mapping, and table-level notes.
- `fields.tsv`: one row per DeST field, including its table, original order,
  role, and field-level semantics.
- `observations.tsv`: one row per source model and table, currently used for
  fixture-specific row counts and notes.

Both files are plain text and read with base R, so the catalog adds no package
dependency. The normalized layout is intentional: reviewing and updating one
table or field should produce a small line-based diff, not a large nested list
change.

When Access inspection or fixture analysis clarifies a table, update these
files instead of leaving that knowledge only in converter comments.

## Catalog updates

- `ROOM_TYPE_DATA` was cataloged in PR #11 as the room-type template source for
  internal-gain defaults and setpoint schedule metadata.

## Status values

- `converted`: a converter consumes the table directly and tests cover it.
- `supporting`: a converter joins or depends on the table.
- `planned`: a likely near-term converter target.
- `observed`: meaning is partly known, but no converter is planned yet.
- `deferred`: keep visible, but postpone implementation until a useful model
  fixture exists.

## Maintenance rules

- Keep table names and field names exactly as they appear in the DeST SQLite
  database.
- Add row counts only to `observations.tsv`, and only for real fixtures that
  can be checked in tests.
- Use `notes` for uncertainty; do not delete uncertain entries just because the
  mapping is incomplete.
- When adding a converter, update the table status, `energyplus` mapping, and
  field semantics in the same PR as the converter.

Use `Rscript data-raw/check-dest-schema.R path/to/model.sql` from the package
root to compare this catalog with another converted DeST SQLite model.
