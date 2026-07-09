# DeST schema catalog

This directory records observed DeST database tables, inferred semantics, and
current EnergyPlus conversion coverage.

The catalog is split into four tab-separated files:

- `tables.tsv`: one row per DeST table, including category, conversion status,
  EnergyPlus mapping, and table-level notes.
- `fields.tsv`: one row per DeST field, including its table, original order,
  role, and English field-level semantics.
- `fields_cn.tsv`: one row per Access field description extracted from a real
  source model. This is the source-of-truth asset for field semantics when a
  DeST developer comment is available.
- `observations.tsv`: one row per source model and table, currently used for
  fixture-specific row counts and notes.

These files are plain text and read with base R, so the catalog adds no package
dependency. The normalized layout is intentional: reviewing and updating one
table or field should produce a small line-based diff, not a large nested list
change.

When Access inspection or fixture analysis clarifies a table, update these
files instead of leaving that knowledge only in converter comments.

## Catalog updates

- Outdoor-ventilation rows in `ROOM_RELATION` are converted to
  `ZoneVentilation:DesignFlowRate` when `RELA_ROOM_ID` references `OUTSIDE`.
  Inter-zone mixing remains deferred until adjacent-room semantics are verified.
- Access field descriptions were extracted to `fields_cn.tsv` in PR #13, and
  `fields.tsv` was refreshed so English field semantics are derived from those
  source comments.
- `ROOM_RELATION` was cataloged in PR #12 as the observed room ventilation or
  air-exchange relation table, using Access field descriptions for field
  semantics.
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
- Treat Access field `Description` comments as the primary source for field
  semantics when they are available.
- Keep `fields_cn.tsv` as the extracted Chinese source text. Keep `fields.tsv`
  in English, translated or summarized from the Chinese source text.
- Regenerate `fields_cn.tsv` with
  `Rscript tools/extract-cn.R path/to/model.accdb`.
- Keep reviewed Chinese-to-English field-description terms in
  `tools/field-terms.tsv`.
- Refresh English field semantics with `Rscript tools/sync-fields.R` after
  reviewing any new Chinese descriptions.
- Put fixture observations and inferred mappings in `notes`, and mark
  uncertainty instead of promoting guesses to `semantics`.
- Add row counts only to `observations.tsv`, and only for real fixtures that
  can be checked in tests.
- Use `notes` for uncertainty; do not delete uncertain entries just because the
  mapping is incomplete.
- When adding a converter, update the table status, `energyplus` mapping, and
  field semantics in the same PR as the converter.

Use `Rscript data-raw/check-dest-schema.R path/to/model.sql` from the package
root to compare this catalog with another converted DeST SQLite model.
