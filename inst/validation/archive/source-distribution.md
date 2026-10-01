# Historical validation: retired DeST source-distribution adapter

This document describes an earlier implementation and is retained as a record
of the experiments. The `source_distribution`, `exterior_boundary`, weather
prepass and `"dest"` preset it names were removed in the 2026-10-02 conversion
scope cleanup. The example below is not a supported current API call. Current
source inputs use native EnergyPlus distribution and exterior heat balance.

## Original record

The optional `source_distribution = "dest"` mode preserves source heat-input
fractions. It is separate from EnergyPlus's default surface allocation and from
the choice of interzone heat-transfer boundary. The current implementation is
limited to EnergyPlus 26.1, ideal loads, ordinary aggregate exterior windows,
and the explicitly checked input combinations described in `destep_opts()`.

## Evidence and implementation decisions

| Input or behavior | Evidence | Conversion decision |
| --- | --- | --- |
| User-specified solar and internal-gain distribution | Zhu, Hong, Yan and Wang (2012), *Comparison of Building Energy Modeling Programs: Building Loads*, LBNL-6034E, sections 3.2.4–3.2.5, Tables 3.5–3.6 | Read the actual `DIST_MODE` records; do not substitute the paper's default values. |
| Prescribed radiation on both sides of a partition | Xie et al. (2004), *DeST (2): Dynamic thermal process of buildings*, HVAC 34(8), p. 36 Table 1 and pp. 37–39 heat-balance equations | Retain each room's own prescribed radiation. The general equations alone do not prove the installed solver's coupling algorithm. |
| Literal totals below one; category weights times net area | Independent DeST 0.2.230705 directed-source and TF960 fraction checks, archived 2026-09-29 | Normalize recipient weights only. Preserve the source radiant total and air fraction without filling any unassigned remainder. |
| Furniture as a storage slab | Xie et al. (2004), p. 40, section 2.1.2; independent installed-engine capacity and free-floating checks | Keep furniture geometry/materials in `conv-furniture.R`. Use the separately checked small positive absorptance in the optional source mode; do not treat this numerical value as a value from the paper. |
| Neighbor-air plus prescribed-radiation boundary | Independent two-room installed-engine checks; the 2004 paper identifies the input terms but does not specify this complete implementation | Default to EnergyPlus's coupled surfaces. Apply the version-specific `T_neighbor + Q / (A h)` representation only when explicitly selected by `partition_boundary = "dest_air"`. |
| Sky exchange and transient load integration | Distinct boundary inputs and solver algorithms | No additional sky model or first-step load-fitting correction is enabled by source distribution. |

People, lighting and equipment converters retain ownership of their powers,
minimum/variable schedules, electricity, moisture and source metadata.
`conv-source.R` calculates common recipient shares and checks completeness.
`conv-surface.R` derives net target receiving areas, including clipped window
pieces; `conv-solar.R` owns window source identities and optical inputs.
`conv-solar-prepass.R` owns execution and cache verification.

## Public workflow

```r
idf <- to_eplus(
    dest,
    "26.1",
    options = destep_opts(
        "objects",
        people_heat = "temperature_dependent",
        window_optics = "dest_solar",
        source_distribution = "dest",
        weather = "model.epw",
        directory = "model-source-data"
    )
)
```

The prepass uses the converted geometry, glazing, shading, controls and weather
to calculate transmitted solar at each window. It never reads native DeST loads
or temperatures. Each window's original distribution follows it through grouping
and clipping. The main model prevents automatic retransmission and supplies the
prescribed heat exactly once, using signed surface corrections where needed.

Cache identity includes all converted fields, window source metadata, file
contents, executable/IDD contents and prepass implementation. Reuse also checks
the stored data bytes and complete nonnegative five-minute sequences. Failed
prepasses retain their diagnostics and cannot become reusable cache entries.
File dependencies are rechecked after execution. Identical schedule columns are
stored once, with separate named schedule objects pointing to that column.

The result is a normal `eplusr::Idf`, not a live weather-bound simulation wrapper.
**Reconvert when changing weather, geometry, optics, schedules or timestep.**
Editing or running the returned object cannot automatically refresh its external
solar schedules. Keep the generated directory, or use the standard external-file
copying option when saving the IDF. `attr(idf, "source_distribution")` records the
source/face inventory, selected partition representation and cache provenance.

## Validation scope

Unit tests cover literal source fractions, net receiving areas, independently
specified window modes, corrupted/stale caches, SQL time-grid completeness and
unsupported combinations. Real-model conversion and execution records belong in
the research archive. Neither successful IDF validation nor a successful engine
exit establishes DeST/EnergyPlus whole-building numerical equivalence. ASHRAE 140
acceptance and paired source-model comparison remain separate evaluations.
