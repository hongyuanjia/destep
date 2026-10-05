# Convert DeST climate data to an EnergyPlus weather object

`to_epw()` converts the selected `CLIMATE_DATA` hourly series in a DeST
model to an eplusr `Epw` object. The returned object stays in memory;
call its `$save()` method to write an EPW file.

## Usage

``` r
to_epw(dest, radiation_time = c("centered", "hour_start", "auto"))
```

## Arguments

- dest:

  A DBI connection, a path to a SQLite database produced by
  [`read_dest()`](read_dest.md), or a path to a DeST Access
  `.accdb`/`.mdb` file.

- radiation_time:

  Solar interval represented by each source radiation record.
  `"centered"` (the existing default approximation) uses
  `[HOUR - 0.5, HOUR + 0.5]`. `"hour_start"` uses `[HOUR, HOUR + 1]`,
  for hourly interval data imported with zero-based hour indices, such
  as the ASHRAE 140 TF models prepared from TMY3. `"auto"` evaluates
  both intervals and selects one only when it alone passes the sunlight
  and DNI checks. If both pass or both fail, conversion stops with both
  diagnostics. Explicit choices should follow source provenance.

## Value

An
[`eplusr::Epw`](https://hongyuanjia.github.io/eplusr/reference/Epw.html)
object. The `destep_audit` attribute records input repairs and radiation
diagnostics.

## Details

DNI is estimated from horizontal beam radiation and the mean positive
sine of solar altitude over the selected interval. GHI and DHI alone do
not uniquely recover measured hourly DNI; the estimate assumes constant
DNI during the sunlit part of the interval. No radiation values are
shifted or interpolated, and output rows retain the EPW hours 1 through
24. The centered convention retains the existing conversion
approximation; its solar support is half an hour earlier than the EPW
record interval. Inconsistent positive beam radiation without sunlight
and DNI above 1500 W/m2 are rejected rather than discarded or clipped.

Automatic selection is an inference from physical consistency, not proof
of the source timestamp definition. It never selects the smaller DNI or
the candidate with fewer failures. With no discriminating radiation
(including an all-zero year), both candidates can pass and an explicit
choice is needed. The audit records the requested and selected
conventions, selection reason, and candidate diagnostics.
Automatic-selection errors inherit from `destep_radiation_time_error`
and expose these diagnostics as `$candidates`.
