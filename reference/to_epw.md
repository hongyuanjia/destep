# Convert DeST climate data to an EnergyPlus weather object

`to_epw()` converts the selected `CLIMATE_DATA` hourly series in a DeST
model to an eplusr `Epw` object. The returned object stays in memory;
call its `$save()` method to write an EPW file.

## Usage

``` r
to_epw(dest)
```

## Arguments

- dest:

  A DBI connection, a path to a SQLite database produced by
  [`read_dest()`](read_dest.md), or a path to a DeST Access
  `.accdb`/`.mdb` file.

## Value

An
[`eplusr::Epw`](https://hongyuanjia.github.io/eplusr/reference/Epw.html)
object. The `destep_audit` attribute records input repairs and radiation
diagnostics.
