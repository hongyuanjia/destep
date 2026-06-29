# Summarize DeST schema coverage for a SQLite model

`destep_schema_coverage()` compares the tables and fields in a DeST
SQLite database against the schema catalog shipped with destep. It is
intended as a development diagnostic for tracking which DeST tables are
cataloged, which non-empty tables are not cataloged yet, and whether
cataloged fields drift from real models.

## Usage

``` r
destep_schema_coverage(dest)
```

## Arguments

- dest:

  A DBI connection or a path to a SQLite database produced from a DeST
  model. Access `.accdb` and `.mdb` files are not read directly; convert
  them first with [`read_dest()`](read_dest.md).

## Value

A named list with two data frames:

- `tables`:

  Table-level coverage, with one row for each table in the database or
  `tables.tsv`.

- `fields`:

  Field-level coverage, with one row for each field in the database or
  `fields.tsv`.
