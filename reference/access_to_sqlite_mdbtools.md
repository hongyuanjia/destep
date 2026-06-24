# Convert Microsoft Access database to SQLite using mdbtools

Convert Microsoft Access database to SQLite using mdbtools

## Usage

``` r
access_to_sqlite_mdbtools(
  accdb,
  sqlite = ":memory:",
  tables = NULL,
  drop = TRUE,
  verbose = FALSE
)
```

## Arguments

- accdb:

  \[string\] Path to the DeST model file. Usually a Microsoft Access
  database file with `.accdb` extension.

- sqlite:

  \[string\] Path to the SQLite database file. If `:memory:`, which is
  the default, a temporary in-memory database will be created.

- tables:

  \[character\] Vector of table names to read from the DeST model. If
  `NULL`, which is the default, all tables will be read.

- drop:

  \[logical\] Whether to drop tables in the SQLite database if they
  already exist. Default is `TRUE`.

- verbose:

  \[logical\] Whether to print information about the conversion process.
  Default is `FALSE`.

## Note

This function requires 'mdbtools' installed and can be found in PATH,
which means that it only works on macOS and Linux. For Windows, use
[`access_to_sqlite_odbc()`](access_to_sqlite_odbc.md) instead.
