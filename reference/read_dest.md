# Read tables from a DeST model and convert to SQLite

Read tables from a DeST model and convert to SQLite

## Usage

``` r
read_dest(
  accdb,
  tables = NULL,
  sqlite = ":memory:",
  verbose = FALSE,
  drop = TRUE
)
```

## Arguments

- accdb:

  \[string\] Path to the DeST model file. Usually a Microsoft Access
  database file with `.accdb` extension.

- tables:

  \[character\] Vector of table names to read from the DeST model. If
  `NULL`, which is the default, all tables will be read.

- sqlite:

  \[string\] Path to the SQLite database file. If `:memory:`, which is
  the default, a temporary in-memory database will be created.

- verbose:

  \[logical\] Whether to print information about the conversion process.
  Default is `FALSE`.

- drop:

  \[logical\] Whether to drop tables in the SQLite database if they
  already exist. Default is `TRUE`.

## Value

\[DBIConnection\] A SQLite database connection with all specified
tables.

## Details

The function converts a Microsoft Access database (`.accdb` or `.mdb`)
to SQLite using the following approach:

1.  **Driver Selection**: The function tries to use ODBC drivers first:

    - On Windows: Primarily uses
      "`Microsoft Access Driver (*.mdb, *.accdb)`"

    - On macOS/Linux: Tries to find available drivers like
      "`MDBTools Driver`"

2.  **Fallback Mechanism**: If ODBC connection fails, on non-Windows
    systems, the function falls back to using MDBTools command-line
    utilities

3.  **Table Selection**:

    - If `tables = NULL`, all tables are read except for system tables
      (MSys\*)

    - Otherwise, only the specified tables are processed

4.  **Data Transfer**: For each table:

    - Optionally drops existing table in target SQLite database

    - Reads the entire table from Access

    - Writes the data to SQLite

5.  **Schema Handling**: Table structure (columns and types) is
    automatically preserved during the transfer process

For systems without proper ODBC drivers (especially some Linux
distributions), consider installing one of the following:

- `MDBTools`: Open-source utilities for reading Access databases

- `LibreOffice Base`: Provides ODBC drivers for Access

- Commercial drivers: `Actual Technologies` or `Easysoft ODBC drivers`

## Note

Tables with the same name will be overwritten in the SQLite database.
