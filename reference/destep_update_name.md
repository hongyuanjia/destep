# Update NAME column in DeST tables

Update NAME column in DeST tables

## Usage

``` r
destep_update_name(dest, tables = NULL)
```

## Arguments

- dest:

  \[DBIConnection\] A SQLite database connection to the DeST model.

- tables:

  \[character\] Vector of table names to update. If `NULL`, which is the
  default, all tables will be updated.

## Value

\[DBIConnection\] The same database connection object.

## Details

In EnergyPlus, object names have to be unique in the scope of their
belonging class. However, in DeST, 'NAME' column is likely empty or '.'
for most table. This function can be called before actual conversion
starts to update the values in 'NAME' column and make sure the follow
EnergyPlus requirements.

The updating process consists of two steps:

1.  Fill empty name column with a prefix if the name is empty or '.'.
    The prefix is based on the table name.

2.  Add suffix to the names if there are duplicated names.
