# Download a typical building model from the DeST website

Download a typical building model from the DeST website

## Usage

``` r
download_dest_model(building_type, location, year, dir = ".")
```

## Arguments

- building_type:

  \[character\] Building type to download.

- location:

  \[character\] Location of the building.

- year:

  \[integer\] Year of the building model.

- dir:

  \[string\] Directory to save the file. Default is the current workding
  directory.

## Value

\[character\] Path to the downloaded file.

## Details

The downloaded `7z` file will be unachieved using the `{archive}`
package.

## Note

Information about all available models can be retrieved by sending a
`GET` request to `https://svr.dest.net.cn/api/v1/all_model_names`.

## Examples

``` r
if (FALSE) { # \dontrun{
download_typical_dest("Commerical office A", "Chongqin", 2015)
} # }
```
