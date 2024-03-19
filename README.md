
<!-- README.md is generated from README.Rmd. Please edit that file -->

# destep

<!-- badges: start -->

[![R-CMD-check](https://github.com/hongyuanjia/destep/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hongyuanjia/destep/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

> A toolkit to convert ‘DeST’ models to ‘EnergyPlus’ models

## Installation

You can install the development version of destep like so:

``` r
install.packages("destep",
    repos = c(
        hongyuanjia = "https://hongyuanjia.r-universe.dev",
        cran = "https://cran.r-project.org"
    )
)
```

## Supported DeST components

This package is still under heavy development and is not ready for use.
Currently, the following components are supported:

- [x] Geometry
- [x] Material
- [x] Construction
- [ ] Internal gains \[WIP\]
- [ ] Shading
- [ ] HVAC

## Get started

``` r
library(destep)

# use a DeST typical building model as an example
path <- download_dest_model("Commercial office A", "Chongqin", 2015, tempdir())

# make sure EnergyPlus IDD file can be found even if EnergyPlus itself was not
# installed
eplusr::use_idd(23.1, download = "auto")
#> IDD v23.1.0 has not been parsed before.
#> Try to locate 'Energy+.idd' in EnergyPlus v23.1.0 installation folder '/Applications/EnergyPlus-23-1-0'.
#> IDD file found: '/Users/hongyuanjia/Applications/EnergyPlus-23-1-0/Energy+.idd'.
#> Start parsing...
#> Parsing completed.
#> ── EnergyPlus Input Data Dictionary ────────────────────────────────────────────
#> * Version: 23.1.0
#> * Build: 87ed9199d4
#> * Total Class: 840

# read the DeST model and convert it to an EnergyPlus model
read_dest(path) |> to_eplus(23.1)
#> There are windows in the input DeST model. However, the optical properties of the glazing in DeST are not one-to-one match with the glazing in EnergyPlus. Here the optical properties of a 3mm clear glazing in EnergyPlus dataset 'WindowGlassMaterials.idf' will be used for all glazing in DeST. Please check the results in the converted IDF file.
#> ── EnergPlus Input Data File ───────────────────────────────────────────────────
#>  • Path: NOT LOCAL
#>  • Version: '23.1.0'
#> 
#> Group: <Simulation Parameters>
#> ├─ [001<O>] Class: <Version>
#> └─ [001<O>] Class: <Building>
#> 
#> Group: <Location and Climate>
#> └─ [001<O>] Class: <Site:Location>
#> 
#> Group: <Surface Construction Elements>
#> ├─ [014<O>] Class: <Material>
#> │─ [001<O>] Class: <WindowMaterial:Glazing>
#> │─ [001<O>] Class: <WindowMaterial:Gas>
#> └─ [007<O>] Class: <Construction>
#> 
#> Group: <Thermal Zones and Surfaces>
#> ├─ [036<O>] Class: <Zone>
#> │─ [003<O>] Class: <ZoneList>
#> │─ [003<O>] Class: <ZoneGroup>
#> └─ [470<O>] Class: <BuildingSurface:Detailed>
```
