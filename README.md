
<!-- README.md is generated from README.Rmd. Please edit that file -->

# destep

<!-- badges: start -->

[![R-CMD-check](https://github.com/hongyuanjia/destep/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hongyuanjia/destep/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

> A toolkit to convert [DeST](https://www.dest.net.cn/) models to
> [EnergyPlus](https://energyplus.net/) models

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

## Supported conversion boundary

`destep` is under active development. The default conversion currently
preserves the following DeST components:

- Geometry, opaque constructions, doors, and windows
- Material thermal properties and aggregate or detailed glazing
  properties
- Hourly schedules from `SCHEDULE_YEAR`
- Outdoor ventilation from `ROOM_RELATION`
- Thermostat setpoints and Ideal Loads zone equipment from `ROOM_GROUP`
- Internal gains and occupant outdoor-air requirements
- Ground temperatures, site metadata, and ground reflectance
- Exterior window overhangs and side fins
- Climate data through `to_epw()`

The opt-in `hvac = "physical"` mode supports one-room `AC_SYS_TYPE = 0`
constant-volume systems and exactly two conditioned rooms linked to a
shared type-0 constant-volume or type-1 VAV terminal-reheat system.
Supported systems may use `FRESH_AIR_TYPE` 1, 5, or 6. Equipment and
zone outdoor-air parameters that are absent from the DeST model must be
provided explicitly through `hvac_options`; see `?to_eplus` for the
required fields. Other physical topologies are not projected, and source
models with an `AC_SYS_TYPE` other than 0 or 1 stop with an explicit
error.

## EnergyPlus versions

The reproducible conversion baseline is
[EnergyPlus](https://energyplus.net/) 9.0.1. The default load-only
conversion can request versions supported by
[eplusr](https://cran.r-project.org/package=eplusr), with compatibility
warnings where a version-specific behavior has not been verified. The
physical HVAC paths currently require EnergyPlus 9.0.1. A converted
9.0.1 model can be transitioned to a newer EnergyPlus version with
`eplusr`. Aggregate `WindowMaterial:SimpleGlazingSystem` output
targeting EnergyPlus 9.0 through 9.3 emits a warning because EnergyPlus
corrected the relevant angular-reflectance behavior in version 9.4.

## Get started

``` r
library(destep)

# use a DeST typical building model as an example
path <- download_dest_model("Commercial office A", "Chongqin", 2015, tempdir())

# make sure EnergyPlus IDD file can be found even if EnergyPlus itself was not
# installed
eplusr::use_idd("9.0.1", download = "auto")

# read once, then create the EnergyPlus input and weather objects
dest <- read_dest(path)
idf <- to_eplus(dest, "9.0.1")
epw <- to_epw(dest)

idf$save("model.idf")
epw$save("weather.epw")
```
