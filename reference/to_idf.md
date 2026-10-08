# Convert a DeST model to EnergyPlus model

Convert a DeST model to EnergyPlus model

## Usage

``` r
to_idf(dest, ver = "latest", copy = TRUE, verbose = FALSE, options = "objects")
```

## Arguments

- dest:

  A \[string or DBIConnection\] path to a DeST model file or a
  DBIConnection object.

- ver:

  \[string\] A character string specifying the EnergyPlus version. It
  can be `"latest"`, which is the default, to indicate using the latest
  EnergyPlus version supported by the
  {[eplusr](https://cran.r-project.org/package=eplusr)} package. Objects
  are generated using the project's EnergyPlus 9.0.1 baseline. Earlier
  targets are not maintained. Effective moisture raises the generation
  baseline to 9.1 and requires a target of at least 9.1. Higher targets
  are produced with
  [`eplusr::transition()`](https://hongyuanjia.github.io/eplusr/reference/transition.html),
  not separate object writers. Physical HVAC requires the generation
  version's local ExpandObjects; target and intermediate IDDs are
  resolved by eplusr for transition. The generation/target versions are
  recorded in the conversion audit. Geometry compatibility has been
  validated against EnergyPlus 23.1; other versions currently reuse that
  profile with an explicit warning.

- copy:

  \[logical\] Whether to copy the input DeST database to a temporary
  SQLite database. Note that if `FALSE`, the input database will be
  modified during the conversion. Default is `TRUE`.

- verbose:

  \[logical\] Whether to show verbose messages. Default is `FALSE`.

- options:

  \[string or destep_options\] Conversion configuration. Use `"objects"`
  (default) or [`destep_opts()`](destep_opts.md) to configure source
  inputs, time tables and HVAC representation.

## Value

\[eplusr::Idf\] The converted EnergyPlus model. The `conversion`
attribute and Version comments record the selected options, resolved
HVAC representation, source component record counts, and necessary EMS
programs. Its `windows` table records the source K/SC, nominal SHGC,
face blackness and unresolved optical properties for each window. The
same aggregate assumptions appear in the saved IDF glazing comments.
This does not establish whole-building equivalence.

## Details

Outdoor ventilation retains the source minimum ACH time table. A saved
`OPTION.VARIANT_VENT = 0` disables the range supplement. When the saved
switch is absent, conversion retains its legacy documented
outdoor-temperature-band rule and warns about the assumed enabled
setting. The `ventilation` attribute records this selection. The rule
preserves range inputs but does not reproduce DeST's internal
ventilation control algorithm.

Storey multipliers are mapped to ZoneGroup independently of source
surface boundaries. Outdoor, ground and interzone relationships are
retained. Interzone pairs with unequal multipliers produce a warning and
are listed in `conversion$surface_boundaries`: weighted outputs must not
be interpreted as a physically balanced whole-building model. Successful
conversion does not establish DeST thermal equivalence. Missing
enclosure-side or middle-plane references produce a
`destep_invalid_surface_references` error whose `references` table
identifies the source enclosure, field and saved reference.

## Conversion scope

Source inputs are mapped to EnergyPlus objects with documented
equivalent representations where needed. Conversion does not reproduce
DeST solver algorithms or guarantee matching annual loads. Unsupported
selected HVAC equipment stops automatic/physical conversion with a
diagnostic; it is not replaced silently by IdealLoads. See
[`destep_opts()`](destep_opts.md) for supported subsets. Terrain, solar
distribution and shading-update settings retain EnergyPlus defaults.
Edit the returned
[eplusr::Idf](https://hongyuanjia.github.io/eplusr/reference/Idf.html)
to change target simulation settings.

## See also

[`to_epw()`](to_epw.md), [`destep_opts()`](destep_opts.md) for supported
inputs and HVAC limitations.

## Examples

``` r
if (FALSE) { # \dontrun{
to_idf(dest, "23.1", options = "objects")
opts <- destep_opts("objects", run_period = c(1L, 31L))
to_idf(dest, "23.1", options = opts)
} # }
```
