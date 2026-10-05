# Convert a DeST model to EnergyPlus model

Convert a DeST model to EnergyPlus model

## Usage

``` r
to_eplus(
  dest,
  ver = "latest",
  copy = TRUE,
  verbose = FALSE,
  options = "objects"
)
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
  inputs, HVAC and target simulation settings.

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

## Examples

``` r
if (FALSE) { # \dontrun{
to_eplus(dest, "23.1", options = "objects")
opts <- destep_opts("objects", terrain = "Country")
to_eplus(dest, "23.1", options = opts)
} # }
```
