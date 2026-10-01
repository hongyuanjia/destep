# Retired DeST-specific solar-window reconstruction

The `solar-optics-reference.csv` values were used by the removed
`R/conv-solar.R` and `tests/testthat/test-conv-solar.R` to check a reconstruction
of DeST's simplified-window angular solar algorithm. The files remain in Git
history at commit `6936b2d`; this CSV is retained as historical evidence, not
as a current converter fixture or a validation of aggregate K/SC semantics.

The reconstruction inferred a normal-incidence transmittance target from
`0.87 * SC`, searched a refractive index, generated angle tables, and placed
absorbed solar heat according to a DeST-specific equivalent-layer rule. Those
steps reproduced source solver behavior rather than transferring uniquely
defined window properties. Current conversion uses an explicitly diagnosed
SimpleGlazing approximation; its unresolved two-face `SURFACE.BLACKNESS`
inputs are recorded in the conversion audit and saved IDF comments.

For the decision and limitations, see the research archive's
`analysis/conversion-scope-review-20261001-v1/step-04b-window-policy.md`.
