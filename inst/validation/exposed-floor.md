# Exposed-floor exterior coefficients

`SURFACE.ABSORB_COEF = 0` on an exposed floor is preserved literally. The
converter must not borrow the absorptance of other exterior faces or retain
EnergyPlus's default material absorptance. Thermal absorptance still uses the
documented small positive bound required by the target IDD.

The 2012 LBNL comparison report, *Comparison of Building Energy Modeling
Programs: Building Loads* (LBNL-6034E), section 3.2.3, describes ground-reflected
solar radiation. It does not establish a special meaning for zero floor
coefficients. The decision above is supported by independent runs of the
installed DeST engine, bshell 0.2.230705, archived on 2026-09-30.

Sixteen single-room cases retained fixed room/outdoor temperatures and changed
absorptance, blackness, solar intensity or ground reflectance. The floor was an
`air_floor` with the explicit `absorb_gain` object and links used by an untouched
TF600 export. All process exits, unchanged input hashes and result hashes were
checked. Comparisons use the final week of 31-day simulations.

| Floor input | Predicted cooling increment | Observed increment |
| --- | ---: | ---: |
| Absorptance 0; diffuse solar 100 W/m2; ground reflectance 0.3 | 0 W | within 8e-12 W of zero |
| Absorptance 0.6; same radiation | 18.645949 W | 18.645949 W |
| Absorptance 0.6; ground reflectance 0 | 0 W | within 8e-12 W of zero |
| Absorptance 0.6; double reflectance or double solar intensity | 37.291898 W | 37.291898 W |

These distinguish literal zero from substitution with 0.6, and demonstrate
that a floor with positive absorptance can receive ground-reflected radiation.
Changing the floor's exterior blackness from 0 to 0.85 had no effect under the
checked zero-sky-view conditions; that observation does not identify its meaning
for other surface types or radiation paths.

The five wall controls confirmed active radiation forcing. Three positive-wall
cases did not satisfy the initial constant-wall-irradiance assumption and are
retained as failed steady predictions, not counted as full validation. The
floor checks do not establish whole-building DeST/EnergyPlus agreement or
ASHRAE 140 acceptance; those require separate public-converter regressions.
