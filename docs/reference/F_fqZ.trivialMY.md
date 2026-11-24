# Infective mosquito biting rate for `trivialMY`

Returns \$\$F\_{fqZ}(t) = fqZ F_S(t) F_T(t) F_K(t)\$\$ where

- \\(f\\) is the feeding rate

- \\(q\\) is the human fraction

- \\(Z\\) is the density of infectious mosquitoes, per patch

- \\(F_S(t)\\) or `F_season` is a seasonal pattern

- \\(F_T(t)\\) or `F_trend` is a trend pattern

- \\(F_K(t)\\) or `F_shock` is a perturbation

- `season_par` — parameters used by
  [get_variables](https://dd-harp.github.io/ramp.xds/reference/get_variables.md)
  to obtain \\(V_s\\)

- `trend_par` — parameters used by
  [get_variables](https://dd-harp.github.io/ramp.xds/reference/get_variables.md)
  to obtain \\(V_t\\)

- `shock_par` — parameters used by
  [get_variables](https://dd-harp.github.io/ramp.xds/reference/get_variables.md)
  to obtain \\(V_k\\)

## Usage

``` r
# S3 method for class 'trivialMY'
F_fqZ(t, y, xds_obj, s)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`
