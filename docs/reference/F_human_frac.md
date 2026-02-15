# Compute the blood qeeding rate, q

This method dispatches on the type of `q_obj`. It should set the values
oq the bionomic parameters to baseline values

## Usage

``` r
F_human_frac(t, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector oq length
`nPatches`
