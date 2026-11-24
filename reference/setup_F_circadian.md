# Setup a circadian pattern function

Set the circadian activity function for vector species \\s\\.

## Usage

``` r
setup_F_circadian(F_circadian, xds_obj, s = 1)
```

## Arguments

- F_circadian:

  a circadian function or a method name (*e.g.* "setup")

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

an **`xds`** model object

## Circadian Pattern Function

A circadian pattern function is a model for relative mosquito activity
by time of day, \\F_c(t)\\. On average, it should be true that
\$\$\int\_{t}^{t+1} F_c(s) ds \approx 1.\$\$ A circadian function is
used to weight time spent by time of day to compute time at risk.

## See also

[F_TaR](https://dd-harp.github.io/ramp.xds/reference/F_TaR.md)
