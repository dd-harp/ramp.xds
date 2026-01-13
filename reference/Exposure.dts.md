# Compute Attack Rates

For `dts` models, compute the attack rates, the expected probability of
infection over one time step, is a weighted sum of local and travel
exposure.

## Usage

``` r
# S3 method for class 'dts'
Exposure(t, y, xds_obj)
```

## Arguments

- t:

  the time

- y:

  the state variables

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** object

## Details

Local attack rates (AR or \\\alpha\\) are a function of local daily
entomological inoculation rates (dEIR or \\E\\), and a model for the
probability of infection per infectious bite, \\b\\ (from
\\F_infectivity\\, as defined by an \\\cal X\\ model). The total attack
rates also consider exposure to malaria while traveling,
(\\T\_\delta\\): \$\$\alpha = 1-((1-\delta) \\ F\_\alpha(E,
b))(1-\delta\\ T\_\alpha(b))\$\$

## See also

Related:
[Exposure](https://dd-harp.github.io/ramp.xds/reference/Exposure.md) &
[F_ar.pois](https://dd-harp.github.io/ramp.xds/reference/F_ar.pois.md) &
[F_ar.nb](https://dd-harp.github.io/ramp.xds/reference/F_ar.nb.md) &
[Travel](https://dd-harp.github.io/ramp.xds/reference/Travel.md)
