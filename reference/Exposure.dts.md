# Compute Attack Rates

For `dts` models, compute the attack rates, the expected probabilities
of infection over one time step, from the time-weighted local and travel
FoI.

## Usage

``` r
# S3 method for class 'dts'
Exposure(t, y, xds_obj)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** model object

## Details

Local attack rates (AR or \\\alpha\\) are a function of local daily
entomological inoculation rates (dEIR or \\E\\), and a model for the
probability of infection per infectious bite, \\b\\ (from
\\F_infectivity\\, as defined by an \\\cal X\\ model). The total attack
rates are computed from the time-weighted local and travel FoI, using
the travel EIR, \\E\_\delta\\: \$\$\alpha = 1-\exp(-((1-\delta)
F_h(E,b) + \delta F_h(E\_\delta,b)))\$\$

## See also

Related:
[Exposure](https://dd-harp.github.io/ramp.xds/reference/Exposure.md) &
[F_ar.pois](https://dd-harp.github.io/ramp.xds/reference/F_ar.pois.md) &
[F_ar.nb](https://dd-harp.github.io/ramp.xds/reference/F_ar.nb.md) &
[xds_info_travel_malaria](https://dd-harp.github.io/ramp.xds/reference/travel_malaria.md)
