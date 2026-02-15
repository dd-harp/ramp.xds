# Compute the Force of Infection

For **`xde`** models, compute the FoI

## Usage

``` r
# S3 method for class 'xde'
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

The local force of infection (FoI or \\h\\) is a function of local daily
entomological inoculation rate (dEIR or \\E\\) under a model for the
probability of infection per infectious bite, \\b\\ (called from
\\F_infectivity\\, as defined by an \\\cal X\\ model). The total FoI is
a weighted sum of the local FoI and exposure to malaria while traveling,
computed from the the travel EIR, \\E_T\\: \$\$h = (1-\delta) \\ F_h(E,
b) + \delta\\ F_h(E_T, b,t)\$\$

## See also

Related:
[Exposure](https://dd-harp.github.io/ramp.xds/reference/Exposure.md) &
[F_foi.pois](https://dd-harp.github.io/ramp.xds/reference/F_foi.pois.md)
& [F_foi.nb](https://dd-harp.github.io/ramp.xds/reference/F_foi.nb.md) &
[Travel](https://dd-harp.github.io/ramp.xds/reference/Travel.md)
