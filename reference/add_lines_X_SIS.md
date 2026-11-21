# Add lines for the density of infected individuals for the `SIS` model

Add lines for the density of infected individuals for the `SIS` model

## Usage

``` r
add_lines_X_SIS(time, XH, nStrata, clrs = c("darkblue", "darkred"), llty = 1)
```

## Arguments

- time:

  time points for the observations

- XH:

  parsed outputs from
  [parse_XH_orbits.SIS](https://dd-harp.github.io/ramp.xds/reference/parse_XH_orbits.SIS.md)

- nStrata:

  the number of population strata

- clrs:

  a vector of colors

- llty:

  an integer (or integers) to set the `lty` for plotting

## See also

[SIS](https://dd-harp.github.io/ramp.xds/reference/SIS.md)
