# Change the deaths

Modify the death rates. Birth rates are unchanged;
[setup_deaths.constant](https://dd-harp.github.io/ramp.xds/reference/setup_deaths.constant.md)
also adjusts births to balance deaths at the configured population
density.

## Usage

``` r
change_deaths(death_rates, xds_obj, i = 1)
```

## Arguments

- death_rates:

  death rates

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

an **`xds`** model object
