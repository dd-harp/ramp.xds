# Change the deaths

Modify the death rates for a model with static population rates. The
birth rate gets adjusted to ensure the \\dH/dt=0\\

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

an **`xds`** object
