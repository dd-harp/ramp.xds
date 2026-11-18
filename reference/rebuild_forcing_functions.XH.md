# Rebuild Forcing Functions

Rebuild forcing functions on the XH object using `make_function`

For \\\kappa\\

- `F_season` is made from `season_par`

- `F_trend` is made from `trend_par`

- `F_shock` is made from `shock_par`

For \\H\\

- `H_season` is made from `H_season_par`

- `H_trend` is made from `H_trend_par`

- `H_shock` is made from `H_shock_par`

## Usage

``` r
# S3 method for class 'XH'
rebuild_forcing_functions(xds_obj, ix)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- ix:

  the species index

## Value

an **`xds`** model object
