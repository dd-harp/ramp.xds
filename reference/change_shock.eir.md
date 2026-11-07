# Change shock parameters

Change parameters for the shock function when `forced_by = "eir"`

## Usage

``` r
# S3 method for class 'eir'
change_shock(shock_par, xds_obj, s = 1)
```

## Arguments

- shock_par:

  parameters for
  [ramp.func::make_function](https://dd-harp.github.io/ramp.func/reference/make_function.html)

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

an **`xds`** object
