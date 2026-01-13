# Update State Variables for an **L** Component Module

This method updates the state variables for **L** Component modules

## Usage

``` r
Update_Lt(t, y, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

the states, a [numeric](https://rdrr.io/r/base/numeric.html) vector of
length \\n_q=\\`nHabitats`

## Note

Dispatches on the class of `xds_obj$L_obj[[s]]`
