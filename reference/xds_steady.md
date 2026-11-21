# Solve for the steady state

Compute the steady state for a system of differential equations.

- Run for \\Y\\ years

- Check whether the sum of squared differences between successive final
  states is less than `tol`

- If not, repeat the run up to ten more times using the latest final
  state as the new initial state.

- Stop after ten additional runs, even if the tolerance has not been
  reached.

## Usage

``` r
xds_steady(xds_obj, Y = 10, tol = 1e-05)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- Y:

  the number of years to run once

- tol:

  the desired accuracy

## Value

an **`xds`** model object
