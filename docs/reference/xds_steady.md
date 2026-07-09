# Solve for the steady state

Compute the steady state for a system of differential equations.

- Run for \\Y\\ years

- Check the sun of squared differences in the final state is less than
  `tol`

- If not

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

an **`xds`** object
