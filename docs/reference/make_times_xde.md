# Times Utility for Differential Equations

This method sets up the time points for the independent variable to
return solutions for the dependent variables.

If `times` is not `NULL`, the function returns those values.

Otherwise, it returns a sequence from `0` to `Tmax` by `dt`.

## Usage

``` r
make_times_xde(Tmax, dt, times = NULL)
```

## Arguments

- Tmax:

  the last time point, run from 0...Tmax

- dt:

  the time interval for outputs

- times:

  the times

## Value

a vector of time points
