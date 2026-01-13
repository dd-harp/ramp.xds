# Times Utility for Differential Equations

This method sets up the time points for the independent variable to
return solutions for the dependent variables.

If `times` is not `NULL,` then it returns solutions at those values.

Otherwise, it returns a sequence from `0` to `Tmax` by `dt.`

If values in `times` are not a sequence, the function checks that all
its values are in the sequence of time points when solutions are
returned.

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

an **`xds`** object
