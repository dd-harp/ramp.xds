# Compute the HTC Matrix

Compute the \\N_p \times N_p\\ matrix \\\[D\]\\.

## Usage

``` r
compute_HTC(xds_obj, i = 1, s = 1, tol = 1e-05)
```

## Arguments

- xds_obj:

  a **`ramp.xds`** model object

- i:

  the host species index

- s:

  the vector species index

- tol:

  a tolerance parameter

## Value

a numeric [matrix](https://rdrr.io/r/base/matrix.html)
