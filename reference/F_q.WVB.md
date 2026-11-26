# WVB human fraction (**MY**)

Compute the baseline human fraction from the availability of resident
hosts (\\W_H\\), visitors (\\W_V\\), and all available blood hosts
(\\B\\).

\$\$q = \frac{W_H+W_V}{B}\$\$

## Usage

``` r
# S3 method for class 'WVB'
F_q(t, xds_obj, s)
```

## Arguments

- t:

  time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

\\q\\, the baseline human fraction
