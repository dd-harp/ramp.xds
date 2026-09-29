# WVB human feeding fraction

Computes the human feeding fraction from the availability of resident
hosts (\\W\\), visitors (\\V\\), and all available blood hosts (\\B\\).

\$\$q = \frac{W+V}{B}\$\$

## Usage

``` r
# S3 method for class 'WVB'
F_q(t, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

\\q\\, the human feeding fraction
