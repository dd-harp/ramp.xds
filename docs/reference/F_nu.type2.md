# Compute the baseline oviposition rate (type 2) (**MY**)

Compute the baseline oviposition rate using a type 2 functional response
to water availability, \\(Q\\). \$\$\nu = \nu_x \frac{s\_\nu Q}{1+s\_\nu
Q}\$\$

## Usage

``` r
# S3 method for class 'type2'
F_nu(t, xds_obj, s)
```

## Arguments

- t:

  time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

\\(\nu\\), the baseline oviposition rate
