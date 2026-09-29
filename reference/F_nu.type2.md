# Egg laying rate

Egg laying rates are a type2 functional response to available water.

Letting \\Q\\ be the availability of all water. The egg laying rate is
\$\$\nu = \nu_x \frac{s\_\nu Q}{1+s\_\nu Q}\$\$

## Usage

``` r
# S3 method for class 'type2'
F_nu(t, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

\\\nu\\, the baseline egg laying rate
