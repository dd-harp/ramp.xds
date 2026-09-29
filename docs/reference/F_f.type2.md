# Compute the blood feeding rate, f

Implements a type2 functional response to compute blood feeding rates as
a functional response to resource availability: \$\$F_f(B)= f_x
\frac{s_f B}{1+s_f B}\$\$.

## Usage

``` r
# S3 method for class 'type2'
F_f(t, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

\\f\\, the baseline blood feeding rate
