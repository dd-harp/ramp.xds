# Compute the baseline blood feeding rate (type 2) (**MY**)

Compute the baseline blood feeding rate using a type 2 functional
response to blood host availability, \\B\\. \$\$F_f(B)= f_x \frac{s_f
B}{1+s_f B}\$\$.

## Usage

``` r
# S3 method for class 'type2'
F_f(t, xds_obj, s)
```

## Arguments

- t:

  time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

\\f\\, the baseline blood feeding rate
