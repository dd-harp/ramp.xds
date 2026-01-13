# Make a type2 function for age

Return an age pattern \\\omega(a)\\, a function of the form
\$\$\omega(a) = \frac{A(a+\tau)}{B+a+\tau}\$\$ where \\\tau\\ is a shift
so that \\\omega(0) \> 0\\ and \\A\\ and \\B\\ are shape parameters

## Usage

``` r
# S3 method for class 'type2'
make_function(opts)
```

## Arguments

- opts:

  a named list

## Value

a function for seasonality

## See also

[makepar_F_type2](https://dd-harp.github.io/ramp.xds/reference/makepar_F_type2.md)
