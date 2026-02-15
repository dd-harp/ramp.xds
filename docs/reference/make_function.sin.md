# Make a Sine-based Seasonality Function

Return a seasonal pattern \\S(t)\\, a function of the form \$\$S(t) = c
\left(1+\epsilon + \sin\left(\frac{2 \pi
(t-\tau)}{365}\right)\right)^p\$\$ where \\c\\ is a normalizing
constant, and

- \\\epsilon \geq 0\\ or `bottom`

- \\\tau\\ or `phase`

- \\p\\ or `pw`

The algorithm sets the constant \\c\\ or `norm` such that
\$\$\int_0^{365} S(t) dt=c\$\$ where the default is `norm=365.`

## Usage

``` r
# S3 method for class 'sin'
make_function(opts)
```

## Arguments

- opts:

  a named list

## Value

a function for seasonality

## See also

[makepar_F_sin](https://dd-harp.github.io/ramp.xds/reference/makepar_F_sin.md)
