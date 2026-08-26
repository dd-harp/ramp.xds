# Change F_shock

Change the shock function. The function `F` should have the form
\\F(t,V)\\. If the variable \\V\\ is not used, it's default value should
be set to an empty list.

## Usage

``` r
change_F_shock(F, xds_obj, ix = 1)
```

## Arguments

- F:

  new shock function

- xds_obj:

  an **`xds`** model object

- ix:

  the species index

## Value

an **`xds`** object

## See also

[F_one](https://dd-harp.github.io/ramp.xds/reference/F_one.md)
