# Change parameters for `RMdts` (**MY**)

Change parameter values named in `options` for the `RMdts` module,
leaving other values unchanged. Call
[MBionomics](https://dd-harp.github.io/ramp.xds/reference/MBionomics.md)
at time zero after updating parameters, without calling
[MEffectSizes](https://dd-harp.github.io/ramp.xds/reference/MEffectSizes.md).
This method changes static parameter values.

## Usage

``` r
# S3 method for class 'RMdts'
change_MY_pars(xds_obj, options = list(), s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- options:

  a named list

- s:

  the vector species index

## Value

an **`xds`** model object

## See also

[RMdts](https://dd-harp.github.io/ramp.xds/reference/RMdts.md),
[get_MY_pars.RMdts](https://dd-harp.github.io/ramp.xds/reference/get_MY_pars.RMdts.md)
