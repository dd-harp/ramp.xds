# Change parameters for `trivialMY` (**MY**)

For the `trivialMY` module (**MY**) for the \\(s^{th}\\) species, change
parameter values named in `options` leaving other values unchanged.
Rebuild only forcing functions whose parameters are supplied. An empty
parameter list resets the corresponding function to constant one.

## Usage

``` r
# S3 method for class 'trivialMY'
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

## Details

After the updates, call
[MBionomics](https://dd-harp.github.io/ramp.xds/reference/MBionomics.md)
at time zero with the flattened initial state. Do not call
[MEffectSizes](https://dd-harp.github.io/ramp.xds/reference/MEffectSizes.md);
bionomics is a no-op for this module.
