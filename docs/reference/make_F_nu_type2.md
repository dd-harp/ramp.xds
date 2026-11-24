# Make type 2 oviposition rate port (**MY**)

Build a configuration object for a type 2 oviposition rate response to
water availability.

## Usage

``` r
make_F_nu_type2(nPatches, options = list(), vx = 0.3, sv = 0.1)
```

## Arguments

- nPatches:

  is the number of patches

- options:

  is a set of options that overrides the defaults

- vx:

  the maximum oviposition rate: the maximum number of egg batches laid
  in each patch, per mosquito, per day

- sv:

  the shape parameter

## Value

a configuration object for
[F_nu.type2](https://dd-harp.github.io/ramp.xds/reference/F_nu.type2.md)
