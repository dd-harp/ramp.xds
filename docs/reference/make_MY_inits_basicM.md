# Make initial values for `basicM` (**MY**)

Create a named list of initial values for the `basicM` module. Values in
`options` override the defaults.

## Usage

``` r
make_MY_inits_basicM(nPatches, options, M = 5, P = 1)
```

## Arguments

- nPatches:

  the number of patches

- options:

  a [list](https://rdrr.io/r/base/list.html) of values that overrides
  the defaults

- M:

  total mosquito density at each patch

- P:

  total parous mosquito density at each patch

## Value

a named [list](https://rdrr.io/r/base/list.html) of initial values

## Details

Initial values must be numeric scalars or have length `nPatches`.
Scalars are expanded to that length.

## See also

[setup_MY_inits.basicM](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.basicM.md),
[change_MY_inits.basicM](https://dd-harp.github.io/ramp.xds/reference/change_MY_inits.basicM.md)
