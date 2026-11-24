# Make initial values for `RMdts` (**MY**)

Create a named list of initial values for the `RMdts` module. Values in
`options` override the defaults.

## Usage

``` r
make_MY_inits_RMdts(
  nPatches,
  max_eip,
  options = list(),
  M = 5,
  P = 1,
  U = 0,
  Y = 1,
  Z = 1
)
```

## Arguments

- nPatches:

  the number of patches

- max_eip:

  the maximum number of EIP cohorts, an
  [integer](https://rdrr.io/r/base/integer.html)

- options:

  a [list](https://rdrr.io/r/base/list.html) of values that overrides
  the defaults

- M:

  total mosquito density at each patch

- P:

  total parous mosquito density at each patch

- U:

  total uninfected mosquito density at each patch

- Y:

  initial infected mosquito densities across EIP cohorts, a numeric
  scalar or input of length `nPatches * max_eip`

- Z:

  infectious mosquito density at each patch

## Value

a named [list](https://rdrr.io/r/base/list.html) of initial values

## Details

Initial values must be numeric scalars or have the required length:
`nPatches` for `M`, `P`, `U`, and `Z`, and `nPatches * max_eip` for `Y`.
Scalars are expanded to the required length.

## See also

[setup_MY_inits.RMdts](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.RMdts.md),
[change_MY_inits.RMdts](https://dd-harp.github.io/ramp.xds/reference/change_MY_inits.RMdts.md)
