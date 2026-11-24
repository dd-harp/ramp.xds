# Make initial values for `SI` (**MY**)

Create a named list of initial values for the `SI` module. Values in
`options` override the defaults.

## Usage

``` r
make_MY_inits_SI(nPatches, options = list(), M = 10, P = 8, Y = 1)
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

  parous mosquito density at each patch

- Y:

  infected mosquito density at each patch

## Value

a named [list](https://rdrr.io/r/base/list.html) of initial values

## Details

Initial values must be numeric scalars or have length `nPatches`.
Scalars are expanded to that length. Require `Y < P < M` in every patch.

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes

- `Y`:

  density of infected adult mosquitoes

## See also

[SI](https://dd-harp.github.io/ramp.xds/reference/SI.md),
[setup_MY_inits.SI](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.SI.md)

[change_MY_inits.SI](https://dd-harp.github.io/ramp.xds/reference/change_MY_inits.SI.md)
