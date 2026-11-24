# Make initial values for `SI` (**MY**)

Make initial values for `SI` (**MY**)

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

a [list](https://rdrr.io/r/base/list.html)

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
