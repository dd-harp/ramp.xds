# Make initial values for `macdonald` (**MY**)

Make initial values for `macdonald` (**MY**)

## Usage

``` r
make_MY_inits_macdonald(nPatches, options = list(), M = 5, P = 1, Y = 1, Z = 1)
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

- Y:

  infected mosquito density at each patch

- Z:

  infectious mosquito density at each patch

## Value

a [list](https://rdrr.io/r/base/list.html)

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes

- `Y`:

  density of infected adult mosquitoes

- `Z`:

  density of infectious adult mosquitoes

## See also

[macdonald](https://dd-harp.github.io/ramp.xds/reference/macdonald.md),
[setup_MY_inits.macdonald](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.macdonald.md)
