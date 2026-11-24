# Make initial values for `macdonald` (**MY**)

Create a named list of initial values for the `macdonald` module. Values
in `options` override the defaults.

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

a named [list](https://rdrr.io/r/base/list.html) of initial values

## Details

Initial mosquito densities must be numeric scalars or have length
`nPatches`. Scalars are expanded to that length. Initialize `dk_ix` to
zero in each patch. This accessory value is constructed internally
rather than independently set through `options`.

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

[change_MY_inits.macdonald](https://dd-harp.github.io/ramp.xds/reference/change_MY_inits.macdonald.md)
