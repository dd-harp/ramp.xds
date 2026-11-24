# Make initial values for `GeM` (**MY**)

Create a named list of initial values for the `GeM` module. Values in
`options` override the defaults.

## Usage

``` r
make_MY_inits_GeM(
  nPatches,
  Upsilon,
  options = list(),
  M = 5,
  P = 1,
  Y = 1,
  Z = 1
)
```

## Arguments

- nPatches:

  the number of patches

- Upsilon:

  an `nPatches` by `nPatches` matrix describing survival and dispersal
  through the initial EIP

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
`nPatches`. Scalars are expanded to that length. Initialize `U` from
`Upsilon` and `d1`, `d2`, and `d3` to zero in each patch. These
accessory values are constructed internally rather than independently
set through `options`.

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes

- `Y`:

  density of infected adult mosquitoes

- `Z`:

  density of infectious adult mosquitoes

## Accessory Variable

The matrix \\\Upsilon(t)\\ represents survival and dispersal through the
time-varying EIP, from \\t\_\tau=t-\tau'(t)\\ to \\t\\. In the
autonomous case it is \\e^{-\Omega\tau}\\. For time-varying demography
and EIP, it is computed as an accessory variable:
\$\$\frac{d\Upsilon}{dt}=\left(\Omega(t\_\tau)\left(1-\frac{d\tau'(t)}{dt}\right)-\Omega(t)\right)\cdot\Upsilon(t).\$\$
The initial matrix is set to \\\exp(-\Omega eip)\\. This variable is
internal to the model and is not an output orbit.

## See also

[GeM](https://dd-harp.github.io/ramp.xds/reference/GeM.md),
[setup_MY_inits.GeM](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.GeM.md)

[change_MY_inits.GeM](https://dd-harp.github.io/ramp.xds/reference/change_MY_inits.GeM.md)
