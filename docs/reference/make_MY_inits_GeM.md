# Make initial values for `GeM` (**MY**)

Make initial values for `GeM` (**MY**)

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

  a matrix describing survival and dispersal through the initial EIP

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

## Accessory Variable

The matrix \\(\Upsilon(t)\\) represents survival and dispersal through
the time-varying EIP, from \\(t\_\tau=t-\tau'(t)\\) to \\(t\\). In the
autonomous case it is \\(e^{-\Omega\tau}\\). For time-varying demography
and EIP, it is computed as an accessory variable:
\$\$\frac{d\Upsilon}{dt}=\left(\Omega(t\_\tau)\left(1-\frac{d\tau'(t)}{dt}\right)-\Omega(t)\right)\cdot\Upsilon(t).\$\$
The initial matrix is set to \\(\exp(-\Omega eip)\\). This variable is
internal to the model and is not an output orbit.

## See also

[GeM](https://dd-harp.github.io/ramp.xds/reference/GeM.md),
[setup_MY_inits.GeM](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.GeM.md)
