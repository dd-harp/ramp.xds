# Setup initial values for `GeM` (**MY**)

Set initial values for all the variables in the `GeM` module (**MY**)
for the \\s^{th}\\ species.

- [make_MY_inits_GeM](https://dd-harp.github.io/ramp.xds/reference/make_MY_inits_GeM.md)
  supplies default values

- The values in `options` override the defaults

- To inspect variable names, use
  [get_MY_inits](https://dd-harp.github.io/ramp.xds/reference/get_MY_inits.md)
  or see the inline module documentation.

## Usage

``` r
# S3 method for class 'GeM'
setup_MY_inits(xds_obj, options = list(), s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- options:

  a [list](https://rdrr.io/r/base/list.html)

- s:

  the vector species index

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

The matrix \\\Upsilon(t)\\ represents survival and dispersal through the
time-varying EIP, from \\t\_\tau=t-\tau'(t)\\ to \\t\\. In the
autonomous case it is \\e^{-\Omega\tau}\\. For time-varying demography
and EIP, it is computed as an accessory variable:
\$\$\frac{d\Upsilon}{dt}=\left(\Omega(t\_\tau)\left(1-\frac{d\tau'(t)}{dt}\right)-\Omega(t)\right)\cdot\Upsilon(t).\$\$
The initial matrix is set to \\\exp(-\Omega eip)\\. This variable is
internal to the model and is not an output orbit.
