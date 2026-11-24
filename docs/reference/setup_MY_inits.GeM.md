# Set up initial values for `GeM` (**MY**)

Set up initial values for the `GeM` module using
[make_MY_inits_GeM](https://dd-harp.github.io/ramp.xds/reference/make_MY_inits_GeM.md).
Values in `options` override the defaults. The EIP survival and
dispersal matrix is computed and passed to the constructor.

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

an **`xds`** model object

## Note

Initial values are stored at `xds_obj$MY_obj[[s]]$inits`.

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

[change_MY_inits.GeM](https://dd-harp.github.io/ramp.xds/reference/change_MY_inits.GeM.md)

[get_MY_inits](https://dd-harp.github.io/ramp.xds/reference/get_MY_inits.md)
