# Get the state variables for `GeM` (**MY**)

Extract the state variables for the `GeM` module from `y` and return
them by name in a list for use during model solving. Return `U` as an
`nPatches` by `nPatches` matrix.

## Usage

``` r
# S3 method for class 'GeM'
get_MY_vars(y, xds_obj, s)
```

## Arguments

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a named [list](https://rdrr.io/r/base/list.html) of state variables and
module-specific derived quantities

## Note

Variable indices are stored at `xds_obj$MY_obj[[s]]$ix`.

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
