# Build a Model of Human / Host Epidemiology

A modified version of
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
that streamlines setup for models with a trival **MY** Component.

The **`xds`** object defines `frame = class(frame) = 'human'` to
dispatch
[xde_derivatives.human](https://dd-harp.github.io/ramp.xds/reference/xde_derivatives.human.md)
or
[dts_update.human](https://dd-harp.github.io/ramp.xds/reference/dts_update.human.md)
and associated functions.

The **MY** Component module is set to `trivial.` The funcion
[F_fqZ.trivial](https://dd-harp.github.io/ramp.xds/reference/F_fqZ.trivial.md)
is called to compute passes the density of infectious adult mosquitoes,
and \\f\\ and \\q\\ can still be configured. In this case, the daily EIR
is computed using the blood feeding interface, including
[Exposure](https://dd-harp.github.io/ramp.xds/reference/Exposure.md), in
the same way as a model with a non-trivial **MY** Component module.

To study human cohort dynamics by passing a function that computes the
daily EIR, consider using
[xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md).

## Usage

``` r
xds_setup_human(
  Xname = "SIS",
  XHoptions = list(),
  xds = "ode",
  nPatches = 1,
  residence = 1,
  HPop = 1000,
  searchB = 1,
  TimeSpent = list(),
  MYoptions = list(),
  BFopts = list(),
  model_name = "unnamed"
)
```

## Arguments

- Xname:

  a character string defining a **X** Component module

- XHoptions:

  a named list to configure the **X** Component module

- xds:

  is `ode` or `dde` or `dts` for ordinary OR delay differential OR
  difference equations

- nPatches:

  the number of patches

- residence:

  a vector that describes the patch where each human stratum lives

- HPop:

  the number of humans in each patch

- searchB:

  a vector of search weights for blood feeding

- TimeSpent:

  either a TimeSpent matrix or a string to call a function that sets it
  up

- MYoptions:

  list to configure the **MY** Component module

- BFopts:

  list to configure the blood feeding model

- model_name:

  a name for the model

## Value

an **`xds`** model object

## See also

[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
and
[xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md)
