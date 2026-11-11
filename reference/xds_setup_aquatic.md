# Basic setup for aquatic mosquito ecology models

A modified version of
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
that streamlines setup for the **L** Component and egg laying for a
`trivial` **MY** Component. This sets `class(xds_obj$frame) = 'aquatic'`
to dispatch functions that compute derivatives (`xde`), update variables
(`dts`), and parse outputs.

Choose a module:

- `Lname` – aquatic mosquito ecology (see
  [L_module_list](https://dd-harp.github.io/ramp.xds/reference/L_module_list.md))

Set up the model structure:

- `nHabitats` – sets the number of habitats

- `nPatches` is set to `nHabitats`

Set parameter values and initial conditions using setup options (named
lists):

- `MYoptions` sets up egg laying using the `trivial` **MY** module

- `Loptions` sets parameter values and initial conditions for the **L**
  module

## Usage

``` r
xds_setup_aquatic(
  model_name = "unnamed",
  xds = "ode",
  nHabitats = 1,
  Lname = "basicL",
  Loptions = list(),
  MYoptions = list()
)
```

## Arguments

- model_name:

  is a name for the model (arbitrary)

- xds:

  is `ode` or `dde` or `dts` for ordinary OR delay differential OR
  difference equations

- nHabitats:

  is the number of habitats

- Lname:

  the name of an **L** module (see
  [L_module_list](https://dd-harp.github.io/ramp.xds/reference/L_module_list.md))

- Loptions:

  a list to configure the **L** Component module

- MYoptions:

  options to configure egg laying

## Value

an **`xds`** object

## Note

The **XH** model is never used, but it is set up as the default
`trivial` module.

## See also

[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
