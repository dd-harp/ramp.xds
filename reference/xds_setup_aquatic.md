# Basic setup for aquatic mosquito ecology

Build a dynamical system to describe the dynamics of immature mosquitoes
in aquatic habitats. The function encodes the model as an **xds** object
that can be solved by
[xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md).

## Usage

``` r
xds_setup_aquatic(
  Lname = "basicL",
  Loptions = list(),
  nHabitats = 1,
  MYoptions = list(),
  model_name = "unnamed",
  xds = "ode"
)
```

## Arguments

- Lname:

  the name of an **L** module (see
  [L_module_list](https://dd-harp.github.io/ramp.xds/reference/L_module_list.md))

- Loptions:

  a list to configure the **L** Component module

- nHabitats:

  is the number of habitats

- MYoptions:

  options to configure egg laying

- model_name:

  is a name for the model (arbitrary)

- xds:

  is `ode` or `dde` or `dts` for ordinary OR delay differential OR
  difference equations

## Value

an **`xds`** object

## Details

This is a modified version of
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
that streamlines setup for the **L** module and egg laying for a
`trivial` **MY** module. It sets

`class(xds_obj$frame) = 'aquatic'`

to dispatch functions that compute derivatives (`xde`), update variables
(`dts`), and parse outputs.

**Dynamics**

Choose a module:

- `Lname` – the name of an **L** module: see the
  [L_module_list](https://dd-harp.github.io/ramp.xds/reference/L_module_list.md)
  for aquatic mosquito ecology

Set parameter values and initial conditions using setup options (named
lists):

- `MYoptions` sets up egg laying using the `trivial` **MY** module

- `Loptions` sets parameter values and initial conditions for the **L**
  module

**Model Structure**

Set up the model structure:

- `nHabitats` – sets the number of habitats

- `nPatches` \\=\\ `nHabitats`

- `nVectorSpecies`\\=1\\

**Next**

- To solve the model, see
  [xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)

- To check or change parameters, see
  [xds_info_change_parameters](https://dd-harp.github.io/ramp.xds/reference/xds_info_change_parameters.md)

- To check or change the initial conditions, see
  [xds_info_initial_conditions](https://dd-harp.github.io/ramp.xds/reference/xds_info_initial_conditions.md)

- To modify the model structure, see
  [xds_info_advanced_setup](https://dd-harp.github.io/ramp.xds/reference/xds_info_advanced_setup.md)

## Note

[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
calls
[make_xds_object_template](https://dd-harp.github.io/ramp.xds/reference/make_xds_object_template.md),
which returns an object template with a properly configured interface
for blood feeding and egg laying (see
[xds_object](https://dd-harp.github.io/ramp.xds/reference/xds_object.md)).

- The **XH** model is never used, but it is set up as the default
  `trivial` module.

## See also

- Basic Setup:
  [xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md),
  [xds_info_basic_setup](https://dd-harp.github.io/ramp.xds/reference/xds_info_basic_setup.md)

  - [xds_info_change_parameters](https://dd-harp.github.io/ramp.xds/reference/xds_info_change_parameters.md)

  - [xds_info_initial_conditions](https://dd-harp.github.io/ramp.xds/reference/xds_info_initial_conditions.md)

- Advanced Setup:
  [xds_info_advanced_setup](https://dd-harp.github.io/ramp.xds/reference/xds_info_advanced_setup.md)

- Solving:
  [xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)

- Internals:
  [xds_info_dynamical_components](https://dd-harp.github.io/ramp.xds/reference/xds_info_dynamical_components.md),
  [xds_object](https://dd-harp.github.io/ramp.xds/reference/xds_object.md),
  [make_xds_object_template](https://dd-harp.github.io/ramp.xds/reference/make_xds_object_template.md)
