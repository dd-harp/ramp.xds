# Basic setup for host disease dynamics

Build a dynamical system to describe the disease dynamics in human or
host population in response to infective biting. The function encodes
the model as an **xds** object that can be solved by
[xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md).

To build a model forced by the EIR, see
[xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md).

## Usage

``` r
xds_setup_human(
  XHname = "SIS",
  XHoptions = list(),
  HPop = 1000,
  nPatches = 1,
  residence = 1,
  searchB = list(),
  TSoptions = list(),
  MYoptions = list(),
  model_name = "unnamed",
  xds = "ode"
)
```

## Arguments

- XHname:

  the name of an **XH** module (see
  [XH_module_list](https://dd-harp.github.io/ramp.xds/reference/XH_module_list.md))

- XHoptions:

  a named list to set parameter values and initial conditions for the
  **XH** module

- HPop:

  the initial human population density for each stratum

- nPatches:

  the number of patches

- residence:

  a vector that describes the patch where each human stratum lives

- searchB:

  a vector of search weights for blood feeding

- TSoptions:

  a time spent matrix or setup options (see
  [setup_timespent](https://dd-harp.github.io/ramp.xds/reference/setup_TimeSpent.md))

- MYoptions:

  a list to configure the trivial **MY** module

- model_name:

  a name for the model

- xds:

  is `ode` or `dde` or `dts` for ordinary OR delay differential OR
  difference equations

## Value

an **`xds`** object

## Details

A modified version of
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
that sets up a model to study exposure and infection. The **MY**
component module is set to `trivial` and a trace function computes
infective biting in the patches.

`class(xds_obj$frame) = 'human'`

to dispatch functions that compute derivatives (`xde`), update variables
(`dts`), and parse outputs.

To set up a model that is forced by the EIR, use
[xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md).

**Dynamics**

Choose a module:

- `XHname` — the name of an **XH** module: see the
  [XH_module_list](https://dd-harp.github.io/ramp.xds/reference/XH_module_list.md)
  for human / host infection dynamics

- `MYname` \\=\\ `trivial`

Set parameter values and initial conditions using setup options (named
lists):

- `XHoptions` sets parameter values and initial conditions for the
  **XH** module

- `MYoptions` sets up infective biting in the patches for the trivial
  **MY** module

- `HPop` is the human population density

**Model Structure**

Set up the model structure:

- `nPatches` - the number of patches (see
  [xds_info_patch_dynamics](https://dd-harp.github.io/ramp.xds/reference/xds_info_patch_dynamics.md))

- `residence` - the human residence vector (see
  [xds_info_human_populations](https://dd-harp.github.io/ramp.xds/reference/xds_info_human_populations.md))

During setup:

- The structural parameter `nStrata` is set to `length(residence)`

- `nHostSpecies`\\=1\\

If `nPatches`\\\>1\\, set up some basic aspects of blood feeding and
exposure:

- `searchB` the blood feeding search weights (see
  [xds_info_search_weights_blood](https://dd-harp.github.io/ramp.xds/reference/xds_info_search_weights_blood.md))

- `TSoptions` sets up a time spent matrix (see
  [xds_info_time_spent](https://dd-harp.github.io/ramp.xds/reference/xds_info_time_spent.md))

**Next**

- To solve the model, see
  [xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)
  or
  [xds_solve_cohort](https://dd-harp.github.io/ramp.xds/reference/xds_solve_cohort.md)

- To check or change parameters, see
  [xds_info_change_parameters](https://dd-harp.github.io/ramp.xds/reference/xds_info_change_parameters.md)

- To check or change the initial conditions, see
  [xds_info_initial_conditions](https://dd-harp.github.io/ramp.xds/reference/xds_info_initial_conditions.md)

- To modify the model structure, see
  [xds_info_advanced_setup](https://dd-harp.github.io/ramp.xds/reference/xds_info_advanced_setup.md)

## See also

- Basic Setup:
  [xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md),
  [xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md),
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
