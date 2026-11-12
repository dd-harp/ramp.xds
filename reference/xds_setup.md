# Basic setup

Build a dynamical system to describe the epidemiology and transmission
dynamics of a mosquito-borne pathogen. The function encodes the model as
an **xds** object that can be solved by
[xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md).

## Usage

``` r
xds_setup(
  XHname = "SIS",
  MYname = "SI",
  Lname = "trivial",
  XHoptions = list(),
  MYoptions = list(),
  Loptions = list(),
  nPatches = 1,
  residence = 1,
  membership = 1,
  HPop = 1000,
  searchB = list(),
  TSoptions = list(),
  searchQ = list(),
  Koptions = list(),
  model_name = "unnamed",
  xds = "ode"
)
```

## Arguments

- XHname:

  the name of an **XH** module (see
  [XH_module_list](https://dd-harp.github.io/ramp.xds/reference/XH_module_list.md))

- MYname:

  the name of an **MY** module (see
  [MY_module_list](https://dd-harp.github.io/ramp.xds/reference/MY_module_list.md))

- Lname:

  the name of an **L** module (see
  [L_module_list](https://dd-harp.github.io/ramp.xds/reference/L_module_list.md))

- XHoptions:

  a named list to set parameter values and initial conditions for the
  **XH** module

- MYoptions:

  a named list to set parameter values and initial conditions for the
  **MY** component

- Loptions:

  a named list to set parameter values and initial conditions for the
  **L** component

- nPatches:

  the number of patches

- residence:

  the index of the patch where each human population stratum resides

- membership:

  the index of the patch where each aquatic habitat is found

- HPop:

  the initial human population density for each stratum

- searchB:

  a vector of blood search weights or setup options (see
  [setup_blood_search_weights](https://dd-harp.github.io/ramp.xds/reference/setup_blood_search_weights.md))

- TSoptions:

  a time spent matrix or setup options (see
  [setup_timespent](https://dd-harp.github.io/ramp.xds/reference/setup_TimeSpent.md))

- searchQ:

  a vector of habitat search weights or setup options (see
  [setup_habitat_search_weights](https://dd-harp.github.io/ramp.xds/reference/setup_habitat_search_weights.md))

- Koptions:

  a mosquito dispersal matrix (\\K\\) or setup options (see
  [setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md))

- model_name:

  a name for the model

- xds:

  is `xde` for differential equations or `dts` for discrete time systems

## Value

an **`xds`** object

## Details

Basic setup has three parts: dynamics, model structure, and basic
spatial elements.

**Dynamics**

To set up a dynamical system, a module is selected for each dynamical
component by name:

- `XHname` — the name of an **XH** module: see the
  [XH_module_list](https://dd-harp.github.io/ramp.xds/reference/XH_module_list.md)
  for human / host infection dynamics

- `MYname` – the name of an **MY** module: see the
  [MY_module_list](https://dd-harp.github.io/ramp.xds/reference/MY_module_list.md)
  for adult mosquito ecology and infection dynamics

- `Lname` – the name of an **L** module: see the
  [L_module_list](https://dd-harp.github.io/ramp.xds/reference/L_module_list.md)
  for aquatic mosquito ecology

To configure the modules, set parameter values and initial conditions
using setup options (named lists):

- `XHoptions` sets parameter values and initial conditions for the
  **XH** module

- `MYoptions` sets parameter values and initial conditions for the
  **MY** module

- `Loptions` sets parameter values and initial conditions for the **L**
  module

- `HPop` is the human population density

For more information about how to change parameters or initial
conditions, see
[xds_info_change_parameters](https://dd-harp.github.io/ramp.xds/reference/xds_info_change_parameters.md)
or
[xds_info_initial_conditions](https://dd-harp.github.io/ramp.xds/reference/xds_info_initial_conditions.md).

**Model Structure**

Set up the model structure:

- `nPatches` - the number of patches (see
  [xds_info_patch_dynamics](https://dd-harp.github.io/ramp.xds/reference/xds_info_patch_dynamics.md))

- `residence` - the human residence vector (see
  [xds_info_human_populations](https://dd-harp.github.io/ramp.xds/reference/xds_info_human_populations.md))

- `membership` - the habitat membership vector (see
  [xds_info_aquatic_habitats](https://dd-harp.github.io/ramp.xds/reference/xds_info_aquatic_habitats.md))

During setup:

- The structural parameter `nStrata` is set to `length(residence)`

- The structural parameter `nHabitats` is set to `length(membership)`

- `nPatches` is checked and set to `max(membership, nPatches)`

- `nVectorSpecies`\\=1\\

- `nHostSpecies`\\=1\\

**Spatial Dynamics**

If `nPatches`\\\>1\\, set up some basic aspects of egg laying, blood
feeding, mosquito spatial dynamics, and exposure:

- `searchQ` the habitat search weights (see
  [xds_info_search_weights_habitat](https://dd-harp.github.io/ramp.xds/reference/xds_info_search_weights_habitat.md))

- `searchB` the blood feeding search weights (see
  [xds_info_search_weights_blood](https://dd-harp.github.io/ramp.xds/reference/xds_info_search_weights_blood.md))

- `Koptions` sets up a mosquito dispersal matrix (see
  [xds_info_mosquito_dispersal](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_dispersal.md))

- `TSoptions` sets up a time spent matrix (see
  [xds_info_time_spent](https://dd-harp.github.io/ramp.xds/reference/xds_info_time_spent.md))

**Basic Setup Variants**

Other versions of xds_setup are available (see
[xds_info_basic_setup](https://dd-harp.github.io/ramp.xds/reference/xds_info_basic_setup.md)):

- Use
  [xds_setup_aquatic](https://dd-harp.github.io/ramp.xds/reference/xds_setup_aquatic.md)
  for studies of mosquito aquatic ecology

- Use
  [xds_setup_mosy](https://dd-harp.github.io/ramp.xds/reference/xds_setup_mosy.md)
  for studies of mosquito ecology without infection dynamics

- Use
  [xds_setup_human](https://dd-harp.github.io/ramp.xds/reference/xds_setup_human.md)
  for studies of human/host disease dynamics forced by infective biting
  in patches

- Use
  [xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md)
  for studies of human/host disease dynamics forced by the local daily
  EIR

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

xds_setup calls
[make_xds_object_template](https://dd-harp.github.io/ramp.xds/reference/make_xds_object_template.md),
which returns an object template with a properly configured interface
for blood feeding and egg laying (see
[xds_object](https://dd-harp.github.io/ramp.xds/reference/xds_object.md)).

## See also

- Basic Setup:
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
