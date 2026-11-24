# Basic setup

Build a dynamical system to describe the epidemiology and transmission
dynamics of a mosquito-borne pathogen. The function encodes the model as
an **xds** object that can be solved by
[xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md).

Other basic setup functions are discussed in
[xds_info_basic_setup](https://dd-harp.github.io/ramp.xds/reference/xds_basic_setup.md).

## Usage

``` r
xds_setup(
  XHname = "SIS",
  MYname = "SI",
  Lname = "trivialL",
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
  xds = "xde",
  model_name = "unnamed"
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
  (see
  [residence_vector](https://dd-harp.github.io/ramp.xds/reference/residence_vector.md))

- membership:

  the habitat membership vector or
  [habitat_vector](https://dd-harp.github.io/ramp.xds/reference/habitat_vector.md)

- HPop:

  the initial human population density for each stratum

- searchB:

  a vector of blood search weights or setup options (see
  [setup_blood_search_weights](https://dd-harp.github.io/ramp.xds/reference/setup_blood_search_weights.md))

- TSoptions:

  a time spent matrix or setup options (see
  [setup_timespent](https://dd-harp.github.io/ramp.xds/reference/setup_timespent.md))

- searchQ:

  a vector of habitat search weights or setup options (see
  [setup_habitat_search_weights](https://dd-harp.github.io/ramp.xds/reference/setup_habitat_search_weights.md))

- Koptions:

  a mosquito dispersal matrix (\\(K\\)) or setup options (see
  [setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md))

- xds:

  use `xde` for differential equations and `dts` for discrete time
  systems

- model_name:

  a name for the model

## Value

an **`xds`** model object

## Details

Basic setup has three parts: dynamics, model structure, and basic
spatial elements.

## Note

xds_setup calls
[make_xds_object_template](https://dd-harp.github.io/ramp.xds/reference/make_xds_object_template.md),
which returns an object template with a properly configured interface
for blood feeding and egg laying (see
[xds_object](https://dd-harp.github.io/ramp.xds/reference/xds_object.md)).

## Dynamics

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
[xds_info_change_parameters](https://dd-harp.github.io/ramp.xds/reference/xds_change_parameters.md)
or
[xds_info_initial_conditions](https://dd-harp.github.io/ramp.xds/reference/xds_initial_conditions.md).

## Model Structure

Set up the model structure:

- `nPatches` - the number of patches (see
  [xds_info_patch_dynamics](https://dd-harp.github.io/ramp.xds/reference/xds_info_patch_dynamics.md))

- `residence` - the human residence vector (see
  [residence_vector](https://dd-harp.github.io/ramp.xds/reference/residence_vector.md)
  and
  [residence_matrix](https://dd-harp.github.io/ramp.xds/reference/residence_matrix.md))

- `membership` - the habitat membership vector (see
  [xds_info_aquatic_habitats](https://dd-harp.github.io/ramp.xds/reference/xds_info_aquatic_habitats.md))

During setup:

- The structural parameter `nStrata` is set to `length(residence)`

- The structural parameter `nHabitats` is set to `length(membership)`

- `nPatches` is checked and set to `max(membership, nPatches)`

- `nVectorSpecies`\\(=1\\)

- `nHostSpecies`\\(=1\\)

## Basic Spatial Dynamics

If `nPatches`\\(\>1\\), set up some basic aspects of egg laying, blood
feeding, mosquito spatial dynamics, and exposure:

- `searchQ` the habitat search weights (see
  [xds_info_search_weights_habitat](https://dd-harp.github.io/ramp.xds/reference/xds_info_search_weights_habitat.md))

- `searchB` the blood feeding search weights (see
  [xds_info_search_weights_blood](https://dd-harp.github.io/ramp.xds/reference/xds_info_search_weights_blood.md)
  and
  [human_availability](https://dd-harp.github.io/ramp.xds/reference/human_availability.md))

- `Koptions` sets up a mosquito dispersal matrix (see
  [xds_info_mosquito_dispersal](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_dispersal.md))

- `TSoptions` sets up a time spent matrix (see
  [xds_info_time_spent](https://dd-harp.github.io/ramp.xds/reference/time_spent_matrix.md))

Blood host availability and time at risk determine the
[mixing_matrix](https://dd-harp.github.io/ramp.xds/reference/mixing_matrix.md),
which maps biting density in patches to biting rates in population
strata. See
[blood_feeding](https://dd-harp.github.io/ramp.xds/reference/blood_feeding.md)
for the blood-feeding framework.

## See also

- Basic Setup:
  [xds_info_basic_setup](https://dd-harp.github.io/ramp.xds/reference/xds_basic_setup.md)

  - change parameters —
    [xds_info_change_parameters](https://dd-harp.github.io/ramp.xds/reference/xds_change_parameters.md)

  - change initial conditions —
    [xds_info_initial_conditions](https://dd-harp.github.io/ramp.xds/reference/xds_initial_conditions.md)

- Advanced Setup:
  [xds_info_advanced_setup](https://dd-harp.github.io/ramp.xds/reference/xds_advanced_setup.md)

- Solving:
  [xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)

- Internals:
  [xds_info_dynamical_components](https://dd-harp.github.io/ramp.xds/reference/xds_info_dynamical_components.md),
  [xds_object](https://dd-harp.github.io/ramp.xds/reference/xds_object.md),
  [make_xds_object_template](https://dd-harp.github.io/ramp.xds/reference/make_xds_object_template.md)
