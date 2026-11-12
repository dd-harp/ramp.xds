# Basic setup for mosquito ecology

Build a dynamical system to describe mosquito ecology. The function
encodes the model as an **xds** object that can be solved by
[xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md).

## Usage

``` r
xds_setup_mosy(
  MYname = "basicM",
  Lname = "basicL",
  nPatches = 1,
  membership = 1,
  HPop = 1000,
  searchQ = list(),
  MYoptions = list(),
  Koptions = list(),
  Loptions = list(),
  model_name = "unnamed",
  xds = "ode"
)
```

## Arguments

- MYname:

  the name of an **MY** module (see
  [MY_module_list](https://dd-harp.github.io/ramp.xds/reference/MY_module_list.md))

- Lname:

  the name of an **L** module (see
  [L_module_list](https://dd-harp.github.io/ramp.xds/reference/L_module_list.md))

- nPatches:

  the number of patches

- membership:

  is a vector that describes the patch where each aquatic habitat is
  found

- HPop:

  the initial human population density for each stratum

- searchQ:

  is a vector of search weights for egg laying

- MYoptions:

  a list to configure the **MY** Component module

- Koptions:

  a mosquito dispersal matrix (\\K\\) or setup options (see
  [setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md))

- Loptions:

  a list to configure the **L** Component module

- model_name:

  is a name for the model

- xds:

  is `xde` for differential equations or `dts` for discrete time systems

## Value

an **`xds`** object

## Details

This is a modified version of
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
that streamlines setup for models without parasite / pathogen infection
dynamics. It sets

`class(xds_obj$frame) = 'mosy'`

to dispatch functions that compute derivatives (`xde`), update variables
(`dts`), and parse outputs. The solver ignores infection dynamics (the
**X** and **Y** processes). A trivial **XH** component is set up to
handle human/host population density.

**Dynamics**

Choose a module to use for the mosquito dynamical components (see
[xds_info_dynamical_components](https://dd-harp.github.io/ramp.xds/reference/xds_info_dynamical_components.md)):

- `MYname` – the name of an **MY** module: see the
  [MY_module_list](https://dd-harp.github.io/ramp.xds/reference/MY_module_list.md)
  for adult mosquito ecology and infection dynamics

- `Lname` – the name of an **L** module: see the
  [L_module_list](https://dd-harp.github.io/ramp.xds/reference/L_module_list.md)
  for aquatic mosquito ecology

Set parameter values and initial conditions using setup options (named
lists):

- `MYoptions` sets parameter values and initial conditions for the
  **MY** module

- `Loptions` sets parameter values and initial conditions for the **L**
  module

- `HPop` sets the human population density

**Model Structure**

Set up the model structure:

- `nPatches` - the number of patches (see
  [xds_info_patch_dynamics](https://dd-harp.github.io/ramp.xds/reference/xds_info_patch_dynamics.md))

- `membership` - the habitat membership vector (see
  [xds_info_aquatic_habitats](https://dd-harp.github.io/ramp.xds/reference/xds_info_aquatic_habitats.md))

During setup:

- The structural parameter `nHabitats` is set to `length(membership)`

- The structural parameter `nStrata` is set to `nPatches`

- `nPatches` is checked and set to `max(membership, nPatches)`

- `nVectorSpecies`\\=1\\

**Spatial Dynamics**

If `nPatches`\\\>1\\, set up some basic aspects of egg laying, blood
feeding, mosquito spatial dynamics, and exposure:

- `searchQ` the habitat search weights (see
  [xds_info_search_weights_habitat](https://dd-harp.github.io/ramp.xds/reference/xds_info_search_weights_habitat.md))

- `Koptions` sets up a mosquito dispersal matrix (see
  [xds_info_mosquito_dispersal](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_dispersal.md))

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
