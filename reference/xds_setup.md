# Basic setup

Build a basic model that defines all three components from a set of
input parameters.

Choose a module to use for each dynamical component (see
[xds_info_dynamical_components](https://dd-harp.github.io/ramp.xds/reference/xds_info_dynamical_components.md)):

- `XHname` — human / host infection dynamics (see
  [XH_module_list](https://dd-harp.github.io/ramp.xds/reference/XH_module_list.md))

- `MYname` – adult mosquito ecology and infection dynamics (see
  [MY_module_list](https://dd-harp.github.io/ramp.xds/reference/MY_module_list.md))

- `Lname` – aquatic mosquito ecology (see
  [L_module_list](https://dd-harp.github.io/ramp.xds/reference/L_module_list.md))

Set up the model structure:

- `nPatches` - the number of patches (see
  [xds_info_patch_dynamics](https://dd-harp.github.io/ramp.xds/reference/xds_info_patch_dynamics.md))

- `membership` - the habitat membership vector (see
  [xds_info_aquatic_habitats](https://dd-harp.github.io/ramp.xds/reference/xds_info_aquatic_habitats.md))

- `residence` - the human residence vector (see
  [xds_info_human_populations](https://dd-harp.github.io/ramp.xds/reference/xds_info_human_populations.md))

During setup:

- The structural parameter `nHabitats` is set to `length(membership)`

- The structural parameter `nStrata` is set to `length(residence)`

- `nPatches` is checked and set to `max(membership, nPatches)`

Set parameter values and initial conditions using setup options (named
lists):

- `XHoptions` sets parameter values and initial conditions for the
  **XH** module

- `MYoptions` sets parameter values and initial conditions for the
  **MY** module

- `Loptions` sets parameter values and initial conditions for the **L**
  module

- `HPop` is the human population density

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
  HPop = 1000,
  residence = 1,
  searchB = list(),
  TSoptions = list(),
  membership = 1,
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

- HPop:

  the initial human population density for each stratum

- residence:

  the index of the patch where each human population stratum resides

- searchB:

  a vector of blood search weights or setup options (see
  [setup_blood_search_weights](https://dd-harp.github.io/ramp.xds/reference/setup_blood_search_weights.md))

- TSoptions:

  a time spent matrix or setup options (see
  [setup_timespent](https://dd-harp.github.io/ramp.xds/reference/setup_TimeSpent.md))

- membership:

  the index of the patch where each aquatic habitat is found

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

1.  xds_setup calls
    [make_xds_object_template](https://dd-harp.github.io/ramp.xds/reference/make_xds_object_template.md),
    which returns an object template with a properly configured
    interface for blood feeding and egg laying (see
    [xds_object](https://dd-harp.github.io/ramp.xds/reference/xds_object.md)).
    The structural parameters are:

    - `nPatches` is passed as a parameter

    - `nHabitats = length(membership)`

    - `nStrata = length(residence)`

    - `nHostSpecies=1` (basic setup handles only the first host species)

    - `nVectorSpecies=1` (basic setup handles only the first vector
      species)

    - `class(xds_obj$frame) = 'full'`

2.  Each one of the dynamical components is set up as a module object:

    - **`xds_obj$XH_obj[[1]]`** defines the module for human / host
      infection dynamics of class `XHname` (the **XH** component). The
      parameter values are passed in a named list, `XHoptions`.

    - **`xds_obj$MY_obj[[1]]`** defines the module for adult mosquito
      ecology & infection dynamics of class `MYname` (the **MY**
      Component). The parameter values are passed in a named list,
      `MYoptions`.

    - **`xds_obj$L_obj[[1]]`** defines the module for aquatic mosquito
      ecology of class `Lname` (the **L** Component). The parameter
      values are passed in a named list, `Loptions`.

## Note

Other options can be configured after basic setup (see
[xds_info_setup_options](https://dd-harp.github.io/ramp.xds/reference/xds_info_setup_options.md)).
If the **MY** Component is the `trivial` module, consider using
[xds_setup_aquatic](https://dd-harp.github.io/ramp.xds/reference/xds_setup_aquatic.md)
or
[xds_setup_human](https://dd-harp.github.io/ramp.xds/reference/xds_setup_human.md)
or
[xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md).
Models for mosquito ecology need not include states describing infection
status (see
[xds_setup_mosy](https://dd-harp.github.io/ramp.xds/reference/xds_setup_mosy.md)).

Other versions of xds_setup are available:

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
  for studies of human/host disease dynamics forced by the eir

## See also

[make_xds_object_template](https://dd-harp.github.io/ramp.xds/reference/make_xds_object_template.md),
[xds_info_basic_setup](https://dd-harp.github.io/ramp.xds/reference/xds_info_basic_setup.md)
