# Build a Model and Configure All Components

Make an **`xds`** *model object* from a set of input parameters.

The first set of arguments chooses a module to use for each dynamical
component (see
[xds_info_dynamical_components](https://dd-harp.github.io/ramp.xds/reference/xds_info_dynamical_components.md)):

- `Xname` — human / host infection dynamics (see
  [XH_module_list](https://dd-harp.github.io/ramp.xds/reference/XH_module_list.md))

- `MYname` – adult mosquito ecology and infection dynamics (see
  [MY_module_list](https://dd-harp.github.io/ramp.xds/reference/MY_module_list.md))

- `Lname` – aquatic mosquito ecology (see
  [L_module_list](https://dd-harp.github.io/ramp.xds/reference/L_module_list.md))

The second set of arguments sets up the model structure

- `nPatches` - the number of patches (see
  [xds_info_patch_dynamics](https://dd-harp.github.io/ramp.xds/reference/xds_info_patch_dynamics.md))

- `membership` - the habitat membership vector (see
  [xds_info_aquatic_habitats](https://dd-harp.github.io/ramp.xds/reference/xds_info_aquatic_habitats.md)).

- `residence` - the human residence vector (see
  [xds_info_human_populations](https://dd-harp.github.io/ramp.xds/reference/xds_info_human_populations.md))

Note that:

- The structural parameter `nHabitats` is set to `length(membership)`

- The structural parameter `nStrata` is set to `length(residence)`

Configure some of the basic elements:

- `XHoptions` sets parameter values and initial conditions for the
  **XH** module

- `MYoptions` sets parameter values and initial conditions for the
  **MY** module

- `Loptions` sets parameter values and initial conditions for the **L**
  module

- `HPop` is the human population density

- `searchQ` the habitat search weights (see
  [xds_info_search_weights_habitat](https://dd-harp.github.io/ramp.xds/reference/xds_info_search_weights_habitat.md))

- `searchB` the blood feeding search weights (see
  [xds_info_search_weights_blood](https://dd-harp.github.io/ramp.xds/reference/xds_info_search_weights_blood.md))

- `Koptions` sets up a mosquito dispersal matrix (if `nPatches>1`, see
  [xds_info_mosquito_dispersal](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_dispersal.md))

- `TSoptions` sets up a time spent matrix (if `nPatches>1`; see
  [xds_info_time_spent](https://dd-harp.github.io/ramp.xds/reference/xds_info_time_spent.md))

## Usage

``` r
xds_setup(
  model_name = "unnamed",
  xds = "ode",
  Xname = "SIS",
  XHoptions = list(),
  MYname = "SI",
  MYoptions = list(),
  Lname = "trivial",
  Loptions = list(),
  nPatches = 1,
  HPop = 1000,
  residence = 1,
  searchB = list(),
  TSoptions = list(),
  membership = 1,
  searchQ = list(),
  Koptions = list()
)
```

## Arguments

- model_name:

  a name for the model

- xds:

  is `xde` for differential equations or `dts` for discrete time systems

- Xname:

  the name of an **XH** module (see
  [XH_module_list](https://dd-harp.github.io/ramp.xds/reference/XH_module_list.md))

- XHoptions:

  a named list to set parameter values and initial conditions for the
  **XH** module

- MYname:

  the name of an **MY** module (see
  [MY_module_list](https://dd-harp.github.io/ramp.xds/reference/MY_module_list.md))

- MYoptions:

  a named list to set parameter values and initial conditions for the
  **MY** component

- Lname:

  the name of an **L** module (see
  [L_module_list](https://dd-harp.github.io/ramp.xds/reference/L_module_list.md))

- Loptions:

  a named list to set parameter values and initial conditions for the
  **L** component

- nPatches:

  the number of patches

- HPop:

  the human population density in each patch

- residence:

  the index of the patch where each human population stratum resides

- searchB:

  a vector of blood search weights or setup options (see
  [setup_blood_search_weights](https://dd-harp.github.io/ramp.xds/reference/setup_blood_search_weights.md))

- TSoptions:

  a time spent matrix or setup options (see
  [setup_timespent](https://dd-harp.github.io/ramp.xds/reference/setup_timespent.md))

- membership:

  the index of the patch where each aquatic habitat is found

- searchQ:

  a vector of habitat search weights or setup options (see
  [setup_habitat_search_weights](https://dd-harp.github.io/ramp.xds/reference/setup_habitat_search_weights.md))

- Koptions:

  a mosquito dispersal matrix (\\(K\\)) or setup options (see
  [setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md))

## Value

an **`xds`** object

## Details

1.  [make_xds_object_template](https://dd-harp.github.io/ramp.xds/reference/make_xds_object_template.md)
    returns an object template with a properly configured interface for
    blood feeding and egg laying (see
    [xds_object](https://dd-harp.github.io/ramp.xds/reference/xds_object.md)).
    The structural parameters are:

    - `nPatches` is passed as a parameter

    - `nHabitats = length(membership)`

    - `nStrata = length(residence) = length(HPop)`

    - `nHostSpecies=1` (basic setup handles only the first host species)

    - `nVectorSpecies=1` (basic setup handles only the first vector
      species)

    - `class(xds_obj$frame) = 'full'`

2.  Each one of the dynamical components is configured.

    - **`xds_obj$XH_obj[[1]]`** defines a model for human / host
      infection dynamics of class `Xname` (the **X** component). The
      parameter values passed in a named list, `XHoptions.`

    - **`xds_obj$MY_obj[[1]]`** defines a model for adult mosquito
      ecology & infection dynamics of class `MYname` (the **MY**
      Component). The parameter values are passed in a named list,
      `MYoptions.`

    - **`xds_obj$L_obj[[1]]`** defines a model for aquatic mosquito
      ecology of class `Lname` (The **L** Component). The parameter
      values are passed in a named list, `Loptions.`

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

## See also

[make_xds_object_template](https://dd-harp.github.io/ramp.xds/reference/make_xds_object_template.md),
[xds_info_basic_setup](https://dd-harp.github.io/ramp.xds/reference/xds_info_basic_setup.md)
