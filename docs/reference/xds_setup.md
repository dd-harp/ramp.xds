# Build a Model and Configure All Components

Make an **`xds`** *model object*:

- Define a module for each dynamical component (see
  [xds_info_dynamical_components](https://dd-harp.github.io/ramp.xds/reference/xds_info_dynamical_components.md)):

  - **XH** Component – human / host infection dynamics

  - **MY** Component – adult mosquito ecology and infection dynamics

  - **L** Component – aquatic mosquito ecology

- Define basic structural parameters for a single host and vector
  population:

  - `nPatches` - the number of patches (see
    [xds_info_patch_dynamics](https://dd-harp.github.io/ramp.xds/reference/xds_info_patch_dynamics.md))

  - `membership` - the habitat membership vector (see
    [xds_info_aquatic_habitats](https://dd-harp.github.io/ramp.xds/reference/xds_info_aquatic_habitats.md))

  - `nHabitats = length(membership)`

  - `residence` - the human residence vector (see
    [xds_info_human_populations](https://dd-harp.github.io/ramp.xds/reference/xds_info_human_populations.md))

  - `HPop` - the human population size (see
    [xds_info_human_populations](https://dd-harp.github.io/ramp.xds/reference/xds_info_human_populations.md))

  - `nStrata = length(residence) = length(HPop)`

- Configure some of the basic elements:

  - Search Weights (see
    [xds_info_search_weights](https://dd-harp.github.io/ramp.xds/reference/xds_info_search_weights.md))

  - Mosquito Dispersal matrix (if `nPatches>1`; see
    [xds_info_mosquito_dispersal](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_dispersal.md))

  - Time Spent matrix (if `nPatches>1`; see
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
  searchB = 1,
  TSoptions = list(name = "no_setup"),
  membership = 1,
  searchQ = 1,
  Koptions = list(Kname = "no_setup"),
  BFopts = list()
)
```

## Arguments

- model_name:

  is a name for the model (arbitrary)

- xds:

  is `ode` or `dde` or `dts` for ordinary OR delay differential OR
  difference equations

- Xname:

  a character string defining a **X** Component module

- XHoptions:

  a list to configure the **X** Component module

- MYname:

  a character string defining an **MY** module

- MYoptions:

  options to set up the **MY** component

- Lname:

  a character string defining a **L** Component module

- Loptions:

  a list to configure the **L** Component module

- nPatches:

  is the number of patches

- HPop:

  is the number of humans in each patch

- residence:

  is a vector that describes the patch where each human stratum lives

- searchB:

  is a vector of search weights for blood feeding

- TSoptions:

  is either a TimeSpent matrix or a string to call a function that sets
  it up

- membership:

  is a vector that describes the patch where each aquatic habitat is
  found

- searchQ:

  is a vector of search weights for egg laying

- Koptions:

  a K matrix, or options for
  [setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)
  (see
  [xds_info_mosquito_dispersal](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_dispersal.md))

- BFopts:

  a list to configure the blood feeding model

## Value

an **`xds`** object

## Details

1.  [make_xds_object_template](https://dd-harp.github.io/ramp.xds/reference/make_xds_object_template.md)
    returns an object template with a properly configured interface for
    blood feeding and egg laying (see
    [xds_object](https://dd-harp.github.io/ramp.xds/reference/xds_object.md)):

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

3.  After configuring the dynamical components, several structural
    parameters can be configured at the command line:

    - Habitat search weights can be set

    - Host population search weights can be set

    - A mosquito dispersal matrix, \\\cal Koptions\\, can be set

    - A time spent matrix, \\\Theta\\, can be set

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
