# Basic setup for mosquito ecology models

Build a basic model to study mosquito ecology. This is a modified
version of
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
that streamlines setup for models without parasite / pathogen infection
dynamics.

This sets `class(xds_obj$frame) = 'mosy'` to dispatch functions that
compute derivatives (`xde`), update variables (`dts`), and parse
outputs. Models ignore infection dynamics (the **X** and **Y**
processes). The **XH** component is set up to handle human/host
population density.

Choose a module to use for the mosquito dynamical components (see
[xds_info_dynamical_components](https://dd-harp.github.io/ramp.xds/reference/xds_info_dynamical_components.md)):

- `MYname` – adult mosquito ecology (see
  [MY_module_list](https://dd-harp.github.io/ramp.xds/reference/MY_module_list.md))

- `Lname` – aquatic mosquito ecology (see
  [L_module_list](https://dd-harp.github.io/ramp.xds/reference/L_module_list.md))

Set up the model structure:

- `nPatches` - the number of patches (see
  [xds_info_patch_dynamics](https://dd-harp.github.io/ramp.xds/reference/xds_info_patch_dynamics.md))

- `membership` - the habitat membership vector (see
  [xds_info_aquatic_habitats](https://dd-harp.github.io/ramp.xds/reference/xds_info_aquatic_habitats.md))

During setup:

- The structural parameter `nHabitats` is set to `length(membership)`

- The structural parameter `nStrata` is set to `nPatches`

- `nPatches` is checked and set to `max(membership, nPatches)`

Set parameter values and initial conditions using setup options (named
lists):

- `MYoptions` sets parameter values and initial conditions for the
  **MY** module

- `Loptions` sets parameter values and initial conditions for the **L**
  module

- `HPop` sets the human population density

If `nPatches`\\\>1\\, set up some basic aspects of egg laying, blood
feeding, mosquito spatial dynamics, and exposure:

- `searchQ` the habitat search weights (see
  [xds_info_search_weights_habitat](https://dd-harp.github.io/ramp.xds/reference/xds_info_search_weights_habitat.md))

- `Koptions` sets up a mosquito dispersal matrix (see
  [xds_info_mosquito_dispersal](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_dispersal.md))

## Usage

``` r
xds_setup_mosy(
  model_name = "unnamed",
  xds = "ode",
  MYname = "basicM",
  Lname = "basicL",
  nPatches = 1,
  membership = 1,
  HPop = 1000,
  searchQ = list(),
  MYoptions = list(),
  Koptions = list(),
  Loptions = list()
)
```

## Arguments

- model_name:

  is a name for the model

- xds:

  is `xde` for differential equations or `dts` for discrete time systems

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

## Value

an **`xds`** object

## See also

[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
and
[dMYdt.basicM](https://dd-harp.github.io/ramp.xds/reference/dMYdt.basicM.md)
