# Build a Model of Mosquito Ecology

A modified version of
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
that streamlines setup for models without parasite / pathogen infection
dynamics. These models lack the **Y** component and an **X** component,
but they will often need an **H** component (host density).

The **`xds`** object defines `frame = class(frame) = 'mosy'` to dispatch
functions that compute derivatives (`xde`), update variables (`dts`),
and parse outputs

The **X** Component module is `trivial`, but since humans / vertebrate
hosts can be a resource, `HPop` must be set.

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
  kappa = 1,
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

  is the human / host population density

- searchQ:

  is a vector of search weights for egg laying

- kappa:

  is a vector describing net infectiousness

- MYoptions:

  a list to configure the **MY** Component module

- Koptions:

  a mosquito dispersal matrix (\\(K\\)) or setup options (see
  [setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md))

- Loptions:

  a list to configure the **L** Component module

## Value

an **`xds`** object

## See also

[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
and
[dMYdt.basicM](https://dd-harp.github.io/ramp.xds/reference/dMYdt.basicM.md)
