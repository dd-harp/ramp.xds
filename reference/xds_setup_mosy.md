# Build a Model of Mosquito Ecology

A modified version of
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
that streamlines setup for models without parasite / pathogen infection
dynamics. These models lack the **Y** component and an **X** component,
but they will often need an **H** component (host density).

The **`xds`** object defines `frame = class(frame) = 'mosy'` to dispatch
[xde_derivatives.mosy](https://dd-harp.github.io/ramp.xds/reference/xde_derivatives.mosy.md)
or
[dts_update.mosy](https://dd-harp.github.io/ramp.xds/reference/dts_update.mosy.md)
and associated functions.

The **X** Component module is `trivial`, but since humans / vertebrate
hosts can be a resource, `HPop` must be set.

## Usage

``` r
xds_setup_mosy(
  xds = "ode",
  MYname = "basicM",
  Lname = "basicL",
  nPatches = 1,
  membership = 1,
  HPop = 1000,
  K_matrix = list(),
  searchQ = 1,
  kappa = 0,
  MYoptions = list(),
  Loptions = list(),
  model_name = "unnamed"
)
```

## Arguments

- xds:

  is `ode` or `dde` or `dts` for ordinary OR delay differential OR
  difference equations

- MYname:

  is a character string defining a **MY** Component module

- Lname:

  is a character string defining a **L** Component module

- nPatches:

  is the number of patches

- membership:

  is a vector that describes the patch where each aquatic habitat is
  found

- HPop:

  is the human / host population density

- K_matrix:

  is either a K_matrix matrix or a string that defines how to set it up

- searchQ:

  is a vector of search weights for egg laying

- kappa:

  is a vector describing net infectiousness

- MYoptions:

  a list to configure the **MY** Component module

- Loptions:

  a list to configure the **L** Component module

- model_name:

  is a name for the model (arbitrary)

## Value

an **`xds`** object

## See also

[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
and
[dMYdt.basicM](https://dd-harp.github.io/ramp.xds/reference/dMYdt.basicM.md)
