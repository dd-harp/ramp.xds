# Basic setup for models of human/host disease dynamics

A modified version of
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
that sets up a model to study exposure and infection. The **MY**
component module is set to `trivial` and a trace function computes
infective biting in the patches. This sets
`class(xds_obj$frame) = 'human'` to dispatch functions that compute
derivatives (`xde`), update variables (`dts`), and parse outputs.

To set up a model that is forced by the EIR, use
[xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md).

Choose a module:

- `XHname` — human / host infection dynamics (see
  [XH_module_list](https://dd-harp.github.io/ramp.xds/reference/XH_module_list.md))

Set up the model structure:

- `nPatches` - the number of patches (see
  [xds_info_patch_dynamics](https://dd-harp.github.io/ramp.xds/reference/xds_info_patch_dynamics.md))

- `residence` - the human residence vector (see
  [xds_info_human_populations](https://dd-harp.github.io/ramp.xds/reference/xds_info_human_populations.md))

During setup:

- The structural parameter `nStrata` is set to `length(residence)`

Set parameter values and initial conditions using setup options (named
lists):

- `XHoptions` sets parameter values and initial conditions for the
  **XH** module

- `MYoptions` sets up infective biting in the patches for the trivial
  **MY** module

- `HPop` is the human population density

If `nPatches`\\\>1\\, set up some basic aspects of blood feeding and
exposure:

- `searchB` the blood feeding search weights (see
  [xds_info_search_weights_blood](https://dd-harp.github.io/ramp.xds/reference/xds_info_search_weights_blood.md))

- `TSoptions` sets up a time spent matrix (see
  [xds_info_time_spent](https://dd-harp.github.io/ramp.xds/reference/xds_info_time_spent.md))

To study human cohort dynamics by passing a function that computes the
daily EIR, consider using
[xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md).

## Usage

``` r
xds_setup_human(
  model_name = "unnamed",
  XHname = "SIS",
  XHoptions = list(),
  xds = "ode",
  nPatches = 1,
  residence = 1,
  HPop = 1000,
  searchB = list(),
  TSoptions = list(),
  MYoptions = list()
)
```

## Arguments

- model_name:

  a name for the model

- XHname:

  the name of an **XH** module (see
  [XH_module_list](https://dd-harp.github.io/ramp.xds/reference/XH_module_list.md))

- XHoptions:

  a named list to set parameter values and initial conditions for the
  **XH** module

- xds:

  is `ode` or `dde` or `dts` for ordinary OR delay differential OR
  difference equations

- nPatches:

  the number of patches

- residence:

  a vector that describes the patch where each human stratum lives

- HPop:

  the initial human population density for each stratum

- searchB:

  a vector of search weights for blood feeding

- TSoptions:

  a time spent matrix or setup options (see
  [setup_timespent](https://dd-harp.github.io/ramp.xds/reference/setup_TimeSpent.md))

- MYoptions:

  a list to configure the **MY** Component module

## Value

an **`xds`** object

## See also

[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
and
[xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md)
