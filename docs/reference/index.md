# Package index

## Help

How to get help for **`ramp.xds`**

- [`xds_help_guide`](https://dd-harp.github.io/ramp.xds/reference/xds_help_guide.md)
  :

  **`ramp.xds`**: Getting Help

- [`xds_help_info`](https://dd-harp.github.io/ramp.xds/reference/xds_help_info.md)
  :

  **`ramp.xds`**: Info Pages

## Model Building

### Basic Setup

Build an xds model object

- [`xds_setup()`](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
  : Build a Model and Configure All Components

- [`xds_setup_mosy()`](https://dd-harp.github.io/ramp.xds/reference/xds_setup_mosy.md)
  : Build a Model of Mosquito Ecology

- [`xds_setup_aquatic()`](https://dd-harp.github.io/ramp.xds/reference/xds_setup_aquatic.md)
  : Build a Model of Immature Mosquito Ecology

- [`xds_setup_human()`](https://dd-harp.github.io/ramp.xds/reference/xds_setup_human.md)
  : Build a Model of Human / Host Epidemiology

- [`xds_setup_eir()`](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md)
  : Build a Model for a single Human / Host Epidemiology forced by the
  EIR

- [`xds_info_basic_setup`](https://dd-harp.github.io/ramp.xds/reference/xds_info_basic_setup.md)
  :

  **`ramp.xds`**: Basic Setup

- [`xds_info_setup_options`](https://dd-harp.github.io/ramp.xds/reference/xds_info_setup_options.md)
  :

  **`ramp.xds`**: All Setup Options

### Dynamical Modules

Setting the value of structural parameters

- [`XH_module_list`](https://dd-harp.github.io/ramp.xds/reference/XH_module_list.md)
  : XH modules
- [`MY_module_list`](https://dd-harp.github.io/ramp.xds/reference/MY_module_list.md)
  : MY Modules
- [`L_module_list`](https://dd-harp.github.io/ramp.xds/reference/L_module_list.md)
  : L modules
- [`xds_info_dynamical_components`](https://dd-harp.github.io/ramp.xds/reference/xds_info_dynamical_components.md)
  : Dynamical Components

### Model Structure

Setting the value of structural parameters

- [`xds_info_patch_dynamics`](https://dd-harp.github.io/ramp.xds/reference/xds_info_patch_dynamics.md)
  : Patch Dynamics
- [`xds_info_strata`](https://dd-harp.github.io/ramp.xds/reference/xds_info_strata.md)
  : Population Strata
- [`xds_info_human_populations`](https://dd-harp.github.io/ramp.xds/reference/xds_info_human_populations.md)
  : The human population
- [`xds_info_aquatic_habitats`](https://dd-harp.github.io/ramp.xds/reference/xds_info_aquatic_habitats.md)
  : Aquatic Habitats

### Basic Setup Options

Search Weights, Mosquito Dispersal, Time Spent

- [`xds_info_search_weights`](https://dd-harp.github.io/ramp.xds/reference/xds_info_search_weights.md)
  : Search Weights
- [`xds_info_heterogeneous_biting_rates`](https://dd-harp.github.io/ramp.xds/reference/xds_info_heterogeneous_biting_rates.md)
  : Heterogeneous Biting Rates
- [`xds_info_mosquito_dispersal`](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_dispersal.md)
  : Mosquito Dispersal
- [`xds_info_time_spent`](https://dd-harp.github.io/ramp.xds/reference/xds_info_time_spent.md)
  : Time Spent

### Adding Features

Forcing, Malaria Control

- [`xds_info_trivial_forcing`](https://dd-harp.github.io/ramp.xds/reference/xds_info_trivial_forcing.md)
  : Forcing with Trivial Modules

- [`xds_info_forcing`](https://dd-harp.github.io/ramp.xds/reference/xds_info_forcing.md)
  :

  **`ramp.xds`**: Forcing

- [`xds_info_port`](https://dd-harp.github.io/ramp.xds/reference/xds_info_port.md)
  : Ports

- [`xds_info_junction`](https://dd-harp.github.io/ramp.xds/reference/xds_info_junction.md)
  : Junctions

- [`xds_info_heterogeneity`](https://dd-harp.github.io/ramp.xds/reference/xds_info_heterogeneity.md)
  : Heterogeneity

- [`xds_info_environmental_heterogeneity`](https://dd-harp.github.io/ramp.xds/reference/xds_info_environmental_heterogeneity.md)
  : Environmental Heterogeneity

### The xds model object

- [`xds_object`](https://dd-harp.github.io/ramp.xds/reference/xds_object.md)
  :

  The **`xds`** Model Object

- [`xds_info_dynamical_components`](https://dd-harp.github.io/ramp.xds/reference/xds_info_dynamical_components.md)
  : Dynamical Components

- [`xds_info_interfaces`](https://dd-harp.github.io/ramp.xds/reference/xds_info_interfaces.md)
  : Dynamical Interfaces

## Solve

Methods to numerically solve differential equations and get the outputs

- [`xds_solve()`](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)
  : Solve a Dynamical System
- [`xds_stable_orbit()`](https://dd-harp.github.io/ramp.xds/reference/xds_stable_orbit.md)
  : Compute stable orbits
- [`dts_stable_orbit()`](https://dd-harp.github.io/ramp.xds/reference/dts_stable_orbit.md)
  : Compute the stable orbit for a discrete time system
- [`burnin()`](https://dd-harp.github.io/ramp.xds/reference/burnin.md) :
  Burn In
- [`xds_info_parsing`](https://dd-harp.github.io/ramp.xds/reference/xds_info_parsing.md)
  : Parsing

### Variable

Inspect initial values. Get the final state. Copy final state to initial
values.

- [`get_inits()`](https://dd-harp.github.io/ramp.xds/reference/get_inits.md)
  : Get the stored initial values, \\y_0\\
- [`get_last()`](https://dd-harp.github.io/ramp.xds/reference/get_last.md)
  : Get the last state
- [`last_to_inits()`](https://dd-harp.github.io/ramp.xds/reference/last_to_inits.md)
  : Set the initial values to the last values of the last simulation

### Get Outputs

- [`get_XH_orbits()`](https://dd-harp.github.io/ramp.xds/reference/get_XH_orbits.md)
  :

  Get orbits (**XH**)

- [`get_MY_orbits()`](https://dd-harp.github.io/ramp.xds/reference/get_MY_orbits.md)
  :

  Get orbits (**MY**)

- [`get_L_orbits()`](https://dd-harp.github.io/ramp.xds/reference/get_L_orbits.md)
  :

  Get orbits (**L**)

- [`get_EIR()`](https://dd-harp.github.io/ramp.xds/reference/get_EIR.md)
  : Get the EIR

- [`get_PR()`](https://dd-harp.github.io/ramp.xds/reference/get_PR.md) :

  Get the *Pf*PR from a malaria model

### Compute Steady States

- [`xds_steady()`](https://dd-harp.github.io/ramp.xds/reference/xds_steady.md)
  : Solve for the steady state

- [`dts_steady()`](https://dd-harp.github.io/ramp.xds/reference/dts_steady.md)
  : Compute the steady state of a discrete time system

- [`steady_state_X()`](https://dd-harp.github.io/ramp.xds/reference/steady_state_X.md)
  :

  Steady states for **X**

- [`steady_state_XH()`](https://dd-harp.github.io/ramp.xds/reference/steady_state_XH.md)
  :

  Steady states for **XH**

- [`steady_state_M()`](https://dd-harp.github.io/ramp.xds/reference/steady_state_M.md)
  :

  Compute steady states for **M**

- [`steady_state_MY()`](https://dd-harp.github.io/ramp.xds/reference/steady_state_MY.md)
  :

  Compute steady states for **MY**

- [`steady_state_Y()`](https://dd-harp.github.io/ramp.xds/reference/steady_state_Y.md)
  :

  Compute steady states for **Y**

- [`steady_state_L()`](https://dd-harp.github.io/ramp.xds/reference/steady_state_L.md)
  :

  Compute steady states for **L** component modules

## Plot

### PR & EIR

Plot the prevalence and the entomological inoculation rate

- [`xds_plot_PR()`](https://dd-harp.github.io/ramp.xds/reference/xds_plot_PR.md)
  : Plot the prevalence / parasite rate (PR) from a model of human
  infection and immunity

- [`xds_lines_PR()`](https://dd-harp.github.io/ramp.xds/reference/xds_lines_PR.md)
  : Add lines for the prevalence / parasite rate (PR) from a model of
  human infection and immunity

- [`xds_plot_EIR()`](https://dd-harp.github.io/ramp.xds/reference/xds_plot_EIR.md)
  :

  Plot the EIR *vs.* time

- [`xds_lines_EIR()`](https://dd-harp.github.io/ramp.xds/reference/xds_lines_EIR.md)
  :

  Add lines for the EIR *vs.* time

### Adult Mosquito

Plot common variables and terms

- [`xds_plot_M()`](https://dd-harp.github.io/ramp.xds/reference/xds_plot_M.md)
  : Plot adult mosquito population density
- [`xds_lines_M()`](https://dd-harp.github.io/ramp.xds/reference/xds_lines_M.md)
  : Add lines for adult mosquito population density
- [`xds_plot_Y()`](https://dd-harp.github.io/ramp.xds/reference/xds_plot_Y.md)
  : Plot the density of infected and infective mosquitoes
- [`xds_lines_Y()`](https://dd-harp.github.io/ramp.xds/reference/xds_lines_Y.md)
  : Add lines for the density of infected and infective mosquitoes
- [`xds_plot_Z()`](https://dd-harp.github.io/ramp.xds/reference/xds_plot_Z.md)
  : Plot the density of infective mosquitoes
- [`xds_lines_Z()`](https://dd-harp.github.io/ramp.xds/reference/xds_lines_Z.md)
  : Add lines for the density of infective mosquitoes
- [`xds_plot_Y_fracs()`](https://dd-harp.github.io/ramp.xds/reference/xds_plot_Y_fracs.md)
  : Plot the fraction of infected and infective mosquitoes
- [`xds_lines_Y_fracs()`](https://dd-harp.github.io/ramp.xds/reference/xds_lines_Y_fracs.md)
  : Add lines for the fraction of infected and infective mosquitoes
- [`xds_plot_Z_fracs()`](https://dd-harp.github.io/ramp.xds/reference/xds_plot_Z_fracs.md)
  : Plot the fraction infective
- [`xds_lines_Z_fracs()`](https://dd-harp.github.io/ramp.xds/reference/xds_lines_Z_fracs.md)
  : Add lines for the fraction of infected and infective mosquitoes

### Aquatic Mosquito

Plot common variables and terms

- [`xds_plot_L()`](https://dd-harp.github.io/ramp.xds/reference/xds_plot_L.md)
  : Plot aquatic mosquito population density
- [`xds_lines_L()`](https://dd-harp.github.io/ramp.xds/reference/xds_lines_L.md)
  : Add lines for aquatic mosquito population density

### Show Forcing

Show patterns for forced systems

- [`show_season()`](https://dd-harp.github.io/ramp.xds/reference/show_season.md)
  : Plot the seasonal pattern
- [`show_trend()`](https://dd-harp.github.io/ramp.xds/reference/show_trend.md)
  : Plot the Temporal Trend
- [`show_shock()`](https://dd-harp.github.io/ramp.xds/reference/show_shock.md)
  : Plot the temporal shock

## Get

Inspect elements of the xds object

### Get Orbits

Get orbits

- [`get_XH_orbits()`](https://dd-harp.github.io/ramp.xds/reference/get_XH_orbits.md)
  :

  Get orbits (**XH**)

- [`get_L_orbits()`](https://dd-harp.github.io/ramp.xds/reference/get_L_orbits.md)
  :

  Get orbits (**L**)

- [`get_PR()`](https://dd-harp.github.io/ramp.xds/reference/get_PR.md) :

  Get the *Pf*PR from a malaria model

- [`get_EIR()`](https://dd-harp.github.io/ramp.xds/reference/get_EIR.md)
  : Get the EIR

- [`get_H()`](https://dd-harp.github.io/ramp.xds/reference/get_H.md) :
  Get human population density

- [`get_M()`](https://dd-harp.github.io/ramp.xds/reference/get_M.md) :
  Get mosquito population density

### Get Parameters

Get model parameters

- [`get_XH_pars()`](https://dd-harp.github.io/ramp.xds/reference/get_XH_pars.md)
  :

  Get parameters (**XH**)

- [`get_TimeSpent_matrix()`](https://dd-harp.github.io/ramp.xds/reference/get_TimeSpent_matrix.md)
  : Get the Time Spent Matrix

- [`get_MY_pars()`](https://dd-harp.github.io/ramp.xds/reference/get_MY_pars.md)
  :

  Get parameters (**MY**)

- [`get_f()`](https://dd-harp.github.io/ramp.xds/reference/get_f.md) :
  Get the feeding rate(s)

- [`get_g()`](https://dd-harp.github.io/ramp.xds/reference/get_g.md) :
  Get the adult mosquito mortality rate(s)

- [`get_q()`](https://dd-harp.github.io/ramp.xds/reference/get_q.md) :
  Get the human fraction(s)

- [`get_sigma()`](https://dd-harp.github.io/ramp.xds/reference/get_sigma.md)
  : Get the patch emigration rates

- [`get_K_matrix()`](https://dd-harp.github.io/ramp.xds/reference/get_K_matrix.md)
  : Get the Mosquito Dispersal Matrix

- [`get_L_pars()`](https://dd-harp.github.io/ramp.xds/reference/get_L_pars.md)
  :

  Get parameters (**L**)

- [`get_V_pars()`](https://dd-harp.github.io/ramp.xds/reference/get_V_pars.md)
  : Return the parameters as a list

### Get Variables

Get initial values and indices

- [`get_XH_vars()`](https://dd-harp.github.io/ramp.xds/reference/get_XH_vars.md)
  :

  List variables (**XH**)

- [`get_XH_inits()`](https://dd-harp.github.io/ramp.xds/reference/get_XH_inits.md)
  : A function to set up XH_obj

- [`get_MY_vars()`](https://dd-harp.github.io/ramp.xds/reference/get_MY_vars.md)
  :

  List variables (**MY**)

- [`get_MY_inits()`](https://dd-harp.github.io/ramp.xds/reference/get_MY_inits.md)
  : Return initial values as a vector

- [`get_L_vars()`](https://dd-harp.github.io/ramp.xds/reference/get_L_vars.md)
  :

  List variables (**L**)

- [`get_L_inits()`](https://dd-harp.github.io/ramp.xds/reference/get_L_inits.md)
  :

  Get initial values (**L**)

- [`get_variables()`](https://dd-harp.github.io/ramp.xds/reference/get_variables.md)
  : Get Variables

### Get Forcing

Get the parameters from trace functions

- [`get_mean_forcing()`](https://dd-harp.github.io/ramp.xds/reference/get_mean_forcing.md)
  : Get mean forcing

- [`get_season()`](https://dd-harp.github.io/ramp.xds/reference/get_season.md)
  : Get seasonality parameters

- [`get_season_phase()`](https://dd-harp.github.io/ramp.xds/reference/get_season_phase.md)
  :

  Get seasonality parameter `phase`

- [`get_season_bottom()`](https://dd-harp.github.io/ramp.xds/reference/get_season_bottom.md)
  :

  Get seasonality parameter `bottom`

- [`get_season_pw()`](https://dd-harp.github.io/ramp.xds/reference/get_season_pw.md)
  :

  Get seasonality parameter `pw`

- [`get_trend()`](https://dd-harp.github.io/ramp.xds/reference/get_trend.md)
  : Get the trend parameters

- [`get_shock()`](https://dd-harp.github.io/ramp.xds/reference/get_shock.md)
  : Get the shock parameters

- [`get_spline()`](https://dd-harp.github.io/ramp.xds/reference/get_spline.md)
  : Get spline interpolation points

- [`get_spline_s()`](https://dd-harp.github.io/ramp.xds/reference/get_spline_s.md)
  : Get spline interpolation points

## Change

Change parameters

- [`change_XH_pars()`](https://dd-harp.github.io/ramp.xds/reference/change_XH_pars.md)
  :

  Change parameters (**XH**)

- [`change_MY_pars()`](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.md)
  :

  Change parameters (**MY**)

- [`change_L_pars()`](https://dd-harp.github.io/ramp.xds/reference/change_L_pars.md)
  :

  Change parameters (**L**)

- [`change_H()`](https://dd-harp.github.io/ramp.xds/reference/change_H.md)
  : Change human population density

- [`change_mean_forcing()`](https://dd-harp.github.io/ramp.xds/reference/change_mean_forcing.md)
  : Change mean forcing

- [`change_season()`](https://dd-harp.github.io/ramp.xds/reference/change_season.md)
  : Change season parameters

- [`change_trend()`](https://dd-harp.github.io/ramp.xds/reference/change_trend.md)
  : Set the interpolating points

- [`change_TimeSpent_matrix()`](https://dd-harp.github.io/ramp.xds/reference/change_TimeSpent_matrix.md)
  : Set up (or change) a Time Spent matrix

- [`change_K_matrix()`](https://dd-harp.github.io/ramp.xds/reference/change_K_matrix.md)
  : Change Mosquito Dispersal Matrix

## Save & Read

Build and store **`xds`** model objects

- [`saveXDS()`](https://dd-harp.github.io/ramp.xds/reference/saveXDS.md)
  :

  saveRDS for `xds` Objects

- [`readXDS()`](https://dd-harp.github.io/ramp.xds/reference/readXDS.md)
  :

  readRDS for `xds` Objects

## XH Component

Models of human / host infection dynamics - immunity - disease -
infectiousness - diagnostics and detection

- [`XH_module_list`](https://dd-harp.github.io/ramp.xds/reference/XH_module_list.md)
  : XH modules
- [`XH_functions`](https://dd-harp.github.io/ramp.xds/reference/XH_functions.md)
  : Generic methods for the XH component
- [`XH_get`](https://dd-harp.github.io/ramp.xds/reference/XH_get.md) :
  Get functions for XH
- [`XH_change`](https://dd-harp.github.io/ramp.xds/reference/XH_change.md)
  : A list of functions to change the XH component
- [`XH_setup`](https://dd-harp.github.io/ramp.xds/reference/XH_setup.md)
  : Setup functions for the XH component

### Modules

XH component module implementations

- [`trivial_XH`](https://dd-harp.github.io/ramp.xds/reference/trivial_XH.md)
  :

  `trivial` — **XH** module

- [`SIS`](https://dd-harp.github.io/ramp.xds/reference/SIS.md) :

  The `SIS` module for the XH component

- [`hMoI`](https://dd-harp.github.io/ramp.xds/reference/hMoI.md) :

  The `hMoI` module for the XH component

### Time Spent

Utilities for Time Spent Matrices

- [`xds_info_time_spent`](https://dd-harp.github.io/ramp.xds/reference/xds_info_time_spent.md)
  : Time Spent
- [`xds_info_time_at_risk`](https://dd-harp.github.io/ramp.xds/reference/xds_info_time_at_risk.md)
  : Time at Risk
- [`setup_TimeSpent()`](https://dd-harp.github.io/ramp.xds/reference/setup_TimeSpent.md)
  : Make a time spent matrix, called TimeSpent
- [`change_TimeSpent_matrix()`](https://dd-harp.github.io/ramp.xds/reference/change_TimeSpent_matrix.md)
  : Set up (or change) a Time Spent matrix
- [`make_TimeSpent_athome()`](https://dd-harp.github.io/ramp.xds/reference/make_TimeSpent_athome.md)
  : Make a mosquito dispersal matrix, called TimeSpent
- [`make_TimeSpent_xy()`](https://dd-harp.github.io/ramp.xds/reference/make_TimeSpent_xy.md)
  : Make a mosquito dispersal matrix, called TimeSpent

Utilities for Time Spent Matrices

- [`change_XH_inits()`](https://dd-harp.github.io/ramp.xds/reference/change_XH_inits.md)
  :

  Change initial values (**XH**)

- [`get_XH_pars()`](https://dd-harp.github.io/ramp.xds/reference/get_XH_pars.md)
  :

  Get parameters (**XH**)

- [`change_XH_pars()`](https://dd-harp.github.io/ramp.xds/reference/change_XH_pars.md)
  :

  Change parameters (**XH**)

- [`get_HTC()`](https://dd-harp.github.io/ramp.xds/reference/get_HTC.md)
  : Compute the human transmitting capacity

- [`xds_plot_X()`](https://dd-harp.github.io/ramp.xds/reference/xds_plot_X.md)
  :

  Default plotting for epidemiology (**X**)

- [`steady_state_X()`](https://dd-harp.github.io/ramp.xds/reference/steady_state_X.md)
  :

  Steady states for **X**

- [`steady_state_XH()`](https://dd-harp.github.io/ramp.xds/reference/steady_state_XH.md)
  :

  Steady states for **XH**

## MY Component

Adult mosquito ecology and infection dynamics

- [`MY_module_list`](https://dd-harp.github.io/ramp.xds/reference/MY_module_list.md)
  : MY Modules
- [`MY_functions`](https://dd-harp.github.io/ramp.xds/reference/MY_functions.md)
  : Generic Methods for the MY Component
- [`MY_get`](https://dd-harp.github.io/ramp.xds/reference/MY_get.md) :
  Get Functions for the MY Component
- [`MY_change`](https://dd-harp.github.io/ramp.xds/reference/MY_change.md)
  : Change Functions for the MY Component
- [`MY_setup`](https://dd-harp.github.io/ramp.xds/reference/MY_setup.md)
  : Setup Functions for the MY Component

### Modules

- [`trivial_MY`](https://dd-harp.github.io/ramp.xds/reference/trivial_MY.md)
  :

  `trivial` — **MY** module

- [`basicM`](https://dd-harp.github.io/ramp.xds/reference/basicM.md) :

  The `basicM` Module for the MY Component

- [`SI`](https://dd-harp.github.io/ramp.xds/reference/SI.md) :

  The `SI` module for the MY component

- [`macdonald`](https://dd-harp.github.io/ramp.xds/reference/macdonald.md)
  :

  The `macdonald` module (MY component)

- [`GeM`](https://dd-harp.github.io/ramp.xds/reference/GeM.md) :

  The `GeM` module for the MY component

- [`RMdts`](https://dd-harp.github.io/ramp.xds/reference/RMdts.md) :

  The `RMdts` module for the MY component

## L Component

Immagure mosquito ecology in aquatic habitats

- [`L_functions`](https://dd-harp.github.io/ramp.xds/reference/L_functions.md)
  : Generic methods for the L component

- [`L_module_list`](https://dd-harp.github.io/ramp.xds/reference/L_module_list.md)
  : L modules

- [`L_get`](https://dd-harp.github.io/ramp.xds/reference/L_get.md) :

  Get functions (**L**)

- [`L_change`](https://dd-harp.github.io/ramp.xds/reference/L_change.md)
  :

  Change functions (**L**)

- [`L_setup`](https://dd-harp.github.io/ramp.xds/reference/L_setup.md) :

  Setup functions (**L**)

### L Modules

Models for aquatic mosquito development and maturation in aquatic
habitats

- [`trivial_L`](https://dd-harp.github.io/ramp.xds/reference/trivial_L.md)
  :

  `trivial` — **L** module

- [`basicL`](https://dd-harp.github.io/ramp.xds/reference/basicL.md) :

  The `basicL` module for the L component

### Other State Variables

Compute the derivatives for other variables

- [`variables`](https://dd-harp.github.io/ramp.xds/reference/variables.md)
  : Variables

- [`Other_State_Variables`](https://dd-harp.github.io/ramp.xds/reference/Other_State_Variables.md)
  : Other State Variables

- [`add_variable()`](https://dd-harp.github.io/ramp.xds/reference/add_variable.md)
  : Add Variable

- [`setup_other_variables()`](https://dd-harp.github.io/ramp.xds/reference/setup_other_variables.md)
  : Set Up the first (null) other variable

- [`setup_V_ix(`*`<setup>`*`)`](https://dd-harp.github.io/ramp.xds/reference/setup_V_ix.setup.md)
  : Compute Other Variables

- [`setup_V_obj()`](https://dd-harp.github.io/ramp.xds/reference/setup_V_obj.md)
  :

  Setup an **V** Module (Human / Host Epidemiology & Demography)

- [`get_V_vars()`](https://dd-harp.github.io/ramp.xds/reference/get_V_vars.md)
  : Get Variables by Name

- [`setup_V_ix()`](https://dd-harp.github.io/ramp.xds/reference/setup_V_ix.md)
  : Add indices for human population to parameter list

- [`get_V_ix()`](https://dd-harp.github.io/ramp.xds/reference/get_V_ix.md)
  : Add indices for human population to parameter list

- [`change_V_pars()`](https://dd-harp.github.io/ramp.xds/reference/change_V_pars.md)
  : Set new X parameter values

- [`setup_V_inits()`](https://dd-harp.github.io/ramp.xds/reference/setup_V_inits.md)
  :

  Setup Initial Values for **V** Modules

- [`get_V_inits()`](https://dd-harp.github.io/ramp.xds/reference/get_V_inits.md)
  : A function to set up V_obj

- [`change_V_inits()`](https://dd-harp.github.io/ramp.xds/reference/change_V_inits.md)
  : Set new X parameter values

- [`change_spline()`](https://dd-harp.github.io/ramp.xds/reference/change_spline.md)
  : Change spline interpolation points

- [`change_spline_y()`](https://dd-harp.github.io/ramp.xds/reference/change_spline_y.md)
  : Change y values for spline interpolation points

- [`change_shock()`](https://dd-harp.github.io/ramp.xds/reference/change_shock.md)
  : Change shock parameters

## Blood Feeding & Transmission

- [`xds_info_blood_feeding`](https://dd-harp.github.io/ramp.xds/reference/xds_info_blood_feeding.md)
  : Blood Feeding
- [`xds_info_local_fraction`](https://dd-harp.github.io/ramp.xds/reference/xds_info_local_fraction.md)
  : The Local Fraction
- [`xds_info_available_blood_hosts`](https://dd-harp.github.io/ramp.xds/reference/xds_info_available_blood_hosts.md)
  : Available Blood
- [`xds_info_search_weights`](https://dd-harp.github.io/ramp.xds/reference/xds_info_search_weights.md)
  : Search Weights
- [`xds_info_relative_biting_rates`](https://dd-harp.github.io/ramp.xds/reference/xds_info_relative_biting_rates.md)
  : Relative Biting Rate
- [`xds_info_transmission`](https://dd-harp.github.io/ramp.xds/reference/xds_info_transmission.md)
  : Transmission
- [`check_XY_interface()`](https://dd-harp.github.io/ramp.xds/reference/check_XY_interface.md)
  : Check the XY Interface
- [`make_residency_matrix()`](https://dd-harp.github.io/ramp.xds/reference/make_residency_matrix.md)
  : Create the Residency Matrix
- [`view_residency_matrix()`](https://dd-harp.github.io/ramp.xds/reference/view_residency_matrix.md)
  : View residence membership

## Exposure

Compute the FoI from local EIR and travel

- [`xds_info_exposure`](https://dd-harp.github.io/ramp.xds/reference/xds_info_exposure.md)
  : Exposure
- [`xds_info_environmental_heterogeneity`](https://dd-harp.github.io/ramp.xds/reference/xds_info_environmental_heterogeneity.md)
  : Environmental Heterogeneity
- [`setup_exposure()`](https://dd-harp.github.io/ramp.xds/reference/setup_exposure.md)
  : Set Up Exposure
- [`foi2eir()`](https://dd-harp.github.io/ramp.xds/reference/foi2eir.md)
  : Convert FoI to EIR
- [`ar2eir()`](https://dd-harp.github.io/ramp.xds/reference/ar2eir.md) :
  Convert AR to EIR
- [`make_exposure_pois()`](https://dd-harp.github.io/ramp.xds/reference/make_exposure_pois.md)
  : Make a Poisson Exposure Model Object
- [`make_exposure_nb()`](https://dd-harp.github.io/ramp.xds/reference/make_exposure_nb.md)
  : Make a nbson Exposure Model Object

## Malaria Importation

Functions to model travel & visitors

- [`xds_info_importation`](https://dd-harp.github.io/ramp.xds/reference/xds_info_importation.md)
  : Malaria Importation
- [`xds_info_malaria_importation`](https://dd-harp.github.io/ramp.xds/reference/xds_info_malaria_importation.md)
  : Malaria Importation

### Travel

Time spent traveling & the travel EIR

- [`xds_info_travel_malaria`](https://dd-harp.github.io/ramp.xds/reference/xds_info_travel_malaria.md)
  : Travel Malaria
- [`xds_port_travel_eir`](https://dd-harp.github.io/ramp.xds/reference/xds_port_travel_eir.md)
  : Travel EIR
- [`xds_port_time_spent_here`](https://dd-harp.github.io/ramp.xds/reference/xds_port_time_spent_here.md)
  : Time Spent Here
- [`change_time_at_home()`](https://dd-harp.github.io/ramp.xds/reference/change_time_at_home.md)
  : Change the time at home
- [`change_travel_EIR()`](https://dd-harp.github.io/ramp.xds/reference/change_travel_EIR.md)
  : Change the travel EIR

### Visitors

Blood Feeding on non-Residents

- [`xds_info_visitors`](https://dd-harp.github.io/ramp.xds/reference/xds_info_visitors.md)
  : Visitors
- [`xds_port_visitors`](https://dd-harp.github.io/ramp.xds/reference/xds_port_visitors.md)
  : Visitors
- [`xds_port_visitor_kappa`](https://dd-harp.github.io/ramp.xds/reference/xds_port_visitor_kappa.md)
  : Visitors Infectiousness

## Mosquito Bionomics

Methods to compute or update mosquito bionomic parameters

- [`xds_info_mosquito_bionomics`](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_bionomics.md)
  : Mosquito Bionomics
- [`xds_info_blood_feeding`](https://dd-harp.github.io/ramp.xds/reference/xds_info_blood_feeding.md)
  : Blood Feeding
- [`xds_info_mosquito_dispersal`](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_dispersal.md)
  : Mosquito Dispersal
- [`xds_info_mosquito_demography`](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_demography.md)
  : Mosquito Demography

### Dynamical Parameters

Compute bionomic parameters as functional responses to resource
availability

- [`setup_feeding_rate()`](https://dd-harp.github.io/ramp.xds/reference/setup_feeding_rate.md)
  : Setup Blood Feeding Rate
- [`F_feeding_rate()`](https://dd-harp.github.io/ramp.xds/reference/F_feeding_rate.md)
  : Compute the blood feeding rate, f
- [`F_feeding_rate(`*`<static>`*`)`](https://dd-harp.github.io/ramp.xds/reference/F_feeding_rate.static.md)
  : Constant baseline blood feeding rate
- [`setup_feeding_rate_B2()`](https://dd-harp.github.io/ramp.xds/reference/setup_feeding_rate_B2.md)
  : Setup Blood Feeding Bionomic Object
- [`F_feeding_rate(`*`<B2>`*`)`](https://dd-harp.github.io/ramp.xds/reference/F_feeding_rate.B2.md)
  : Type 2 functional response for the blood feeding rate
- [`setup_human_frac()`](https://dd-harp.github.io/ramp.xds/reference/setup_human_frac.md)
  : Setup a Human Fraction Bionomic Object
- [`F_human_frac()`](https://dd-harp.github.io/ramp.xds/reference/F_human_frac.md)
  : Compute the blood qeeding rate, q
- [`F_human_frac(`*`<static>`*`)`](https://dd-harp.github.io/ramp.xds/reference/F_human_frac.static.md)
  : Static model for the blood feeding rate
- [`setup_mozy_mort()`](https://dd-harp.github.io/ramp.xds/reference/setup_mozy_mort.md)
  : Setup a Mosquito Mortality Bionomic Object
- [`setup_sigma_obj()`](https://dd-harp.github.io/ramp.xds/reference/setup_sigma_obj.md)
  : Setup a Patch Emigration Bionomic Object
- [`setup_sigma_obj_BQS()`](https://dd-harp.github.io/ramp.xds/reference/setup_sigma_obj_BQS.md)
  : Setup Blood Feeding Bionomic Object
- [`setup_mu_obj()`](https://dd-harp.github.io/ramp.xds/reference/setup_mu_obj.md)
  : Setup a Dispersal Loss Bionomic Object
- [`setup_nu_obj()`](https://dd-harp.github.io/ramp.xds/reference/setup_nu_obj.md)
  : Setup Laying Rate Bionomic Object
- [`setup_nu_obj_Q2()`](https://dd-harp.github.io/ramp.xds/reference/setup_nu_obj_Q2.md)
  : Setup Blood Feeding Bionomic Object
- [`setup_psi_obj()`](https://dd-harp.github.io/ramp.xds/reference/setup_psi_obj.md)
  : Setup a Human Fraction Bionomic Object
- [`setup_phi_obj()`](https://dd-harp.github.io/ramp.xds/reference/setup_phi_obj.md)
  : Setup a Human Fraction Bionomic Object
- [`setup_xi_obj()`](https://dd-harp.github.io/ramp.xds/reference/setup_xi_obj.md)
  : Setup a Human Fraction Bionomic Object
- [`setup_theta_obj()`](https://dd-harp.github.io/ramp.xds/reference/setup_theta_obj.md)
  : Setup a Human Fraction Bionomic Object

### Mosquito Dispersal

Specialized methods to set up mosquito dispersal matrices

- [`xds_info_mosquito_dispersal`](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_dispersal.md)
  : Mosquito Dispersal
- [`xds_info_mosquito_demography`](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_demography.md)
  : Mosquito Demography
- [`setup_K_matrix()`](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)
  : Setup Mosquito Dispersal Matrix
- [`change_K_matrix()`](https://dd-harp.github.io/ramp.xds/reference/change_K_matrix.md)
  : Change Mosquito Dispersal Matrix
- [`get_K_matrix()`](https://dd-harp.github.io/ramp.xds/reference/get_K_matrix.md)
  : Get the Mosquito Dispersal Matrix
- [`make_K_matrix_herethere()`](https://dd-harp.github.io/ramp.xds/reference/make_K_matrix_herethere.md)
  : Make a Here-There Dispersal Matrix
- [`make_K_matrix_xy()`](https://dd-harp.github.io/ramp.xds/reference/make_K_matrix_xy.md)
  : make a Kernel-Based Mosquito Dispersal Matrix

## Forcing

- [`xds_info_forcing`](https://dd-harp.github.io/ramp.xds/reference/xds_info_forcing.md)
  :

  **`ramp.xds`**: Forcing

- [`xds_info_trivial_forcing`](https://dd-harp.github.io/ramp.xds/reference/xds_info_trivial_forcing.md)
  : Forcing with Trivial Modules

- [`xds_info_vector_control`](https://dd-harp.github.io/ramp.xds/reference/xds_info_vector_control.md)
  : Vector Control

- [`xds_info_health`](https://dd-harp.github.io/ramp.xds/reference/xds_info_health.md)
  : Health

## Resources

The ports for resources

- [`xds_info_resources`](https://dd-harp.github.io/ramp.xds/reference/xds_info_resources.md)
  : Resources
- [`xds_info_availability`](https://dd-harp.github.io/ramp.xds/reference/xds_info_availability.md)
  : Resource Availability
- [`xds_info_available_blood_hosts`](https://dd-harp.github.io/ramp.xds/reference/xds_info_available_blood_hosts.md)
  : Available Blood
- [`xds_info_search_weights_blood`](https://dd-harp.github.io/ramp.xds/reference/xds_info_search_weights_blood.md)
  : Blood Search Weights
- [`xds_port_other_blood_hosts`](https://dd-harp.github.io/ramp.xds/reference/xds_port_other_blood_hosts.md)
  : Other Blood Hosts
- [`xds_port_blood_traps`](https://dd-harp.github.io/ramp.xds/reference/xds_port_blood_traps.md)
  : Blood Traps
- [`xds_info_available_habitats`](https://dd-harp.github.io/ramp.xds/reference/xds_info_available_habitats.md)
  : Habitat Availability
- [`xds_info_search_weights_habitat`](https://dd-harp.github.io/ramp.xds/reference/xds_info_search_weights_habitat.md)
  : Habitat Search Weights
- [`xds_info_available_sugar`](https://dd-harp.github.io/ramp.xds/reference/xds_info_available_sugar.md)
  : Sugar Availability
- [`xds_info_mosquito_traps`](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_traps.md)
  : Available Blood
- [`change_other_blood_hosts()`](https://dd-harp.github.io/ramp.xds/reference/change_other_blood_hosts.md)
  : Set static blood feeding search weights

## Habitats & Egg Laying

Mosquito Population Dynamical Interface

- [`xds_info_aquatic_habitats`](https://dd-harp.github.io/ramp.xds/reference/xds_info_aquatic_habitats.md)
  : Aquatic Habitats
- [`xds_info_search_weights_habitat`](https://dd-harp.github.io/ramp.xds/reference/xds_info_search_weights_habitat.md)
  : Habitat Search Weights
- [`xds_info_available_habitats`](https://dd-harp.github.io/ramp.xds/reference/xds_info_available_habitats.md)
  : Habitat Availability
- [`xds_info_egg_laying`](https://dd-harp.github.io/ramp.xds/reference/xds_info_egg_laying.md)
  : Egg Laying
- [`xds_port_bad_habitats`](https://dd-harp.github.io/ramp.xds/reference/xds_port_bad_habitats.md)
  : Unproductive Aquatic Habitats
- [`xds_port_ovitraps`](https://dd-harp.github.io/ramp.xds/reference/xds_port_ovitraps.md)
  : Ovitraps
- [`view_habitat_matrix()`](https://dd-harp.github.io/ramp.xds/reference/view_habitat_matrix.md)
  : View habitat membership, \\N\\
- [`change_habitat_weights()`](https://dd-harp.github.io/ramp.xds/reference/change_habitat_weights.md)
  : Change Habitat Search Weights
- [`change_bad_habitat()`](https://dd-harp.github.io/ramp.xds/reference/change_bad_habitat.md)
  : Change Bad Habitat Availability

## Functions

A function maker

- [`make_function()`](https://dd-harp.github.io/ramp.xds/reference/make_function.md)
  : Make a Function
- [`make_ts_function()`](https://dd-harp.github.io/ramp.xds/reference/make_ts_function.md)
  : Make a Time Series Function
- [`makepar_F_zero()`](https://dd-harp.github.io/ramp.xds/reference/makepar_F_zero.md)
  : parameters for make_function
- [`makepar_F_one()`](https://dd-harp.github.io/ramp.xds/reference/makepar_F_one.md)
  : parameters for make_function
- [`makepar_F_val()`](https://dd-harp.github.io/ramp.xds/reference/makepar_F_val.md)
  : parameters for make_function
- [`makepar_F_sin()`](https://dd-harp.github.io/ramp.xds/reference/makepar_F_sin.md)
  : parameters for make_function
- [`makepar_F_sum()`](https://dd-harp.github.io/ramp.xds/reference/makepar_F_sum.md)
  : parameters for make_function
- [`makepar_F_product()`](https://dd-harp.github.io/ramp.xds/reference/makepar_F_product.md)
  : parameters for make_function
- [`makepar_F_nproduct()`](https://dd-harp.github.io/ramp.xds/reference/makepar_F_nproduct.md)
  : parameters for make_function
- [`makepar_F_sigmoid()`](https://dd-harp.github.io/ramp.xds/reference/makepar_F_sigmoid.md)
  : Make Parameters for a Sigmoidal Function
- [`makepar_F_sharkfin()`](https://dd-harp.github.io/ramp.xds/reference/makepar_F_sharkfin.md)
  : Make Parameters for a Sharkfin Function
- [`makepar_F_sharkbite()`](https://dd-harp.github.io/ramp.xds/reference/makepar_F_sharkbite.md)
  : Make Parameters for a sharkbite Function
- [`makepar_F_type2()`](https://dd-harp.github.io/ramp.xds/reference/makepar_F_type2.md)
  : parameters for make_function
- [`makepar_F_spline()`](https://dd-harp.github.io/ramp.xds/reference/makepar_F_spline.md)
  : Make Parameters for a Spline
