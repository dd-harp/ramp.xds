# Package index

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
- [`dynamical_components`](https://dd-harp.github.io/ramp.xds/reference/dynamical_components.md)
  : Dynamical Components

### Model Structure

Setting the value of structural parameters

- [`patch_dynamics`](https://dd-harp.github.io/ramp.xds/reference/patch_dynamics.md)
  : Patch Dynamics
- [`human_populations`](https://dd-harp.github.io/ramp.xds/reference/human_populations.md)
  : The Human Population
- [`aquatic_habitats`](https://dd-harp.github.io/ramp.xds/reference/aquatic_habitats.md)
  : Aquatic Habitats

### Setup Options

Heterogeneous Biting & Spatial Dynamics

- [`trivial_forcing`](https://dd-harp.github.io/ramp.xds/reference/trivial_forcing.md)
  : Forcing with Trivial Modules
- [`search_weights`](https://dd-harp.github.io/ramp.xds/reference/search_weights.md)
  : Search Weights
- [`mosquito_dispersal`](https://dd-harp.github.io/ramp.xds/reference/mosquito_dispersal.md)
  : Mosquito Dispersal
- [`time_spent`](https://dd-harp.github.io/ramp.xds/reference/time_spent.md)
  : Time Spent

### Adding Features

Forcing

- [`port`](https://dd-harp.github.io/ramp.xds/reference/port.md) : Ports
- [`junction`](https://dd-harp.github.io/ramp.xds/reference/junction.md)
  : Junctions

### The xds model object

- [`xds_object`](https://dd-harp.github.io/ramp.xds/reference/xds_object.md)
  :

  The **`xds`** Model Object

- [`xds_object_frame`](https://dd-harp.github.io/ramp.xds/reference/xds_object_frame.md)
  : The xds object frame

## Dynamics Modules

### XH Modules

Human / host epidemiology & demography

- [`trivial_XH`](https://dd-harp.github.io/ramp.xds/reference/trivial_XH.md)
  :

  The `trivial` Module for the XH Component

- [`SIS`](https://dd-harp.github.io/ramp.xds/reference/SIS.md) :

  The `SIS` Module for the XH Component

- [`hMoI`](https://dd-harp.github.io/ramp.xds/reference/hMoI.md) :

  The `hMoI` Module for the XH Component

### MY Modules

Adult mosquito ecology and infection dynamics

- [`trivial_MY`](https://dd-harp.github.io/ramp.xds/reference/trivial_MY.md)
  :

  The `trivial` Module for the MY Component

- [`basicM`](https://dd-harp.github.io/ramp.xds/reference/basicM.md) :

  The `basicM` Module for the MY Component

- [`SI`](https://dd-harp.github.io/ramp.xds/reference/SI.md) :

  The `SI` Module for the MY Component

- [`macdonald`](https://dd-harp.github.io/ramp.xds/reference/macdonald.md)
  :

  The `macdonald` Module (MY Component)

- [`GeRM`](https://dd-harp.github.io/ramp.xds/reference/GeRM.md) :

  The `GeRM` Module for the MY Component

- [`RMdts`](https://dd-harp.github.io/ramp.xds/reference/RMdts.md) :

  The `RMdts` Module for the MY Component

### L Modules

Aquatic mosquito ecology

- [`trivial_L`](https://dd-harp.github.io/ramp.xds/reference/trivial_L.md)
  :

  The `trivial` Module for the L Component

- [`basicL`](https://dd-harp.github.io/ramp.xds/reference/basicL.md) :

  The `basicL` Module for the L Component

## Solve

Methods to numerically solve differential equations and get the outputs

- [`xds_solve()`](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)
  : Solve a Dynamical System

- [`make_times_dts()`](https://dd-harp.github.io/ramp.xds/reference/make_times_dts.md)
  : Times Utility for Discrete Time Systems

- [`make_times_xde()`](https://dd-harp.github.io/ramp.xds/reference/make_times_xde.md)
  : Times Utility for Differential Equations

- [`burnin()`](https://dd-harp.github.io/ramp.xds/reference/burnin.md) :
  Burn In

- [`get_inits()`](https://dd-harp.github.io/ramp.xds/reference/get_inits.md)
  : Get the stored initial values, \\y_0\\

- [`get_last()`](https://dd-harp.github.io/ramp.xds/reference/get_last.md)
  : Get the last state

- [`last_to_inits()`](https://dd-harp.github.io/ramp.xds/reference/last_to_inits.md)
  : Set the initial values to the last values of the last simulation

- [`get_XH_orbits()`](https://dd-harp.github.io/ramp.xds/reference/get_XH_orbits.md)
  :

  Get **XH** outputs

- [`get_MY_orbits()`](https://dd-harp.github.io/ramp.xds/reference/get_MY_orbits.md)
  :

  Get **MY** outputs

- [`get_L_orbits()`](https://dd-harp.github.io/ramp.xds/reference/get_L_orbits.md)
  :

  Get **L** outputs

- [`get_EIR()`](https://dd-harp.github.io/ramp.xds/reference/get_EIR.md)
  : Get the EIR

- [`get_PR()`](https://dd-harp.github.io/ramp.xds/reference/get_PR.md) :

  Get the *Pf*PR from a Malaria Model

### Stable Orbits

Compute Stable Orbits

- [`xds_stable_orbit()`](https://dd-harp.github.io/ramp.xds/reference/xds_stable_orbit.md)
  : Compute stable orbits
- [`dts_stable_orbit()`](https://dd-harp.github.io/ramp.xds/reference/dts_stable_orbit.md)
  : Solve for the steady state or stable orbit of a system of equations

### Steady States

Compute Steady States

- [`xds_steady()`](https://dd-harp.github.io/ramp.xds/reference/xds_steady.md)
  :

  Solve for the steady state of a system of equations using
  [rootSolve::steady](https://rdrr.io/pkg/rootSolve/man/steady.html)

- [`dts_steady()`](https://dd-harp.github.io/ramp.xds/reference/dts_steady.md)
  : Solve for the steady state of a system of equations

- [`steady_state_X()`](https://dd-harp.github.io/ramp.xds/reference/steady_state_X.md)
  :

  Steady States for **X**

- [`steady_state_XH()`](https://dd-harp.github.io/ramp.xds/reference/steady_state_XH.md)
  :

  Steady States for **XH**

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

  Compute steady states for **L** Component Modules

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

### Mosquito

Compute Steady States & Stable Orbits

- [`xds_plot_M()`](https://dd-harp.github.io/ramp.xds/reference/xds_plot_M.md)
  : Plot adult mosquito population density
- [`xds_lines_M()`](https://dd-harp.github.io/ramp.xds/reference/xds_lines_M.md)
  : Add lines for adult mosquito population density
- [`xds_plot_Y()`](https://dd-harp.github.io/ramp.xds/reference/xds_plot_Y.md)
  : Plot the density of infected and infective mosquitoes
- [`xds_lines_Y()`](https://dd-harp.github.io/ramp.xds/reference/xds_lines_Y.md)
  : Add lines for the density of infected and infective mosquitoes
- [`xds_plot_Z()`](https://dd-harp.github.io/ramp.xds/reference/xds_plot_Z.md)
  : Plot the density of infected and infective mosquitoes
- [`xds_lines_Z()`](https://dd-harp.github.io/ramp.xds/reference/xds_lines_Z.md)
  : Add lines for the density of infected and infective mosquitoes
- [`xds_plot_Y_fracs()`](https://dd-harp.github.io/ramp.xds/reference/xds_plot_Y_fracs.md)
  : Plot the fraction of infected and infective mosquitoes
- [`xds_lines_Y_fracs()`](https://dd-harp.github.io/ramp.xds/reference/xds_lines_Y_fracs.md)
  : Add lines for the fraction of infected and infective mosquitoes
- [`xds_plot_Z_fracs()`](https://dd-harp.github.io/ramp.xds/reference/xds_plot_Z_fracs.md)
  : Plot the fraction infective
- [`xds_lines_Z_fracs()`](https://dd-harp.github.io/ramp.xds/reference/xds_lines_Z_fracs.md)
  : Add lines for the fraction of infected and infective mosquitoes

### Show Forcing

Show patterns for forced systems

- [`show_season()`](https://dd-harp.github.io/ramp.xds/reference/show_season.md)
  : Plot the seasonal pattern
- [`show_trend()`](https://dd-harp.github.io/ramp.xds/reference/show_trend.md)
  : Plot the Temporal Trend
- [`show_shock()`](https://dd-harp.github.io/ramp.xds/reference/show_shock.md)
  : Plot the Temporal shock

## Get

Inspect elements of the xds object

### Get Orbits

Get orbits

- [`get_XH_orbits()`](https://dd-harp.github.io/ramp.xds/reference/get_XH_orbits.md)
  :

  Get **XH** outputs

- [`get_MY_orbits()`](https://dd-harp.github.io/ramp.xds/reference/get_MY_orbits.md)
  :

  Get **MY** outputs

- [`get_L_orbits()`](https://dd-harp.github.io/ramp.xds/reference/get_L_orbits.md)
  :

  Get **L** outputs

- [`get_H()`](https://dd-harp.github.io/ramp.xds/reference/get_H.md) :
  Get the initial values as a vector

- [`get_PR()`](https://dd-harp.github.io/ramp.xds/reference/get_PR.md) :

  Get the *Pf*PR from a Malaria Model

### Get Parameters

Get model parameters

- [`get_XH_pars()`](https://dd-harp.github.io/ramp.xds/reference/get_XH_pars.md)
  : Return the parameters as a list
- [`get_XH_ix()`](https://dd-harp.github.io/ramp.xds/reference/get_XH_ix.md)
  : Add indices for human population to parameter list
- [`get_TimeSpent_matrix()`](https://dd-harp.github.io/ramp.xds/reference/get_TimeSpent_matrix.md)
  : Get the Time Spent Matrix
- [`get_MY_pars()`](https://dd-harp.github.io/ramp.xds/reference/get_MY_pars.md)
  : Return the parameters as a list
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
- [`get_mean_forcing()`](https://dd-harp.github.io/ramp.xds/reference/get_mean_forcing.md)
  : Get mean forcing

### Get Variables

Get initial values and indices

- [`get_XH_vars()`](https://dd-harp.github.io/ramp.xds/reference/get_XH_vars.md)
  : Get Variables by Name

- [`get_XH_inits()`](https://dd-harp.github.io/ramp.xds/reference/get_XH_inits.md)
  : A function to set up XH_obj

- [`get_MY_vars()`](https://dd-harp.github.io/ramp.xds/reference/get_MY_vars.md)
  : Return the variables as a list

- [`get_MY_inits()`](https://dd-harp.github.io/ramp.xds/reference/get_MY_inits.md)
  : Return initial values as a vector

- [`get_L_vars()`](https://dd-harp.github.io/ramp.xds/reference/get_L_vars.md)
  :

  List **L** Component Variables

- [`get_L_inits()`](https://dd-harp.github.io/ramp.xds/reference/get_L_inits.md)
  :

  Get Initial Values for the **L** Component

- [`get_variables()`](https://dd-harp.github.io/ramp.xds/reference/get_variables.md)
  : Get Variables

### Get Trace Functions

Get XX

- [`get_mean_forcing()`](https://dd-harp.github.io/ramp.xds/reference/get_mean_forcing.md)
  : Get mean forcing
- [`get_season()`](https://dd-harp.github.io/ramp.xds/reference/get_season.md)
  : Get the Seasonal Pattern
- [`get_season_phase()`](https://dd-harp.github.io/ramp.xds/reference/get_season_phase.md)
  : Get phase
- [`get_season_bottom()`](https://dd-harp.github.io/ramp.xds/reference/get_season_bottom.md)
  : Get phase
- [`get_season_pw()`](https://dd-harp.github.io/ramp.xds/reference/get_season_pw.md)
  : Get pw for seasonality
- [`get_trend()`](https://dd-harp.github.io/ramp.xds/reference/get_trend.md)
  : Get the trend parameters
- [`get_spline()`](https://dd-harp.github.io/ramp.xds/reference/get_spline.md)
  : Get spline interpolation points
- [`get_spline_s()`](https://dd-harp.github.io/ramp.xds/reference/get_spline_s.md)
  : Get spline interpolation points

## Change

Change parameters

- [`change_XH_pars()`](https://dd-harp.github.io/ramp.xds/reference/change_XH_pars.md)
  : Set new X parameter values

- [`change_MY_pars()`](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.md)
  : Set new MY parameter values

- [`change_L_pars()`](https://dd-harp.github.io/ramp.xds/reference/change_L_pars.md)
  :

  Set **L** Component Parameters

- [`change_H()`](https://dd-harp.github.io/ramp.xds/reference/change_H.md)
  : Change human population density

- [`change_mean_forcing()`](https://dd-harp.github.io/ramp.xds/reference/change_mean_forcing.md)
  : Set mean forcing

- [`change_season()`](https://dd-harp.github.io/ramp.xds/reference/change_season.md)
  : Get seasonal pattern

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

## Habitats & Egg Laying

Mosquito Population Dynamic Interface

- [`aquatic_habitats`](https://dd-harp.github.io/ramp.xds/reference/aquatic_habitats.md)
  : Aquatic Habitats
- [`egg_laying`](https://dd-harp.github.io/ramp.xds/reference/egg_laying.md)
  : Egg Laying
- [`view_habitat_matrix()`](https://dd-harp.github.io/ramp.xds/reference/view_habitat_matrix.md)
  : View habitat membership, \\N\\
- [`change_habitat_weights()`](https://dd-harp.github.io/ramp.xds/reference/change_habitat_weights.md)
  : Change Habitat Search Weights
- [`change_bad_habitat()`](https://dd-harp.github.io/ramp.xds/reference/change_bad_habitat.md)
  : Change Habitat Search Weights

## Blood Feeding & Transmission

- [`blood_feeding`](https://dd-harp.github.io/ramp.xds/reference/blood_feeding.md)
  : Blood Feeding Overview
- [`Transmission()`](https://dd-harp.github.io/ramp.xds/reference/Transmission.md)
  : Transmission
- [`check_XY_interface()`](https://dd-harp.github.io/ramp.xds/reference/check_XY_interface.md)
  : Check the XY Interface
- [`make_residency_matrix()`](https://dd-harp.github.io/ramp.xds/reference/make_residency_matrix.md)
  : Create the Residency Matrix
- [`view_residency_matrix()`](https://dd-harp.github.io/ramp.xds/reference/view_residency_matrix.md)
  : View residence membership

## Exposure

Compute the FoI from local EIR and travel

- [`Exposure()`](https://dd-harp.github.io/ramp.xds/reference/Exposure.md)
  : Exposure
- [`environmental_heterogeneity`](https://dd-harp.github.io/ramp.xds/reference/environmental_heterogeneity.md)
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

## Mosquito Bionomics

Methods to compute or update mosquito bionomic parameters

- [`mosquito_bionomics`](https://dd-harp.github.io/ramp.xds/reference/mosquito_bionomics.md)
  : Mosquito Bionomics
- [`blood_feeding`](https://dd-harp.github.io/ramp.xds/reference/blood_feeding.md)
  : Blood Feeding Overview
- [`mosquito_dispersal`](https://dd-harp.github.io/ramp.xds/reference/mosquito_dispersal.md)
  : Mosquito Dispersal
- [`mosquito_demography`](https://dd-harp.github.io/ramp.xds/reference/mosquito_demography.md)
  : Mosquito Demography

### Dynamic Parameters

Compute bionomic parameters as functional responses to resource
availability

- [`setup_f_obj()`](https://dd-harp.github.io/ramp.xds/reference/setup_f_obj.md)
  : Setup Blood Feeding Bionomic Object
- [`F_feeding_rate()`](https://dd-harp.github.io/ramp.xds/reference/F_feeding_rate.md)
  : Compute the blood feeding rate, f
- [`F_feeding_rate(`*`<static>`*`)`](https://dd-harp.github.io/ramp.xds/reference/F_feeding_rate.static.md)
  : Constant baseline blood feeding rate
- [`setup_f_obj_B2()`](https://dd-harp.github.io/ramp.xds/reference/setup_f_obj_B2.md)
  : Setup Blood Feeding Bionomic Object
- [`F_feeding_rate(`*`<B2>`*`)`](https://dd-harp.github.io/ramp.xds/reference/F_feeding_rate.B2.md)
  : Type 2 functional response for the blood feeding rate
- [`setup_q_obj()`](https://dd-harp.github.io/ramp.xds/reference/setup_q_obj.md)
  : Setup a Human Fraction Bionomic Object
- [`F_human_frac()`](https://dd-harp.github.io/ramp.xds/reference/F_human_frac.md)
  : Compute the blood qeeding rate, q
- [`F_human_frac(`*`<static>`*`)`](https://dd-harp.github.io/ramp.xds/reference/F_human_frac.static.md)
  : Static model for the blood feeding rate
- [`setup_g_obj()`](https://dd-harp.github.io/ramp.xds/reference/setup_g_obj.md)
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

- [`mosquito_dispersal`](https://dd-harp.github.io/ramp.xds/reference/mosquito_dispersal.md)
  : Mosquito Dispersal
- [`mosquito_demography`](https://dd-harp.github.io/ramp.xds/reference/mosquito_demography.md)
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

## Malaria Importation

Functions to model travel & visitors

### Travel

Time spent traveling & the travel EIR

- [`setup_travel_object()`](https://dd-harp.github.io/ramp.xds/reference/setup_travel_object.md)
  : Setup the Travel Object
- [`change_time_at_home()`](https://dd-harp.github.io/ramp.xds/reference/change_time_at_home.md)
  : Change the travel EIR
- [`change_travel_EIR()`](https://dd-harp.github.io/ramp.xds/reference/change_travel_EIR.md)
  : Change the travel EIR
- [`setup_F_travel()`](https://dd-harp.github.io/ramp.xds/reference/setup_F_travel.md)
  : Set up no travel
- [`setup_F_travel_eir()`](https://dd-harp.github.io/ramp.xds/reference/setup_F_travel_eir.md)
  : Setup the Travel EIR
- [`setup_F_travel_eir(`*`<static>`*`)`](https://dd-harp.github.io/ramp.xds/reference/setup_F_travel_eir.static.md)
  : Set up no travel
- [`setup_F_travel_eir(`*`<ts_func>`*`)`](https://dd-harp.github.io/ramp.xds/reference/setup_F_travel_eir.ts_func.md)
  : Setup the Travel EIR
- [`travel_dynamics()`](https://dd-harp.github.io/ramp.xds/reference/travel_dynamics.md)
  : Time Spent Here

### Visitors

Blood Feeding on non-Residents

- [`setup_F_vis_kappa()`](https://dd-harp.github.io/ramp.xds/reference/setup_F_vis_kappa.md)
  : Set up no visitors
- [`setup_F_vis_kappa(`*`<ts_func>`*`)`](https://dd-harp.github.io/ramp.xds/reference/setup_F_vis_kappa.ts_func.md)
  : Set
- [`setup_F_visitors()`](https://dd-harp.github.io/ramp.xds/reference/setup_F_visitors.md)
  : Set up no visitors
- [`setup_F_visitors(`*`<ts_func>`*`)`](https://dd-harp.github.io/ramp.xds/reference/setup_F_visitors.ts_func.md)
  : Set up no visitors
- [`setup_visitor_object()`](https://dd-harp.github.io/ramp.xds/reference/setup_visitor_object.md)
  : Setup the Visitors Object
- [`visitor_dynamics()`](https://dd-harp.github.io/ramp.xds/reference/visitor_dynamics.md)
  : Availabilit of Visitors

## Forcing

- [`trivial_forcing`](https://dd-harp.github.io/ramp.xds/reference/trivial_forcing.md)
  : Forcing with Trivial Modules
- [`Forcing()`](https://dd-harp.github.io/ramp.xds/reference/Forcing.md)
  : Forcing
- [`Vector_Control`](https://dd-harp.github.io/ramp.xds/reference/Vector_Control.md)
  : Vector Control
- [`Resources()`](https://dd-harp.github.io/ramp.xds/reference/Resources.md)
  : Resources
- [`setup_forcing_object()`](https://dd-harp.github.io/ramp.xds/reference/setup_forcing_object.md)
  : Setup the Junction for Exogenous Forcing
- [`setup_vector_control_object()`](https://dd-harp.github.io/ramp.xds/reference/setup_vector_control_object.md)
  : Setup the Vector Control Junction
- [`setup_health_object()`](https://dd-harp.github.io/ramp.xds/reference/setup_health_object.md)
  : none set up for exogenous health

### Resources

The ports for resources

- [`setup_resources_object()`](https://dd-harp.github.io/ramp.xds/reference/setup_resources_object.md)
  : Setup Resources Object
- [`setup_sugar_object()`](https://dd-harp.github.io/ramp.xds/reference/setup_sugar_object.md)
  : Setup the Junction for Exogenous sugar
- [`setup_traps_object()`](https://dd-harp.github.io/ramp.xds/reference/setup_traps_object.md)
  : Setup the Junction for Exogenous traps
- [`setup_blood_host_object()`](https://dd-harp.github.io/ramp.xds/reference/setup_blood_host_object.md)
  : Setup the Blood Hosts Object
- [`change_blood_hosts()`](https://dd-harp.github.io/ramp.xds/reference/change_blood_hosts.md)
  : Set static blood feeding search weights

## Humans / Hosts (XH)

Models of human / host infection dynamics - immunity - disease -
infectiousness - diagnostics and detection

- [`change_XH_inits()`](https://dd-harp.github.io/ramp.xds/reference/change_XH_inits.md)
  : Set new X parameter values

- [`get_XH_pars()`](https://dd-harp.github.io/ramp.xds/reference/get_XH_pars.md)
  : Return the parameters as a list

- [`change_XH_pars()`](https://dd-harp.github.io/ramp.xds/reference/change_XH_pars.md)
  : Set new X parameter values

- [`get_HTC()`](https://dd-harp.github.io/ramp.xds/reference/get_HTC.md)
  : Compute the human transmitting capacity

- [`xds_plot_X()`](https://dd-harp.github.io/ramp.xds/reference/xds_plot_X.md)
  : Basic plotting for epidemiological models

- [`xds_plot_XH()`](https://dd-harp.github.io/ramp.xds/reference/xds_plot_XH.md)
  : Basic plotting for epidemiological models

- [`steady_state_X()`](https://dd-harp.github.io/ramp.xds/reference/steady_state_X.md)
  :

  Steady States for **X**

- [`steady_state_XH()`](https://dd-harp.github.io/ramp.xds/reference/steady_state_XH.md)
  :

  Steady States for **XH**

### Modules

XH component module implementations

- [`trivial_XH`](https://dd-harp.github.io/ramp.xds/reference/trivial_XH.md)
  :

  The `trivial` Module for the XH Component

- [`SIS`](https://dd-harp.github.io/ramp.xds/reference/SIS.md) :

  The `SIS` Module for the XH Component

- [`hMoI`](https://dd-harp.github.io/ramp.xds/reference/hMoI.md) :

  The `hMoI` Module for the XH Component

### Time Spent

Utilities for Time Spent Matrices

- [`time_spent`](https://dd-harp.github.io/ramp.xds/reference/time_spent.md)
  : Time Spent
- [`time_at_risk`](https://dd-harp.github.io/ramp.xds/reference/time_at_risk.md)
  : Time at Risk
- [`setup_TimeSpent()`](https://dd-harp.github.io/ramp.xds/reference/setup_TimeSpent.md)
  : Make a time spent matrix, called TimeSpent
- [`change_TimeSpent_matrix()`](https://dd-harp.github.io/ramp.xds/reference/change_TimeSpent_matrix.md)
  : Set up (or change) a Time Spent matrix
- [`make_TimeSpent_athome()`](https://dd-harp.github.io/ramp.xds/reference/make_TimeSpent_athome.md)
  : Make a mosquito dispersal matrix, called TimeSpent
- [`make_TimeSpent_xy()`](https://dd-harp.github.io/ramp.xds/reference/make_TimeSpent_xy.md)
  : Make a mosquito dispersal matrix, called TimeSpent

## Adult Mosquitoes (MY)

The MY Component

### Modules

- [`trivial_MY`](https://dd-harp.github.io/ramp.xds/reference/trivial_MY.md)
  :

  The `trivial` Module for the MY Component

- [`basicM`](https://dd-harp.github.io/ramp.xds/reference/basicM.md) :

  The `basicM` Module for the MY Component

- [`SI`](https://dd-harp.github.io/ramp.xds/reference/SI.md) :

  The `SI` Module for the MY Component

- [`macdonald`](https://dd-harp.github.io/ramp.xds/reference/macdonald.md)
  :

  The `macdonald` Module (MY Component)

- [`GeRM`](https://dd-harp.github.io/ramp.xds/reference/GeRM.md) :

  The `GeRM` Module for the MY Component

- [`RMdts`](https://dd-harp.github.io/ramp.xds/reference/RMdts.md) :

  The `RMdts` Module for the MY Component

## Aquatic Mosquitoes (L)

Generic methods for the aquatic (immature) mosquito component.

- [`skill_set_L()`](https://dd-harp.github.io/ramp.xds/reference/skill_set_L.md)
  :

  The skill set (**L** module)

- [`check_L()`](https://dd-harp.github.io/ramp.xds/reference/check_L.md)
  : Run a check before solving

- [`get_L_pars()`](https://dd-harp.github.io/ramp.xds/reference/get_L_pars.md)
  :

  Get parameters for the **L** Component module

- [`change_L_pars()`](https://dd-harp.github.io/ramp.xds/reference/change_L_pars.md)
  :

  Set **L** Component Parameters

- [`change_L_inits()`](https://dd-harp.github.io/ramp.xds/reference/change_L_inits.md)
  :

  Set **L** Component Initial Values

### Modules

Models for aquatic mosquito development and maturation in aquatic
habitats

- [`trivial_L`](https://dd-harp.github.io/ramp.xds/reference/trivial_L.md)
  :

  The `trivial` Module for the L Component

- [`basicL`](https://dd-harp.github.io/ramp.xds/reference/basicL.md) :

  The `basicL` Module for the L Component

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

- [`get_V_pars()`](https://dd-harp.github.io/ramp.xds/reference/get_V_pars.md)
  : Return the parameters as a list

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
  : Set the interpolating points

- [`change_spline_y()`](https://dd-harp.github.io/ramp.xds/reference/change_spline_y.md)
  : Set yy

- [`change_shock()`](https://dd-harp.github.io/ramp.xds/reference/change_shock.md)
  : Set the interpolating points

## Functions Library

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
