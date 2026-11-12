# functions to set up models

#' @title Basic setup
#' 
#' @description
#' 
#' \loadmathjax
#' 
#' Build a dynamical system to describe the epidemiology and 
#' transmission dynamics of a mosquito-borne pathogen. The function
#' encodes the model as an **xds** object that 
#' can be solved by [xds_solve]. 
#' 
#' @seealso 
#' + Basic Setup: [xds_info_basic_setup] 
#'    + [xds_info_change_parameters]
#'    + [xds_info_initial_conditions]
#' + Advanced Setup: [xds_info_advanced_setup]
#' + Solving: [xds_solve]
#' + Internals: [xds_info_dynamical_components],
#'  [xds_object], [make_xds_object_template]
#' 
#' @details
#' 
#' Basic setup has three parts: dynamics, model structure, and basic spatial elements. 
#' 
#' **Dynamics**
#'
#' To set up a dynamical system, a module is selected for each dynamical component by name:
#' - `XHname` --- the name of an **XH** module: see the [XH_module_list] for human / host infection dynamics
#' - `MYname` -- the name of an **MY** module: see the [MY_module_list] for adult mosquito ecology and infection dynamics
#' - `Lname` -- the name of an **L** module: see the [L_module_list] for aquatic mosquito ecology
#'
#' To configure the modules, set parameter values and initial conditions using setup options (named lists): 
#' - `XHoptions` sets parameter values and initial conditions for the **XH** module
#' - `MYoptions` sets parameter values and initial conditions for the **MY** module
#' - `Loptions` sets parameter values and initial conditions for the **L** module
#' - `HPop` is the human population density
#'
#' For more information about how to change parameters or 
#' initial conditions, see [xds_info_change_parameters] or [xds_info_initial_conditions].
#' 
#' **Model Structure**
#' 
#' Set up the model structure: 
#' - `nPatches` - the number of patches (see [xds_info_patch_dynamics])
#' - `residence` - the human residence vector (see [xds_info_human_populations])
#' - `membership` - the habitat membership vector (see [xds_info_aquatic_habitats])
#' 
#' During setup: 
#' - The structural parameter `nStrata` is set to `length(residence)`
#' - The structural parameter `nHabitats` is set to `length(membership)` 
#' - `nPatches` is checked and set to `max(membership, nPatches)`
#' - `nVectorSpecies`\eqn{=1}
#' - `nHostSpecies`\eqn{=1}
#' 
#' **Spatial Dynamics** 
#' 
#' If `nPatches`\eqn{>1}, set up some basic aspects of egg laying, blood feeding, mosquito spatial dynamics, and exposure:
#' - `searchQ` the habitat search weights (see [xds_info_search_weights_habitat])
#' - `searchB` the blood feeding search weights (see [xds_info_search_weights_blood])
#' - `Koptions` sets up a mosquito dispersal matrix (see [xds_info_mosquito_dispersal])
#' - `TSoptions` sets up a time spent matrix (see [xds_info_time_spent])
#'
#' **Basic Setup Variants**
#' 
#' Other versions of [xds_setup] are available (see [xds_info_basic_setup]):
#' + Use [xds_setup_aquatic] for studies of mosquito aquatic ecology
#' + Use [xds_setup_mosy] for studies of mosquito ecology without infection dynamics
#' + Use [xds_setup_human] for studies of human/host disease dynamics forced by infective biting in patches
#' + Use [xds_setup_eir] for studies of human/host disease dynamics forced by the local daily EIR
#'
#' **Next**
#' + To solve the model, see [xds_solve]
#' + To check or change parameters, see [xds_info_change_parameters]
#' + To check or change the initial conditions, see [xds_info_initial_conditions]
#' + To modify the model structure, see [xds_info_advanced_setup]
#' 
#' @note 
#' 
#' [xds_setup] calls [make_xds_object_template], which returns an object template with
#' a properly configured interface for blood feeding 
#' and egg laying (see [xds_object]).
#' 
#'
#' @param XHname the name of an **XH** module (see [XH_module_list])
#' @param MYname the name of an **MY** module (see [MY_module_list])
#' @param Lname the name of an **L** module (see [L_module_list])
#' @param XHoptions a named list to set parameter values and initial conditions for the **XH** module
#' @param MYoptions a named list to set parameter values and initial conditions for the **MY** component
#' @param Loptions a named list to set parameter values and initial conditions for the **L** component
#' @param nPatches the number of patches
#' @param residence the index of the patch where each human population stratum resides
#' @param membership the index of the patch where each aquatic habitat is found
#' @param HPop the initial human population density for each stratum
#' @param searchB a vector of blood search weights or setup options (see [setup_blood_search_weights])
#' @param TSoptions a time spent matrix or setup options (see [setup_timespent])
#' @param searchQ a vector of habitat search weights or setup options (see [setup_habitat_search_weights])
#' @param Koptions a mosquito dispersal matrix (\eqn{K}) or setup options (see [setup_K_matrix])
#' @param model_name a name for the model 
#' @param xds is `xde` for differential equations or `dts` for discrete time systems
#' 
#' @return an **`xds`** object
#' @export
xds_setup = function(
     XHname = "SIS",
     MYname = "SI",
     Lname = "trivial",
     XHoptions = list(),
     MYoptions = list(),
     Loptions = list(),
     nPatches = 1,
     residence = 1,
     membership = 1,
     HPop = 1000,
     searchB = list(),
     TSoptions = list(),
     searchQ = list(),
     Koptions = list(),
     model_name = "unnamed",
     xds = 'ode'
){
  nPatches = max(membership, nPatches)
  xds_obj <- make_xds_object_template(xds, 'full', nPatches, membership, residence)

  # Aquatic Mosquito Dynamics
  xds_obj$Lname = Lname
  xds_obj <- setup_L_obj(Lname, membership, xds_obj, options=Loptions, s=1)

  # Adult Mosquito Dynamics
  xds_obj$MYname = MYname
  xds_obj <- setup_MY_obj(MYname, xds_obj, options=MYoptions, s=1)
  
  
  # Human Dynamics
  xds_obj$XHname = XHname
  XHoptions$H = HPop
  xds_obj <- setup_XH_obj(XHname, residence, xds_obj, options=XHoptions, i=1)
  
  
  xds_obj <- make_indices(xds_obj)
  
  # Human Dynamics
  xds_obj <- setup_K_matrix(Koptions, xds_obj, list(), 1)
  xds_obj <- setup_habitat_search_weights(searchQ, xds_obj, searchQ, 1)
  xds_obj <- setup_blood_search_weights(searchB, xds_obj, searchB, 1)
  xds_obj <- setup_timespent(TSoptions, xds_obj, TSoptions, 1)
  
  xds_obj <- update_interfaces(xds_obj)

  xds_obj <- check_models(xds_obj)
  
  xds_obj$model_name <- model_name 

  return(xds_obj)
}

#' @title Basic setup for mosquito ecology
#'
#' @description
#' \loadmathjax
#' 
#' Build a dynamical system to describe mosquito ecology. The function
#' encodes the model as an **xds** object that 
#' can be solved by [xds_solve]. 
#' 
#' @details 
#' 
#' This is a modified version of [xds_setup] that streamlines setup 
#' for models without parasite / pathogen infection
#' dynamics. It sets 
#' 
#' `class(xds_obj$frame) = 'mosy'`
#' 
#' to dispatch functions that compute derivatives (`xde`), update variables (`dts`),
#' and parse outputs. The solver ignores infection dynamics (the **X** and **Y** processes). 
#' A trivial **XH** component is set up to handle human/host population density.
#' 
#' **Dynamics** 
#' 
#' Choose a module to use for the mosquito dynamical components (see [xds_info_dynamical_components]):
#' - `MYname` -- the name of an **MY** module: see the [MY_module_list] for adult mosquito ecology and infection dynamics
#' - `Lname` -- the name of an **L** module: see the [L_module_list] for aquatic mosquito ecology
#'
#' Set parameter values and initial conditions using setup options (named lists): 
#' - `MYoptions` sets parameter values and initial conditions for the **MY** module
#' - `Loptions` sets parameter values and initial conditions for the **L** module
#' - `HPop` sets the human population density
#' 
#' **Model Structure** 
#'  
#' Set up the model structure: 
#' - `nPatches` - the number of patches (see [xds_info_patch_dynamics])
#' - `membership` - the habitat membership vector (see [xds_info_aquatic_habitats])
#'
#' During setup: 
#' - The structural parameter `nHabitats` is set to `length(membership)` 
#' - The structural parameter `nStrata` is set to `nPatches`
#' - `nPatches` is checked and set to `max(membership, nPatches)`
#' - `nVectorSpecies`\eqn{=1}
#' 
#' **Spatial Dynamics** 
#'
#' If `nPatches`\eqn{>1}, set up some basic aspects of egg laying, blood feeding, mosquito spatial dynamics, and exposure:
#' - `searchQ` the habitat search weights (see [xds_info_search_weights_habitat])
#' - `Koptions` sets up a mosquito dispersal matrix (see [xds_info_mosquito_dispersal])
#' 
#' **Next**
#' 
#' + To solve the model, see [xds_solve]
#' + To check or change parameters, see [xds_info_change_parameters]
#' + To check or change the initial conditions, see [xds_info_initial_conditions]
#' + To modify the model structure, see [xds_info_advanced_setup]
#' 
#' @note 
#' 
#' [xds_setup] calls [make_xds_object_template], which returns an object template with
#' a properly configured interface for blood feeding 
#' and egg laying (see [xds_object]).
#' 
#' @seealso 
#' + Basic Setup: [xds_setup], [xds_info_basic_setup] 
#'    + [xds_info_change_parameters]
#'    + [xds_info_initial_conditions]
#' + Advanced Setup: [xds_info_advanced_setup]
#' + Solving: [xds_solve]
#' + Internals: [xds_info_dynamical_components],
#'  [xds_object], [make_xds_object_template]
#' 
#' @param MYname the name of an **MY** module (see [MY_module_list])
#' @param Lname the name of an **L** module (see [L_module_list])
#' @param nPatches the number of patches
#' @param membership is a vector that describes the patch where each aquatic habitat is found
#' @param HPop the initial human population density for each stratum
#' @param searchQ is a vector of search weights for egg laying
#' @param MYoptions a list to configure the **MY** Component module
#' @param Koptions a mosquito dispersal matrix (\eqn{K}) or setup options (see [setup_K_matrix])
#' @param Loptions a list to configure the **L** Component module
#' @param model_name is a name for the model 
#' @param xds is `xde` for differential equations or `dts` for discrete time systems
#' 
#' @return an **`xds`** object
#' 
#' @export
xds_setup_mosy = function(
     ### Dynamical Components
     MYname = "basicM",
     Lname = "basicL",
     ### Model Structure
     nPatches = 1,
     membership=1,
     HPop = 1000,
     ### Setup Parameters
     searchQ = list(),
     ### Options
     MYoptions = list(),
     Koptions = list(),
     Loptions = list(), 
     model_name = "unnamed",
     xds = 'ode'
){
  nPatches = max(membership, nPatches)
  residence = 1:nPatches
  xds_obj <- make_xds_object_template(xds, 'mosy', nPatches, membership, residence)
  
  # Aquatic Mosquito Dynamics
  xds_obj$Lname <- Lname
  xds_obj <- setup_L_obj(xds_obj$Lname, membership, xds_obj, options=Loptions, s=1)
 

  # Adult Mosquito Dynamics
  xds_obj$MYname   <- MYname
  xds_obj <- setup_MY_obj(MYname, xds_obj, options=MYoptions, s=1)

  # Human / Host Dynamics
  xds_obj$XHname   <- "trivial"
  XHoptions <- list(H=HPop, kappa=0)
  xds_obj <- setup_XH_obj(xds_obj$XHname, residence, xds_obj, options=XHoptions, i=1) 
  
  xds_obj = make_indices(xds_obj)
  
  xds_obj <- setup_habitat_search_weights(searchQ, xds_obj, searchQ, 1)
  xds_obj <- setup_K_matrix(Koptions, xds_obj, list(), 1)

  xds_obj <- update_interfaces(xds_obj)

#  xds_obj$terms$kappa[[1]] = checkIt(kappa, nPatches)

  xds_obj$model_name <- model_name

  return(xds_obj)
}


#' @title Basic setup for aquatic mosquito ecology
#'
#' @description
#' \loadmathjax
#' 
#' Build a dynamical system to describe the dynamics of immature mosquitoes
#' in aquatic habitats. The function
#' encodes the model as an **xds** object that 
#' can be solved by [xds_solve]. 
#' 
#' @details 
#' 
#' This is a modified version of [xds_setup] that streamlines setup for the **L** module
#' and egg laying for a `trivial` **MY** module. It sets
#' 
#' `class(xds_obj$frame) = 'aquatic'`
#' 
#' to dispatch functions that compute derivatives (`xde`), update variables (`dts`),
#' and parse outputs.
#' 
#' **Dynamics** 
#' 
#' Choose a module:
#' - `Lname` -- the name of an **L** module: see the [L_module_list] for aquatic mosquito ecology
#'
#' Set parameter values and initial conditions using setup options (named lists): 
#' - `MYoptions` sets up egg laying using the `trivial` **MY** module
#' - `Loptions` sets parameter values and initial conditions for the **L** module
#' 
#' **Model Structure** 
#' 
#' Set up the model structure: 
#' - `nHabitats` -- sets the number of habitats 
#' - `nPatches` \eqn{=} `nHabitats` 
#' - `nVectorSpecies`\eqn{=1}
#' 
#' **Next**
#' + To solve the model, see [xds_solve]
#' + To check or change parameters, see [xds_info_change_parameters]
#' + To check or change the initial conditions, see [xds_info_initial_conditions]
#' + To modify the model structure, see [xds_info_advanced_setup]
#' 
#' @seealso 
#' + Basic Setup: [xds_setup], [xds_info_basic_setup] 
#'    + [xds_info_change_parameters]
#'    + [xds_info_initial_conditions]
#' + Advanced Setup: [xds_info_advanced_setup]
#' + Solving: [xds_solve]
#' + Internals: [xds_info_dynamical_components],
#'  [xds_object], [make_xds_object_template]
#' 
#' @note 
#' [xds_setup] calls [make_xds_object_template], which returns an object template with
#' a properly configured interface for blood feeding 
#' and egg laying (see [xds_object]). 
#' + The **XH** model is never used, but it is set up as the default `trivial` module.
#'
#' @param Lname the name of an **L** module (see [L_module_list])
#' @param Loptions a list to configure the **L** Component module
#' @param nHabitats is the number of habitats
#' @param MYoptions options to configure egg laying
#' @param model_name is a name for the model (arbitrary)
#' @param xds is `ode` or `dde` or `dts` for ordinary OR delay differential OR difference equations
#' 
#' @return an **`xds`** object
#' @export
xds_setup_aquatic = function(Lname = "basicL",
                             Loptions = list(),
                             nHabitats=1,
                             MYoptions = list(), 
                             model_name = "unnamed",
                             xds = 'ode'){
  nPatches <- nHabitats
  membership <- 1:nPatches
  residence <- 1:nPatches
  xds_obj <- make_xds_object_template(xds, 'aquatic', nPatches, membership, residence)
  
  # Aquatic Mosquito Dynamics
  xds_obj$Lname <- Lname
  xds_obj <- setup_L_obj(xds_obj$Lname, membership, xds_obj, options=Loptions, s=1)

  # Adult Mosquito Dynamics
  xds_obj$MYname   <- "trivial"
  xds_obj <- setup_MY_obj(xds_obj$MYname, xds_obj, options=MYoptions, s=1)
  
  xds_obj$forced_by = c("MY", "eggs")
  class(xds_obj$forced_by) = c("MY", "eggs")
  
  # Human / Host Dynamics
  xds_obj$XHname   <- "trivial"
  xds_obj <- setup_XH_obj("trivial", residence, xds_obj, options=list(H=1), i=1) 
  
  xds_obj = make_indices(xds_obj)
  y <- as.vector(unlist(get_inits(xds_obj)))
  xds_obj <- egg_laying_dynamics(0, y, xds_obj)

  xds_obj$model_name <- model_name
  return(xds_obj)
}

#' @title Basic setup for host disease dynamics
#'
#' @description
#' \loadmathjax
#' 
#' Build a dynamical system to describe the disease dynamics in human or host population 
#' in response to infective biting.
#' The function encodes the model as an **xds** object that 
#' can be solved by [xds_solve]. 
#'
#' To build a model forced by the EIR, see [xds_setup_eir]. 
#' 
#' @details
#' 
#' A modified version of [xds_setup] that sets up a model to study exposure and 
#' infection. 
#' The **MY** component module is set to `trivial` and a trace
#' function computes infective biting in the patches.
#' 
#' `class(xds_obj$frame) = 'human'`
#' 
#' to dispatch functions that compute derivatives (`xde`), update variables (`dts`),
#' and parse outputs. 
#'
#' To set up a model that is forced
#' by the EIR, use [xds_setup_eir].
#' 
#' **Dynamics** 
#' 
#' Choose a module:
#' - `XHname` --- the name of an **XH** module: see the [XH_module_list] for human / host infection dynamics
#' - `MYname` \eqn{=} `trivial`
#'
#' Set parameter values and initial conditions using setup options (named lists): 
#' - `XHoptions` sets parameter values and initial conditions for the **XH** module
#' - `MYoptions` sets up infective biting in the patches for the trivial **MY** module
#' - `HPop` is the human population density
#' 
#' **Model Structure** 
#' 
#' Set up the model structure: 
#' - `nPatches` - the number of patches (see [xds_info_patch_dynamics])
#' - `residence` - the human residence vector (see [xds_info_human_populations])
#'
#' During setup: 
#' - The structural parameter `nStrata` is set to  `length(residence)`
#' - `nHostSpecies`\eqn{=1}
#'
#' If `nPatches`\eqn{>1}, set up some basic aspects of blood feeding and exposure:
#' - `searchB` the blood feeding search weights (see [xds_info_search_weights_blood])
#' - `TSoptions` sets up a time spent matrix (see [xds_info_time_spent])
#' 
#' **Next**
#' + To solve the model, see [xds_solve] or [xds_solve_cohort]
#' + To check or change parameters, see [xds_info_change_parameters]
#' + To check or change the initial conditions, see [xds_info_initial_conditions]
#' + To modify the model structure, see [xds_info_advanced_setup]
#'
#' @seealso 
#' + Basic Setup: [xds_setup], [xds_setup_eir], [xds_info_basic_setup] 
#'    + [xds_info_change_parameters]
#'    + [xds_info_initial_conditions]
#' + Advanced Setup: [xds_info_advanced_setup]
#' + Solving: [xds_solve]
#' + Internals: [xds_info_dynamical_components],
#'  [xds_object], [make_xds_object_template]
#'
#' @param XHname the name of an **XH** module (see [XH_module_list])
#' @param XHoptions a named list to set parameter values and initial conditions for the **XH** module
#' @param HPop the initial human population density for each stratum
#' @param nPatches the number of patches
#' @param residence a vector that describes the patch where each human stratum lives
#' @param searchB  a vector of search weights for blood feeding
#' @param TSoptions a time spent matrix or setup options (see [setup_timespent])
#' @param MYoptions a list to configure the trivial **MY** module
#' @param model_name a name for the model
#' @param xds is `ode` or `dde` or `dts` for ordinary OR delay differential OR difference equations
#'
#' @return an **`xds`** object
#' @export
xds_setup_human = function(XHname = "SIS",
                           XHoptions = list(),
                           HPop=1000,
                           ### Model Structure
                           nPatches=1,
                           residence=1,
                           ### Setup Parameters
                           searchB = list(),
                           TSoptions = list(),
                           ### Options
                           MYoptions = list(),
                           model_name = "unnamed",
                           xds = 'ode'
){
  nStrata=length(residence)
  HPop = checkIt(HPop, nStrata)
  membership=1
  xds_obj <- make_xds_object_template(xds, 'human', nPatches, membership, residence)

  # Aquatic Mosquito Dynamics
  xds_obj$Lname <- "trivial" 
  xds_obj <- setup_L_obj(xds_obj$Lname, 1, xds_obj, options=list(), s=membership)

  # Adult Mosquito Dynamics
  xds_obj$MYname   <- "trivial" 
  xds_obj <- setup_MY_obj(xds_obj$MYname, xds_obj, options=MYoptions, s=1)

  # Human Dynamics
  xds_obj$XHname <- XHname
  XHoptions$H = HPop
  xds_obj <- setup_XH_obj(xds_obj$XHname, residence, xds_obj, options=XHoptions, i=1) 
  
  xds_obj$forced_by = c(xds_obj$forced_by, "fqZ")
  class(xds_obj$forced_by) = c("MY", "fqZ")

  xds_obj = make_indices(xds_obj)

  xds_obj <- setup_timespent(TSoptions, xds_obj, TSoptions, 1)
  xds_obj <- setup_blood_search_weights(searchB, xds_obj, searchB, 1)
  
  xds_obj <- update_interfaces(xds_obj)

  xds_obj$model_name <- model_name

  return(xds_obj)
}

#' @title Basic setup for host disease dynamics (EIR forced)
#' 
#' @description
#' 
#' Build a dynamical system to describe human malaria epidemiology or host disease dynamics forced by the 
#' daily EIR. The function
#' encodes the model as an **xds** object that 
#' can be solved by [xds_solve] or [xds_solve_cohort].
#' 
#' \loadmathjax
#' 
#' To build a model that is forced by infective biting in the patches, which requires setting up a time
#' spent matrix and search weigths, use [xds_setup_human]. 
#' 
#' @details
#' 
#' A modified version of [xds_setup] that sets up a model to study exposure and 
#' infection. 
#' The **MY** component module is set to `trivial` and a trace
#' function computes infective biting in the patches.
#' 
#' `class(xds_obj$frame) = 'eir'`
#' 
#' to dispatch functions that compute derivatives (`xde`), update variables (`dts`),
#' and parse outputs. 
#'
#' **Dynamics** 
#' 
#' Configure the **XH** component:
#' - `XHname` --- the name of an **XH** module: see the [XH_module_list] for human / host infection dynamics
#' - `HPop` --- the initial human population density 
#' - `XHoptions` sets parameter values and initial conditions for the **XH** module
#' - `nHostSpecies`\eqn{=1}
#'
#' The structural parameter `nStrata` is set to `length(HPop)`.
#' 
#' **The EIR Trace Function**
#'  
#' The interface includes options to configure a composed, multiplicative time series function
#' describing `F_eir` as a function of time. For cohort dynamics (using [xds_solve_cohort]), a function 
#' can be configured to compute relative biting rates by age: 
#'  - `eir` --- the average daily EIR (for each population stratum)
#'  - `F_season` or `season_par` --- set up the seasonal pattern function
#'  - `F_trend` or `trend_par` --- set up the trend pattern function
#'  - `F_shock` or `shock_par` --- set up the shock function
#'  - `F_age` or `age_par` --- set up relative biting rates by age
#'
#' **Next**
#' + To solve the model, see [xds_solve] or [xds_solve_cohort]
#' + To check or change parameters, see [xds_info_change_parameters]
#' + To check or change the initial conditions, see [xds_info_initial_conditions]
#' + To modify the model structure, see [xds_info_advanced_setup]
#' 
#' @seealso
#' + Basic Setup: [xds_setup], [xds_setup_human], [xds_info_basic_setup] 
#'    + [xds_info_change_parameters]
#'    + [xds_info_initial_conditions]
#' + Advanced Setup: [xds_info_advanced_setup]
#' + Solving: [xds_solve], [xds_solve_cohort]
#' + Internals: [xds_info_dynamical_components],
#'  [xds_object], [make_xds_object_template]
#'
#' @param eir is the average, daily entomological inoculation rate
#' @param F_season the seasonal pattern function
#' @param F_trend the trend function
#' @param F_shock the shock function
#' @param F_age the relative biting rate by age 
#' @param season_par a list of options for F_season
#' @param trend_par a list of options for F_trend
#' @param shock_par a list of options for F_shock
#' @param age_par a list of options for F_age
#' @param xds is `ode` or `dde` or `dts` for ordinary OR delay differential OR difference equations
#' @param XHname the name of an **XH** module (see [XH_module_list])
#' @param XHoptions a named list to set parameter values and initial conditions for the **XH** module
#' @param HPop the initial human population density for each stratum
#' @param model_name is a name for the model (arbitrary)
#' 
#' @return an **`xds`** object
#' @export
xds_setup_eir = function(eir=1/365,
                         F_season = F_one, 
                         F_trend = F_one, 
                         F_shock = F_one,
                         F_age = F_one,

                         season_par = list(),
                         trend_par = list(),
                         shock_par = list(),
                         age_par = list(),
                         
                         xds = 'ode',

                         # Dynamical Components
                         XHname = "SIS",
                         XHoptions = list(),

                         # Model Structure
                         HPop=1000,
                         model_name = "unnamed"
){
  residence  = rep(1, length(HPop))
  membership = 1 
  xds_obj <- make_xds_object_template(xds, 'eir', 1, membership, residence)
  xds_obj$forced_by = xds_obj$frame

  xds_obj$EIR_obj <- list()
  xds_obj$EIR_obj$eir <- eir
  xds_obj$EIR_obj$scale <- 1
  xds_obj$EIR_obj$F_season <- F_season
  xds_obj$EIR_obj$F_trend <- F_trend
  xds_obj$EIR_obj$F_shock <- F_shock
  xds_obj$EIR_obj$F_age <- F_age
  
  xds_obj$EIR_obj$season_par <- season_par
  xds_obj$EIR_obj$trend_par <- trend_par
  xds_obj$EIR_obj$shock_par <- shock_par
  xds_obj$EIR_obj$age_par <- age_par
  
  xds_obj$EIR_obj$bday <- 0

  # Aquatic Mosquito Dynamics
  xds_obj$Lname <- "trivial" 
  xds_obj <- setup_L_obj(xds_obj$Lname, 1, xds_obj, options=list(), s=membership)
  
  # Adult Mosquito Dynamics
  xds_obj$MYname   <- "trivial" 
  xds_obj <- xds_obj <- setup_MY_obj(xds_obj$MYname, xds_obj, options=list(), s=1)
  
  # Human Dynamics
  xds_obj$XHname <- XHname
  XHoptions$H = HPop
  xds_obj <- setup_XH_obj(xds_obj$XHname, residence, xds_obj, options=XHoptions, i=1) 
  
  xds_obj$forced_by = xds_obj$frame

  xds_obj = make_indices(xds_obj)

  xds_obj$model_name = model_name

  return(xds_obj)
}

#' Update interfaces
#' 
#' @description
#' Update the values of all terms computed internally 
#' 
#' @param xds_obj an **`xds`** object
#'
#' @return an **`xds`** object
#' @keywords internal 
#' @export
update_interfaces = function(xds_obj){
  y <- as.vector(unlist(get_inits(xds_obj)))
  xds_obj <- importation_dynamics(0, y, xds_obj)
  xds_obj <- egg_laying_dynamics(0, y, xds_obj)
  xds_obj <- blood_feeding_dynamics(0, y, xds_obj)
  xds_obj <- transmission_dynamics(0, y, xds_obj)
  return(xds_obj)
}
