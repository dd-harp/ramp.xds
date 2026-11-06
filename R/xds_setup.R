# functions to set up models

#' @title Build a Model and Configure All Components
#'
#' @description
#' \loadmathjax
#' Make an **`xds`** *model object* from a set of input parameters.
#'
#' The first set of arguments chooses a module to use for each dynamical component (see [xds_info_dynamical_components]):
#' - `Xname` --- human / host infection dynamics (see [XH_module_list])
#' - `MYname` -- adult mosquito ecology and infection dynamics (see [MY_module_list])
#' - `Lname` -- aquatic mosquito ecology (see [L_module_list])
#'
#' The second set of arguments sets up the model structure
#' - `nPatches` - the number of patches (see [xds_info_patch_dynamics])
#' - `membership` - the habitat membership vector (see [xds_info_aquatic_habitats]).
#' - `residence` - the human residence vector (see [xds_info_human_populations])
#'
#' Note that: 
#' - The structural parameter `nHabitats` is set to `length(membership)` 
#' - The structural parameter `nStrata` is set to  `length(residence)`
#' 
#' Configure some of the basic elements:
#' - `XHoptions` sets parameter values and initial conditions for the **XH** module
#' - `MYoptions` sets parameter values and initial conditions for the **MY** module
#' - `Loptions` sets parameter values and initial conditions for the **L** module
#' - `HPop` is the human population density
#' - `searchQ` the habitat search weights (see [xds_info_search_weights_habitat])
#' - `searchB` the blood feeding search weights (see [xds_info_search_weights_blood])
#' - `Koptions` sets up a mosquito dispersal matrix (if `nPatches>1`, see [xds_info_mosquito_dispersal])
#' - `TSoptions` sets up a time spent matrix (if `nPatches>1`; see [xds_info_time_spent])
#'
#' @note Other options can be configured after basic setup (see [xds_info_setup_options]). If the **MY** Component is the `trivial` module, consider using [xds_setup_aquatic] or [xds_setup_human] or [xds_setup_eir]. Models for mosquito
#' ecology need not include states describing infection status (see [xds_setup_mosy]).
#' 
#' @details
#' 1. [make_xds_object_template] returns an object template with
#' a properly configured interface for blood feeding 
#' and egg laying (see [xds_object]). The structural parameters are: 
#'
#'    - `nPatches` is passed as a parameter
#'    - `nHabitats = length(membership)`
#'    - `nStrata = length(residence) = length(HPop)`
#'    - `nHostSpecies=1` (basic setup handles only the first host species)
#'    - `nVectorSpecies=1` (basic setup handles only the first vector species)
#'    - `class(xds_obj$frame) = 'full'`
#'
#' 2. Each one of the dynamical components is configured.
#'
#'    - **`xds_obj$XH_obj[[1]]`** defines a model for human / host infection dynamics of class `Xname` (the **X** component). The parameter values passed in a named list, `XHoptions.`
#'    - **`xds_obj$MY_obj[[1]]`** defines a model for adult mosquito ecology & infection dynamics of class `MYname` (the **MY** Component). The parameter values are passed in a named list, `MYoptions.`
#'    - **`xds_obj$L_obj[[1]]`** defines a model for aquatic mosquito ecology of class `Lname` (The **L** Component). The parameter values are passed in a named list, `Loptions.`
#'
#' @seealso [make_xds_object_template], [xds_info_basic_setup]
#'
#' @param model_name a name for the model 
#' @param xds is `xde` for differential equations or `dts` for discrete time systems
#' @param Xname the name of an **XH** module (see [XH_module_list])
#' @param XHoptions a named list to set parameter values and initial conditions for the **XH** module
#' @param MYname the name of an **MY** module (see [MY_module_list])
#' @param MYoptions a named list to set parameter values and initial conditions for the **MY** component
#' @param Lname the name of an **L** module (see [L_module_list])
#' @param Loptions a named list to set parameter values and initial conditions for the **L** component
#' @param nPatches the number of patches
#' @param HPop the human population density in each patch
#' @param residence the index of the patch where each human population stratum resides
#' @param searchB a vector of blood search weights or setup options (see [setup_blood_search_weights])
#' @param TSoptions a time spent matrix or setup options (see [setup_timespent])
#' @param membership the index of the patch where each aquatic habitat is found
#' @param searchQ a vector of habitat search weights or setup options (see [setup_habitat_search_weights])
#' @param Koptions a mosquito dispersal matrix (\eqn{K}) or setup options (see [setup_K_matrix])
#' 
#' @return an **`xds`** object
#' @export
xds_setup = function(
     model_name = "unnamed",
     xds = 'ode',
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
){
  xds_obj <- make_xds_object_template(xds, 'full', nPatches, membership, residence)

  # Aquatic Mosquito Dynamics
  xds_obj$Lname = Lname
  xds_obj <- setup_L_obj(Lname, membership, xds_obj, 1, Loptions)

 
  
  # Adult Mosquito Dynamics
  xds_obj$MYname = MYname
  xds_obj <- setup_MY_obj(MYname, xds_obj, 1, MYoptions)
  
  
  # Human Dynamics
  xds_obj$Xname = Xname
  XHoptions$H = HPop
  xds_obj <- setup_XH_obj(Xname, residence, xds_obj, 1, XHoptions)
  
  
  xds_obj <- make_indices(xds_obj)
  
  xds_obj <- setup_K_matrix(Koptions, xds_obj, list(), 1)
  xds_obj <- setup_habitat_search_weights(searchQ, xds_obj, searchQ, 1)
  xds_obj <- setup_blood_search_weights(searchB, xds_obj, searchB, 1)
  xds_obj <- setup_timespent(TSoptions, xds_obj, TSoptions, 1)
  
  xds_obj <- update_interfaces(xds_obj)

  xds_obj <- check_models(xds_obj)
  
  xds_obj$model_name <- model_name 

  return(xds_obj)
}

#' @title Build a Model of Mosquito Ecology
#'
#' @description
#' \loadmathjax
#' A modified version of [xds_setup] that streamlines setup for models without parasite / pathogen infection
#' dynamics. These models lack the **Y** component and an **X** component, but they will often need an **H**
#' component (host density).
#'
#' The **`xds`** object defines `frame = class(frame) = 'mosy'`
#' to dispatch functions that compute derivatives (`xde`), update variables (`dts`),
#' and parse outputs
#'
#' The **X** Component module is `trivial`, but since humans / vertebrate hosts can be a
#' resource, `HPop` must be set.
#' @seealso [xds_setup] and [dMYdt.basicM]
#' @param model_name is a name for the model 
#' @param xds is `xde` for differential equations or `dts` for discrete time systems
#' @param MYname the name of an **MY** module (see [MY_module_list])
#' @param Lname the name of an **L** module (see [L_module_list])
#' @param nPatches the number of patches
#' @param membership is a vector that describes the patch where each aquatic habitat is found
#' @param HPop is the human / host population density
#' @param searchQ is a vector of search weights for egg laying
#' @param kappa is a vector describing net infectiousness
#' @param MYoptions a list to configure the **MY** Component module
#' @param Koptions a mosquito dispersal matrix (\eqn{K}) or setup options (see [setup_K_matrix])
#' @param Loptions a list to configure the **L** Component module
#' @return an **`xds`** object
#' @export
xds_setup_mosy = function(
     model_name = "unnamed",
     xds = 'ode',
     ### Dynamical Components
     MYname = "basicM",
     Lname = "basicL",
     ### Model Structure
     nPatches = 1,
     membership=1,
     HPop = 1000,
     ### Setup Parameters
     searchQ = list(),
     kappa = 1,
     ### Options
     MYoptions = list(),
     Koptions = list(),
     Loptions = list()
){
  nPatches = max(membership, nPatches)
  residence = 1:nPatches
  xds_obj <- make_xds_object_template(xds, 'mosy', nPatches, membership, residence)
  
  # Aquatic Mosquito Dynamics
  xds_obj$Lname <- Lname
  xds_obj <- setup_L_obj(xds_obj$Lname, membership, xds_obj, 1, Loptions)
 

  # Adult Mosquito Dynamics
  xds_obj$MYname   <- MYname
  xds_obj <- setup_MY_obj(MYname, xds_obj, 1, MYoptions)

  # Human / Host Dynamics
  xds_obj$Xname   <- "trivial"
  XHoptions <- list(H=HPop, kappa=kappa)
  xds_obj <- setup_XH_obj(xds_obj$Xname, residence, xds_obj, 1, XHoptions) 
  
  xds_obj = make_indices(xds_obj)
  
  xds_obj <- setup_habitat_search_weights(searchQ, xds_obj, searchQ, 1)
  xds_obj <- setup_K_matrix(Koptions, xds_obj, list(), 1)

  xds_obj <- update_interfaces(xds_obj)

#  xds_obj$terms$kappa[[1]] = checkIt(kappa, nPatches)

  xds_obj$model_name <- model_name

  return(xds_obj)
}


#' @title Build a Model of Immature Mosquito Ecology
#'
#' @description
#' \loadmathjax
#'
#' A modified version of [xds_setup] that streamlines setup for an **L** Component
#' and egg laying for a `trivial` **MY** Component. 
#' 
#' The **`xds`** object defines `frame = class(frame) = 'aquatic'`
#' to dispatch functions that compute derivatives (`xde`), update variables (`dts`),
#' and parse outputs
#' 
#' @note The **XH** model 
#' is never used, but it is setup as the default `trivial` module.
#'
#'
#' @seealso [xds_setup]
#' @param model_name is a name for the model (arbitrary)
#' @param xds is `ode` or `dde` or `dts` for ordinary OR delay differential OR difference equations
#' @param nHabitats is the number of habitats
#' @param Lname the name of an **L** module (see [L_module_list])
#' @param Loptions a list to configure the **L** Component module
#' @param MYoptions a list to configure [F_eggs.trivial]
#' @return an **`xds`** object
#' @export
xds_setup_aquatic = function(model_name = "unnamed",
                             xds = 'ode',
                             nHabitats=1,
                             Lname = "basicL",
                             Loptions = list(),
                             MYoptions = list()){
  nPatches <- nHabitats
  membership <- 1:nPatches
  residence <- 1:nPatches
  xds_obj <- make_xds_object_template(xds, 'aquatic', nPatches, membership, residence)
  
  # Aquatic Mosquito Dynamics
  xds_obj$Lname <- Lname
  xds_obj <- setup_L_obj(xds_obj$Lname, membership, xds_obj, 1, Loptions)

  # Adult Mosquito Dynamics
  xds_obj$MYname   <- "trivial"
  xds_obj <- setup_MY_obj(xds_obj$MYname, xds_obj, 1, MYoptions)
  
  xds_obj$forced_by = c("MY", "eggs")
  class(xds_obj$forced_by) = c("MY", "eggs")
  
  # Human / Host Dynamics
  xds_obj$Xname   <- "trivial"
  xds_obj <- setup_XH_obj("trivial", residence, xds_obj, i=1, options=list(H=1)) 
  
  xds_obj = make_indices(xds_obj)
  y <- as.vector(unlist(get_inits(xds_obj)))
  xds_obj <- egg_laying_dynamics(0, y, xds_obj)

  xds_obj$model_name <- model_name
  return(xds_obj)
}


#' @title Build a Model of Human / Host Epidemiology
#'
#' @description
#' \loadmathjax
#'
#' A modified version of [xds_setup] that
#' streamlines setup for models with a trivial **MY** Component.
#'
#' The **`xds`** object defines `frame = class(frame) = 'human'`
#' to dispatch functions that compute derivatives (`xde`), update variables (`dts`),
#' and parse outputs
#'
#' The **MY** Component module is set to `trivial`. The function [F_fqZ.trivial] is called to compute passes the
#' density of infectious adult mosquitoes, and \eqn{f} and \eqn{q} can still be
#' configured. In this case, the daily EIR is computed using the blood feeding
#' interface, including [Exposure], in the same way as a model with a non-trivial
#' **MY** Component module.
#'
#' To study human cohort dynamics by passing a function that computes the daily EIR,
#' consider using [xds_setup_eir].
#'
#' @seealso [xds_setup] and [xds_setup_eir]
#'
#' @param model_name a name for the model
#' @param xds is `ode` or `dde` or `dts` for ordinary OR delay differential OR difference equations
#' @param Xname the name of an **XH** module (see [XH_module_list])
#' @param XHoptions a named list to set parameter values and initial conditions for the **XH** module
#' @param nPatches the number of patches
#' @param residence a vector that describes the patch where each human stratum lives
#' @param HPop the number of humans in each patch
#' @param searchB  a vector of search weights for blood feeding
#' @param TSoptions either a TimeSpent matrix or a string to call a function that sets it up
#' @param MYoptions list to configure the **MY** Component module
#' @param BFopts list to configure the blood feeding model
#'
#' @return an **`xds`** object
#' @export
xds_setup_human = function(model_name = "unnamed",
                           Xname = "SIS",
                           XHoptions = list(),
                           xds = 'ode',
                           ### Dynamical Components
                           ### Model Structure
                           nPatches=1,
                           residence=1,
                           HPop=1000,
                           ### Setup Parameters
                           searchB = list(),
                           TSoptions = list(),
                           ### Options
                           MYoptions = list(),
                           BFopts = list()
){
  nStrata=length(residence)
  HPop = checkIt(HPop, nStrata)
  membership=1
  xds_obj <- make_xds_object_template(xds, 'human', nPatches, membership, residence)

  # Aquatic Mosquito Dynamics
  xds_obj$Lname <- "trivial" 
  xds_obj <- setup_L_obj(xds_obj$Lname, 1, xds_obj, membership, list())

  # Adult Mosquito Dynamics
  xds_obj$MYname   <- "trivial" 
  xds_obj <- setup_MY_obj(xds_obj$MYname, xds_obj, 1, MYoptions)

  # Human Dynamics
  xds_obj$Xname <- Xname
  XHoptions$H = HPop
  xds_obj <- setup_XH_obj(xds_obj$Xname, residence, xds_obj, 1, XHoptions) 

  
  xds_obj$forced_by = c(xds_obj$forced_by, "fqZ")
  class(xds_obj$forced_by) = c("MY", "fqZ")

  xds_obj = make_indices(xds_obj)

  xds_obj <- setup_timespent(TSoptions, xds_obj, TSoptions, 1)
  xds_obj <- setup_blood_search_weights(searchB, xds_obj, searchB, 1)
  
  xds_obj <- update_interfaces(xds_obj)

  xds_obj$model_name <- model_name

  return(xds_obj)
}


#' @title Build a Model for a single Human / Host Epidemiology forced by the EIR
#'
#' @description
#' \loadmathjax
#'
#' A modified version of [xds_setup] to setup up
#' studies of malaria epidemiology, defined in a narrow sense, to
#' examine patterns in populations forced by the EIR.
#'
#' The **`xds`** object defines `frame = class(frame) = 'eir'`
#' to dispatch functions that compute derivatives (`xde`), update variables (`dts`),
#' and parse outputs
#'
#'
#' The interface includes options to configure a function
#' describing `F_eir` as a function of time, with seasonal components
#' and a trend.
#'
#' This can be used to model a cohort as it ages;
#' a function is set up to modify exposure by age.
#'
#' @seealso [xds_setup] and [xds_setup_human]
#'
#' @param model_name is a name for the model (arbitrary)
#' @param eir is the entomological inoculation rate
#' @param F_season the seasonal pattern function
#' @param F_trend the trend function
#' @param F_shock the shock function
#' @param F_age the relative biting rate by age 
#' @param season_par a list of options for F_season
#' @param trend_par a list of options for F_trend
#' @param shock_par a list of options for F_shock
#' @param age_par a list of options for F_age
#' @param xds is `ode` or `dde` or `dts` for ordinary OR delay differential OR difference equations
#' @param Xname the name of an **XH** module (see [XH_module_list])
#' @param XHoptions a named list to set parameter values and initial conditions for the **XH** module
#' @param HPop is the number of humans in each stratum
#' @param searchB is a vector of search weights for blood feeding
#' 
#' @return an **`xds`** object
#' @export
xds_setup_eir = function(model_name = "unnamed",
                         eir=1/365,
                         F_season = F_one, 
                         F_trend = F_one, 
                         F_shock = F_one,
                         F_age = F_one,

                         season_par = list(name = "F_one"),
                         trend_par = list(name = "F_one"),
                         shock_par = list(name = "F_one"),
                         age_par = list(name = "F_one"),
                         
                         xds = 'ode',

                         # Dynamical Components
                         Xname = "SIS",
                         XHoptions = list(),

                         # Model Structure
                         HPop=1000,
                         searchB = 1
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
  xds_obj <- setup_L_obj(xds_obj$Lname, 1, xds_obj, membership, list())
  
  # Adult Mosquito Dynamics
  xds_obj$MYname   <- "trivial" 
  xds_obj <- xds_obj <- setup_MY_obj(xds_obj$MYname, xds_obj, 1, list())
  
  # Human Dynamics
  xds_obj$Xname <- Xname
  XHoptions$H = HPop
  xds_obj <- setup_XH_obj(xds_obj$Xname, residence, xds_obj, 1, XHoptions) 
  
  xds_obj$forced_by = xds_obj$frame

  xds_obj = make_indices(xds_obj)

  xds_obj$model_name = model_name

  return(xds_obj)
}

#' Update interfaces
#'
#' @param xds_obj an **`xds`** model object
#'
#' @return an **`xds`** object
#' 
#' @export
update_interfaces = function(xds_obj){
  y <- as.vector(unlist(get_inits(xds_obj)))
  xds_obj <- importation_dynamics(0, y, xds_obj)
  xds_obj <- egg_laying_dynamics(0, y, xds_obj)
  xds_obj <- blood_feeding_dynamics(0, y, xds_obj)
  xds_obj <- transmission_dynamics(0, y, xds_obj)
  return(xds_obj)
}


