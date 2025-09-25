# functions to set up models

#' @title Build a Model and Configure All Components 
#'
#' @description 
#' \loadmathjax
#' Make an **`xds`** *model object*:
#' - Define the dynamical components:
#'    - **XH** - Component modules for human / host infection dynamics of class **`Xname`** with trivial demographics
#'    - **MY** - Component modules for adult mosquito ecology and infection dynamics of class **`MYname`**
#'    - **L** Component modules for aquatic mosquito ecology of class **`Lname`**
#' - Define basic structural parameters for a single host and vector population:
#'    - \eqn{n_p} or `nPatches` - the number of patches
#'    - \eqn{n_q} or `nHabitats = length(membership)` - the numer and locations of aquatic habitats
#'    - \eqn{n_h} or `nStrata = length(residence)` - the number of human / host population strata and basic demographic information
#' - Configure some of the basic elements
#'    - Search weights for human population strata
#'    - Search weights for aquatic habitats
#'    - The mosquito dispersal matrix, \eqn{K}
#'    - The time spent matrix \eqn{\Theta}
#'
#' Advanced options can be configured after basic setup.
#'
#' @note If the **MY** Component is the `trivial` module, consider using [xds_setup_aquatic] or [xds_setup_human] or [xds_setup_eir]. Models for mosquito
#' ecology need not include states describing infection status (see [xds_setup_mosy]).
#' @details
#' 1. Using the basic structural parameters, a basic template is created by [make_xds_object_template] with
#' a properly configured interface for blood feeding and egg laying, and `xds_obj$frame = class(xds_obj$frame) = 'full'` .
#'
#'    - `nPatches` is passed as a parameter
#'    - `nHabitats` is configured by passing the habitat `membership` vector, and `nHabitats = length(membership)`
#'    - `nStrata` is configured by passing a vector of human population densities and a residence vector, and `nStrata = length(residence) = length(HPop)`
#'    - `nHostSpecies=1` (basic setup handles only the first host species)
#'    - `nVectorSpecies=1` (basic setup handles only the first vector species)
#'
#'.
#' 2. Each one of the dynamical components is configured.
#'
#'    - **`xds_obj$XH_obj[[1]]`** defines a model for human / host infection dynamics of class `Xname` (the **X** component). The parameter values passed in a named list, `XHoptions.`
#'    - **`xds_obj$MY_obj[[1]]`** defines a model for adult mosquito ecology & infection dynamics of class `MYname` (the **MY** Component). The parameter values are passed in a named list, `MYoptions.`
#'    - **`xds_obj$L_obj[[1]]`** defines a model for aquatic mosquito ecology of class `Lname` (The **L** Component). The parameter values are passed in a named list, `Loptions.`
#'
#' 3. After configuring the dynamical components, several structural parameters can be configured at the command line:
#'
#'    - Habitat search weights can be set
#'    - Host population search weights can be set
#'    - A mosquito dispersal matrix, \eqn{\cal K}, can be set
#'    - A time spent matrix, \eqn{\Theta}, can be set
#'
#' Advanced features must be configured later, including:
#' - multiple-host species or multiple-vector species
#' - exogenous forcing by weather, resources, or other factors
#' - vector control, vaccines, or other mass
#' @seealso [make_xds_object_template]
#' @param xds is `ode` or `dde` or `dts` for ordinary OR delay differential OR difference equations
#' @param Xname a character string defining a **X** Component module
#' @param XHoptions a list to configure the **X** Component module
#' @param MYname a character string defining an **MY** module
#' @param MYoptions options to set up the **MY** component 
#' @param Lname a character string defining a **L** Component module
#' @param Loptions a list to configure the **L** Component module
#' @param nPatches is the number of patches
#' @param HPop is the number of humans in each patch
#' @param residence is a vector that describes the patch where each human stratum lives
#' @param membership is a vector that describes the patch where each aquatic habitat is found
#' @param searchB is a vector of search weights for blood feeding
#' @param TimeSpent is either a TimeSpent matrix or a string to call a function that sets it up
#' @param K_matrix is either a K_matrix matrix or a string that defines how to set it up
#' @param searchQ is a vector of search weights for egg laying
#' @param BFopts a list to configure the blood feeding model
#' @param model_name is a name for the model (arbitrary)
#' @return an **`xds`** object
#' @export
xds_setup = function(xds = 'ode',
                     Xname = "SIS",
                     XHoptions = list(),
                     MYname = "macdonald",
                     MYoptions = list(),
                     Lname = "trivial",
                     Loptions = list(),
                     nPatches = 1,
                     HPop = 1000,
                     residence = 1,
                     membership = 1,
                     searchB = 1,
                     TimeSpent = list(),
                     K_matrix = list(),
                     searchQ = 1,
                     BFopts = list(),
                     model_name = "unnamed"
){
  stopifnot(length(HPop) == length(residence))
  xds_obj <- make_xds_object_template('ode', 'full', nPatches, membership, residence)
  
  # Aquatic Mosquito Dynamics
  xds_obj$Lname <- Lname
  xds_obj       <- setup_L_obj(Lname, xds_obj, 1, Loptions)
  xds_obj       <- setup_L_inits(xds_obj, 1, Loptions)
  
  # Adult Mosquito Dynamics
  xds_obj$MYname   <- MYname
  xds_obj           <- setup_MY_obj(MYname, xds_obj, 1, MYoptions)
  xds_obj           <- setup_MY_inits(xds_obj, 1, MYoptions)
  

  # Human Dynamics
  xds_obj$Xname <- Xname
  xds_obj       <- setup_XH_obj(Xname, xds_obj,  1, XHoptions)
  xds_obj       <- setup_XH_inits(xds_obj, HPop, 1, XHoptions)

  xds_obj = make_indices(xds_obj)

  Qwts       <- with(Loptions, checkIt(searchQ, xds_obj$nHabitats))
  xds_obj       <- change_habitat_weights(Qwts, xds_obj, 1)
  

  wts        <- with(BFopts, checkIt(searchB, xds_obj$nStrata))
  xds_obj    <- change_blood_search_weights(wts, xds_obj, 1, 1)
  

  if(is.matrix(TimeSpent))
    xds_obj <- change_TimeSpent_matrix(TimeSpent, xds_obj, 1)


  if(is.matrix(K_matrix))
    xds_obj <- change_K_matrix(K_matrix, xds_obj, 1)

  
  # Probably Not Necessary
  y0 <- as.vector(unlist(get_inits(xds_obj)))

  xds_obj <- check_models(xds_obj)
  
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
#' to dispatch [xde_derivatives.mosy] or [dts_update.mosy] and associated functions.
#'
#' The **X** Component module is `trivial`, but since humans / vertebrate hosts can be a
#' resource, `HPop` must be set.
#' @seealso [xds_setup] and [dMYdt.basicM]
#' @param xds is `ode` or `dde` or `dts` for ordinary OR delay differential OR difference equations
#' @param MYname is a character string defining a **MY** Component module
#' @param Lname is a character string defining a **L** Component module
#' @param nPatches is the number of patches
#' @param membership is a vector that describes the patch where each aquatic habitat is found
#' @param HPop is the human / host population density
#' @param K_matrix is either a K_matrix matrix or a string that defines how to set it up
#' @param searchQ is a vector of search weights for egg laying
#' @param kappa is a vector describing net infectiousness
#' @param MYoptions a list to configure the **MY** Component module
#' @param Loptions a list to configure the **L** Component module
#' @param model_name is a name for the model (arbitrary)
#' @return an **`xds`** object
#' @export
xds_setup_mosy = function(xds = 'ode',
                          ### Dynamical Components
                          MYname = "basicM",
                          Lname = "basicL",
                          ### Model Structure
                          nPatches = 1,
                          membership=1,
                          HPop = 1000,
                          ### Setup Parameters
                          K_matrix = list(),
                          searchQ = 1,
                          kappa = 0,
                          ### Options
                          MYoptions = list(),
                          Loptions = list(),
                          ### Name
                          model_name = "unnamed"

){
  residence = 1:nPatches
  HPop = checkIt(HPop, nPatches)
  xds_obj <- make_xds_object_template(xds, 'mosy', nPatches, membership, residence)
  
  # Adult Mosquito Dynamics
  xds_obj$MYname   <- MYname
  xds_obj           <- setup_MY_obj(MYname, xds_obj, 1, MYoptions)
  xds_obj           <- setup_MY_inits(xds_obj, 1, MYoptions)

  # Aquatic Mosquito Dynamics
  xds_obj$Lname <- Lname
  xds_obj       <- setup_L_obj(Lname, xds_obj, 1, Loptions)
  xds_obj       <- setup_L_inits(xds_obj, 1, Loptions)

  Xo <- list(kappa=kappa, HPop=HPop)
  xds_obj <- setup_XH_obj("trivial", xds_obj, 1, list())

  xds_obj = make_indices(xds_obj)

  Qwts       <- with(Loptions, checkIt(searchQ, xds_obj$nHabitats))
  xds_obj       <- change_habitat_weights(Qwts, xds_obj, 1)

  if(is.matrix(K_matrix))
    xds_obj <- change_K_matrix(K_matrix, xds_obj,1)

  xds_obj$terms$kappa[[1]] = checkIt(kappa, nPatches)

  xds_obj$model_name <- model_name

  return(xds_obj)
}


#' @title Build a Model of Immature Mosquito Ecology
#' 
#' @description 
#' \loadmathjax
#' 
#' A modified version of [xds_setup] that streamlines setup for an **L** Component
#' when the **MY** Component is set to `trivial.` The model
#' also sets **X** Component to the `trivial` module.
#'
#' The **`xds`** object defines `frame = class(frame) = 'aquatic'`
#' to dispatch [xde_derivatives.aquatic] or [dts_update.aquatic]
#'
#' @seealso [xds_setup]
#' @param xds is `ode` or `dde` or `dts` for ordinary OR delay differential OR difference equations
#' @param nHabitats is the number of habitats
#' @param Lname is a character string defining a **L** Component module
#' @param Loptions a list to configure the **L** Component module
#' @param MYoptions a list to configure [F_eggs.trivial]
#' @param model_name is a name for the model (arbitrary)
#' @return an **`xds`** object
#' @export
xds_setup_aquatic = function(xds = 'ode',
                             nHabitats=1,
                             Lname = "basicL",
                             Loptions = list(),
                             MYoptions = list(),
                             model_name = "unnamed"){

  nPatches= nHabitats
  membership = 1:nHabitats
  xds_obj <- make_xds_object_template(xds, 'aquatic', nPatches, membership)

  # Aquatic Mosquito Dynamics
  xds_obj$Lname <- Lname
  xds_obj       <- setup_L_obj(Lname, xds_obj, 1, Loptions)
  xds_obj       <- setup_L_inits(xds_obj, 1, Loptions)

  # Adult Mosquito Dynamics
  xds_obj$MYname   <- "trivial"
  xds_obj           <- setup_MY_obj("trivial", xds_obj, 1, MYoptions)

  # Human Dynamics
  xds_obj$Xname <- "trivial"
  xds_obj <- setup_XH_obj("trivial", xds_obj, 1, list())

  xds_obj = make_indices(xds_obj)
  xds_obj$model_name <- model_name
  return(xds_obj)
}


#' @title Build a Model of Human / Host Epidemiology
#'
#' @description 
#' \loadmathjax
#' 
#' A modified version of [xds_setup] that
#' streamlines setup for models with a trival **MY** Component.
#'
#' The **`xds`** object defines `frame = class(frame) = 'human'`
#' to dispatch [xde_derivatives.human] or [dts_update.human] and associated functions.
#'
#' The **MY** Component module is set to `trivial.` The funcion [F_fqZ.trivial] is called to compute passes the
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
#' @param xds is `ode` or `dde` or `dts` for ordinary OR delay differential OR difference equations
#' @param Xname a character string defining a **X** Component module
#' @param nPatches the number of patches
#' @param residence a vector that describes the patch where each human stratum lives
#' @param HPop the number of humans in each patch
#' @param searchB  a vector of search weights for blood feeding
#' @param TimeSpent  either a TimeSpent matrix or a string to call a function that sets it up
#' @param MYoptions list to configure the **MY** Component module
#' @param XHoptions a named list to configure the **X** Component module
#' @param BFopts list to configure the blood feeding model
#' @param model_name a name for the model
#' 
#' @return an **`xds`** model object
#' @export
xds_setup_human = function(Xname = "SIS",
                           XHoptions = list(),

                           xds = 'ode',
                           ### Dynamical Components
                           ### Model Structure
                           nPatches=1,
                           residence=1,
                           HPop=1000,
                           ### Setup Parameters
                           searchB = 1,
                           TimeSpent = list(),
                           ### Options
                           MYoptions = list(),
                           BFopts = list(),
                           ### Name
                           model_name = "unnamed"
){
  stopifnot(length(HPop) == length(residence))
  membership=1
  xds_obj <- make_xds_object_template(xds, 'human', nPatches, membership, residence)

  # Aquatic Mosquito Dynamics
  xds_obj       <- setup_L_obj("trivial", xds_obj, 1, list())
  xds_obj       <- setup_L_inits(xds_obj, 1)

  # Mosquito Dynamics
  xds_obj           <- setup_MY_obj("trivial", xds_obj, 1, MYoptions)
 
  # Human Dynamics
  xds_obj$Xname <- Xname
  xds_obj       <- setup_XH_obj(Xname, xds_obj,  1, XHoptions)
  xds_obj       <- setup_XH_inits(xds_obj, HPop, 1, XHoptions)

  xds_obj = make_indices(xds_obj)
  
  
  wts          <- with(BFopts, checkIt(searchB, xds_obj$nStrata))
  xds_obj      <- change_blood_search_weights(wts, xds_obj, 1, 1)

  if(is.matrix(TimeSpent))
    xds_obj <- change_TimeSpent_matrix(TimeSpent, xds_obj, 1)

  # Probably Not Necessary
  y0 <- as.vector(unlist(get_inits(xds_obj)))
  xds_obj <- BloodFeeding(0, y0, xds_obj)
  xds_obj <- Transmission(0, y0, xds_obj)

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
#' to dispatch [xde_derivatives.eir] or [dts_update.eir]
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
#' @param eir is the entomological inoculation rate
#' @param season_par parameters to configure a seasonality function using [make_function]
#' @param F_season a function describing a seasonal pattern over time
#' @param trend_par parameters to configure a trends function using [make_function]
#' @param F_trend a function describing a temporal trend over time
#' @param F_age a assigning a biting weight by age
#' @param age_par parameters to configure an age weights function using [make_function]
#' @param xds is `ode` or `dde` or `dts` for ordinary OR delay differential OR difference equations
#' @param Xname is a character string specifying an **X** Component module
#' @param XHoptions a list to configure the **X** Component module
#' @param HPop is the number of humans in each stratum
#' @param searchB is a vector of search weights for blood feeding
#' @param model_name is a name for the model (arbitrary)
#' @return an **`xds`** object
#' @export
xds_setup_eir = function(eir=1,
                         season_par = list(),
                         F_season = F_flat, 
                         trend_par = list(),
                         F_trend = F_flat, 
                         age_par = list(),
                         F_age = F_flat, 
                         xds = 'ode',

                         # Dynamical Components
                         Xname = "SIS",
                         XHoptions = list(),

                         # Model Structure
                         HPop=1000,
                         searchB = 1,

                         # Human Strata / Options
                         model_name = "unnamed"
){
  nPatches = length(HPop)
  residence = rep(1, length(HPop))
  membership = 1
  xds_obj <- make_xds_object_template(xds, 'eir', nPatches, membership, residence)
  xds_obj$forced_by = xds_obj$frame 

  xds_obj$EIRpar <- list()
  xds_obj$EIRpar$eir <- eir
  xds_obj$EIRpar$scale <- 1
  xds_obj$EIRpar$F_season <- F_season
  xds_obj$EIRpar$season_par <- season_par
  if(length(season_par)>0){
    xds_obj$EIRpar$F_season <- make_function(season_par)
  } 
  xds_obj$EIRpar$F_trend <- F_trend
  xds_obj$EIRpar$trend_par <- trend_par
  if(length(trend_par)>0){
    xds_obj$EIRpar$F_trend <- make_function(trend_par) 
  }
  xds_obj$EIRpar$F_age <- F_age
  xds_obj$EIRpar$age_par <- age_par
  if(length(age_par)>0){
    xds_obj$EIRpar$F_age <- make_function(age_par) 
  }

  # Aquatic Mosquito Dynamics
  xds_obj       <- setup_L_obj("trivial", xds_obj, 1, list())
  xds_obj       <- setup_L_inits(xds_obj, 1)

  # Adult Mosquito Dynamics
  xds_obj           <- setup_MY_obj("trivial", xds_obj, 1, list())

  # Human Dynamics
  xds_obj$Xname <- Xname
  xds_obj       <- setup_XH_obj(Xname, xds_obj,  1, XHoptions)
  xds_obj       <- setup_XH_inits(xds_obj, HPop, 1, XHoptions)
  
  xds_obj$forced_by = xds_obj$frame 

  xds_obj       = make_indices(xds_obj)

  wts        <- checkIt(searchB, xds_obj$nStrata)
  xds_obj    <- change_blood_search_weights(wts, xds_obj, 1, 1)

  # Probably Not Necessary
  y0 <- as.vector(unlist(get_inits(xds_obj)))
  xds_obj <- BloodFeeding(0, y0, xds_obj)

  xds_obj$model_name <- model_name

  return(xds_obj)
}
