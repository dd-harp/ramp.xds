# functions to set up models

#' @title Build a Model and Configure All Components
#'
#' @description
#' \loadmathjax
#' Make an **`xds`** *model object*:
#' - Define a module for each dynamical component (see [dynamical_components]):
#'    - **XH** Component -- human / host infection dynamics 
#'    - **MY** Component -- adult mosquito ecology and infection dynamics 
#'    - **L** Component -- aquatic mosquito ecology 
#' - Define basic structural parameters for a single host and vector population:
#'    - `nPatches` - the number of patches (see [patch_dynamics])
#'    - `membership` - the habitat membership vector (see [aquatic_habitats])
#'    - `nHabitats = length(membership)` 
#'    - `residence` - the human residence vector (see [human_populations])
#'    - `HPop` - the human population size (see [human_populations])
#'    - `nStrata = length(residence) = length(HPop)` 
#' - Configure some of the basic elements:
#'    - Search Weights (see [search_weights])
#'    - Mosquito Dispersal matrix (if `nPatches>1`; see [mosquito_dispersal])
#'    - Time Spent matrix (if `nPatches>`; see [time_spent])
#'
#' @note Other options can be configured after basic setup (see [xds_help_setup_options]). If the **MY** Component is the `trivial` module, consider using [xds_setup_aquatic] or [xds_setup_human] or [xds_setup_eir]. Models for mosquito
#' ecology need not include states describing infection status (see [xds_setup_mosy]).
#' 
#' @details
#' 1. [make_xds_object_template] returns an object template with
#' a properly configured interface for blood feeding 
#' and egg laying (see [xds_object]): 
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
#' 3. After configuring the dynamical components, several structural parameters can be configured at the command line:
#'
#'    - Habitat search weights can be set
#'    - Host population search weights can be set
#'    - A mosquito dispersal matrix, \eqn{\cal Koptions}, can be set
#'    - A time spent matrix, \eqn{\Theta}, can be set
#'
#' @seealso [make_xds_object_template], [xds_help_basic_setup]
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
#' @param searchB is a vector of search weights for blood feeding
#' @param TimeSpent is either a TimeSpent matrix or a string to call a function that sets it up
#' @param membership is a vector that describes the patch where each aquatic habitat is found
#' @param searchQ is a vector of search weights for egg laying
#' @param Koptions a K matrix, or options for [setup_K_matrix] (see [mosquito_dispersal])
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
                     TimeSpent = list(),
                     searchB = 1,
                     membership = 1,
                     Koptions = list(Kname = "no_setup"),
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

  if(is.matrix(Koptions)){
    xds_obj <- setup_K_matrix(Koptions, xds_obj)
  } else {
    xds_obj <- setup_K_matrix(Koptions$Kname, xds_obj, Koptions, 1)
  } 


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
#' to dispatch functions that compute derivatives (`xde`), update variables (`dts`),
#' and parse outputs
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
#' @param searchQ is a vector of search weights for egg laying
#' @param kappa is a vector describing net infectiousness
#' @param MYoptions a list to configure the **MY** Component module
#' @param Koptions a K matrix, or options for [setup_K_matrix] (see [mosquito_dispersal])
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
                          searchQ = 1,
                          kappa = 0,
                          ### Options
                          MYoptions = list(),
                          Koptions = list(Kname = "no_setup"),
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
  xds_obj    <- change_habitat_weights(Qwts, xds_obj, 1)
 
  if(is.matrix(Koptions)){
    xds_obj <- setup_K_matrix(Koptions, xds_obj)
  } else {
    xds_obj <- setup_K_matrix(Koptions$Kname, xds_obj, Koptions, 1)
  } 

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
#' to dispatch functions that compute derivatives (`xde`), update variables (`dts`),
#' and parse outputs
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
#' @return an **`xds`** object
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
#' @param eir is the entomological inoculation rate
#' @param season_par parameters to configure a seasonality function using [make_function]
#' @param trend_par parameters to configure a trends function using [make_function]
#' @param age_par parameters to configure an age weights function using [make_function]
#' @param shock_par parameters to configure a shock using [make_function]
#' @param xds is `ode` or `dde` or `dts` for ordinary OR delay differential OR difference equations
#' @param Xname is a character string specifying an **X** Component module
#' @param XHoptions a list to configure the **X** Component module
#' @param HPop is the number of humans in each stratum
#' @param searchB is a vector of search weights for blood feeding
#' @param model_name is a name for the model (arbitrary)
#' @return an **`xds`** object
#' @export
xds_setup_eir = function(eir=1,
                         season_par = makepar_F_one(),
                         trend_par = makepar_F_one(),
                         age_par = makepar_F_one(),
                         shock_par = makepar_F_one(),
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

  xds_obj$EIR_obj <- list()
  xds_obj$EIR_obj$eir <- eir
  xds_obj$EIR_obj$scale <- 1
  xds_obj$EIR_obj$season_par <- season_par
  xds_obj$EIR_obj$trend_par <- trend_par
  xds_obj$EIR_obj$age_par <- age_par
  xds_obj$EIR_obj$shock_par <- shock_par
  xds_obj = rebuild_forcing_functions(xds_obj, 1)

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
