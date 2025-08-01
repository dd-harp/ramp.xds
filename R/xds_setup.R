# functions to set up models

#' @title Build a Model with All Components 
#'
#' @description 
#' \loadmathjax
#' Make an **`xds`** *model object*:
#' - Define the dynamical components:
#'    - **XH** - Component modules for human / host infection dynamics of class **`Xname`** with trivial demographics
#'    - **MYZ** - Component modules for adult mosquito ecology and infection dynamics of class **`MYZname`**
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
#' - Configure runtime parameters for discrete-time systems
#'
#' Advanced options can be configured after basic setup.
#'
#' @note If the **MYZ** Component is the `trivial` module, consider using [xds_setup_aquatic] or [xds_setup_human] or [xds_setup_cohort]. Models for mosquito
#' ecology need not include states describing infection status (see [xds_setup_mosy]).
#' @details
#' 1. Using the basic structural parameters, a basic template is created by [make_xds_template] with
#' a properly configured interface for blood feeding and egg laying, and `pars$frame = class(pars$frame) = 'full'` .
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
#'    - **`pars$Xpar[[1]]`** defines a model for human / host infection dynamics of class `Xname` (the **X** component). The parameter values passed in a named list, `Xopts.`
#'    - **`pars$MYZpar[[1]]`** defines a model for adult mosquito ecology & infection dynamics of class `MYZname` (the **MYZ** Component). The parameter values are passed in a named list, `MYZopts.`
#'    - **`pars$Lpar[[1]]`** defines a model for aquatic mosquito ecology of class `Lname` (The **L** Component). The parameter values are passed in a named list, `Lopts.`
#'
#' 3. After configuring the dynamical components, several structural parameters can be configured at the command line:
#'
#'    - Habitat search weights can be set
#'    - Host population search weights can be set
#'    - A mosquito dispersal matrix, \eqn{\cal K}, can be set
#'    - A time spent matrix, \eqn{\Theta}, can be set
#'    - Runtime parameters for `dts` models can be configured by passing values: `Xday`, `MYZday`, and `Lday`
#'
#' Advanced features must be configured later, including:
#' - multiple-host species or multiple-vector species
#' - exogenous forcing by weather, resources, or other factors
#' - vector control, vaccines, or other mass
#' @seealso [make_xds_template]
#' @param xds is `ode` or `dde` or `dts` for ordinary OR delay differential OR difference equations
#' @param Xname a character string defining a **X** Component module
#' @param Xopts a list to configure the **X** Component module
#' @param MYZname a character string defining a **MYZ** Component module
#' @param MYZopts a list to configure the **MYZ** Component module
#' @param Lname a character string defining a **L** Component module
#' @param Lopts a list to configure the **L** Component module
#' @param nPatches is the number of patches
#' @param HPop is the number of humans in each patch
#' @param residence is a vector that describes the patch where each human stratum lives
#' @param membership is a vector that describes the patch where each aquatic habitat is found
#' @param searchB is a vector of search weights for blood feeding
#' @param TimeSpent is either a TimeSpent matrix or a string to call a function that sets it up
#' @param calK is either a calK matrix or a string that defines how to set it up
#' @param searchQ is a vector of search weights for egg laying
#' @param Xday is the run-time time step for **X** Component (in days): integer or 1/integer
#' @param MYZday is the run-time time step for **MYZ** Component (in days): integer or 1/integer
#' @param Lday is the run-time time step for **L** Component (in days): integer or 1/integer
#' @param BFopts a list to configure the blood feeding model
#' @param model_name is a name for the model (arbitrary)
#' @return an **`xds`** object
#' @export
xds_setup = function(xds = 'ode',
                     Xname = "SIS",
                     Xopts = list(),
                     MYZname = "macdonald",
                     MYZopts = list(),
                     Lname = "trivial",
                     Lopts = list(),
                     nPatches = 1,
                     HPop = 1000,
                     residence = 1,
                     membership = 1,
                     searchB = 1,
                     TimeSpent = list(),
                     calK = list(),
                     searchQ = 1,
                     Xday = 1,
                     MYZday = 1,
                     Lday = 1,
                     BFopts = list(),
                     model_name = "unnamed"
){
  stopifnot(length(HPop) == length(residence))
  pars <- make_xds_template('ode', 'full', nPatches, membership, residence)
  pars <- make_runtime(pars, Xday, MYZday, Lday, Lname)
  class(pars$compute) <- 'ode'

  # Aquatic Mosquito Dynamics
  pars$Lname <- Lname
  pars       <- setup_Lpar(Lname, pars, 1, Lopts)
  pars       <- setup_Linits(pars, 1, Lopts)

  # Adult Mosquito Dynamics
  pars$MYZname   <- MYZname
  pars           <- setup_MYZpar(MYZname, pars, 1, MYZopts)
  pars           <- setup_MYZinits(pars, 1, MYZopts)
  

  # Human Dynamics
  pars$Xname <- Xname
  pars       <- setup_Xpar(Xname, pars,  1, Xopts)
  pars       <- setup_Xinits(pars, HPop, 1, Xopts)
  pars       <- setup_Hpar_static(pars, 1)

  pars = make_indices(pars)

  Qwts       <- with(Lopts, checkIt(searchQ, pars$nHabitats))
  pars       <- change_habitat_weights(pars, Qwts, 1)
  

  wts        <- with(BFopts, checkIt(searchB, pars$nStrata))
  pars       <- change_blood_weights(pars, wts, 1, 1)
  

  if(is.matrix(TimeSpent))
    pars <- change_TimeSpent(TimeSpent, pars, 1)

  
  if(is.matrix(calK))
    pars <- change_calK(calK, pars, 1)

  
  # Probably Not Necessary
  y0 <- as.vector(unlist(get_inits(pars)))

  pars <- MBionomics(0, y0, pars,1)
  pars <- EggLaying(0, y0, pars)
  pars <- BloodFeeding(0, y0, pars)
  pars <- Transmission(0, y0, pars)

  pars$model_name <- model_name

  return(pars)
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
#' @seealso [xds_setup] and [dMYZdt.basicM]
#' @param xds is `ode` or `dde` or `dts` for ordinary OR delay differential OR difference equations
#' @param MYZname is a character string defining a **MYZ** Component module
#' @param Lname is a character string defining a **L** Component module
#' @param nPatches is the number of patches
#' @param membership is a vector that describes the patch where each aquatic habitat is found
#' @param HPop is the human / host population density
#' @param MYZday is the run-time time step for **MYZ** Component (in days): integer or 1/integer
#' @param Lday is the run-time time step for **L** Component (in days): integer or 1/integer
#' @param calK is either a calK matrix or a string that defines how to set it up
#' @param searchQ is a vector of search weights for egg laying
#' @param kappa is a vector describing net infectiousness
#' @param MYZopts a list to configure the **MYZ** Component module
#' @param Lopts a list to configure the **L** Component module
#' @param model_name is a name for the model (arbitrary)
#' @return an **`xds`** object
#' @export
xds_setup_mosy = function(xds = 'ode',
                          ### Dynamical Components
                          MYZname = "basicM",
                          Lname = "basicL",
                          ### Model Structure
                          nPatches = 1,
                          membership=1,
                          HPop = 1000,
                          ### Runtime Time parameters
                          MYZday = 1,
                          Lday = 1,
                          ### Setup Parameters
                          calK = list(),
                          searchQ = 1,
                          kappa = 0,
                          ### Options
                          MYZopts = list(),
                          Lopts = list(),
                          ### Name
                          model_name = "unnamed"

){
  residence = 1:nPatches
  HPop = checkIt(HPop, nPatches)
  pars <- make_xds_template('ode', 'mosy', nPatches, membership, residence)
  pars <- make_runtime(pars, 1, MYZday, Lday, Lname)
  class(pars$compute) = "na"

  # Adult Mosquito Dynamics
  pars$MYZname   <- MYZname
  pars           <- setup_MYZpar(MYZname, pars, 1, MYZopts)
  pars           <- setup_MYZinits(pars, 1, MYZopts)

  # Aquatic Mosquito Dynamics
  pars$Lname <- Lname
  pars       <- setup_Lpar(Lname, pars, 1, Lopts)
  pars       <- setup_Linits(pars, 1, Lopts)

  Xo <- list(kappa=kappa, HPop=HPop)
  pars <- setup_Xpar("trivial", pars, 1, Xo)

  pars = make_indices(pars)

  Qwts       <- with(Lopts, checkIt(searchQ, pars$nHabitats))
  pars       <- change_habitat_weights(pars, Qwts, 1)

  if(is.matrix(calK))
    pars <- change_calK(calK, pars,1)

  # Probably Not Necessary
  y0 <- as.vector(unlist(get_inits(pars)))
  pars <- MBionomics(0, y0, pars,1)
  pars <- EggLaying(0, y0, pars)
  pars <- BloodFeeding(0, y0, pars)

  pars$kappa[[1]] = checkIt(kappa, nPatches)

  pars$model_name <- model_name

  return(pars)
}


#' @title Build a Model of Immature Mosquito Ecology
#' 
#' @description 
#' \loadmathjax
#' 
#' A modified version of [xds_setup] that streamlines setup for an **L** Component
#' when the **MYZ** Component is set to `trivial.` The model
#' also sets **X** Component to the `trivial` module.
#'
#' The **`xds`** object defines `frame = class(frame) = 'aquatic'`
#' to dispatch [xde_derivatives.aquatic] or [dts_update.aquatic]
#'
#' @seealso [xds_setup]
#' @param xds is `ode` or `dde` or `dts` for ordinary OR delay differential OR difference equations
#' @param nHabitats is the number of habitats
#' @param Lname is a character string defining a **L** Component module
#' @param Lday is the run-time time step for **L** Component (in days): integer or 1/integer
#' @param Lopts a list to configure the **L** Component module
#' @param MYZopts a list to configure [F_eggs.trivial]
#' @param model_name is a name for the model (arbitrary)
#' @return an **`xds`** object
#' @export
xds_setup_aquatic = function(xds = 'ode',
                             nHabitats=1,
                             Lname = "basicL",
                             Lday = 1,
                             Lopts = list(),
                             MYZopts = list(),
                             model_name = "unnamed"){

  nPatches= nHabitats
  membership = 1:nHabitats
  pars <- make_xds_template('ode', 'aquatic', nPatches, membership)
  pars <- make_runtime(pars, 1, 1, Lday, Lname)
  class(pars$compute) = "na"

  # Aquatic Mosquito Dynamics
  pars$Lname <- Lname
  pars       <- setup_Lpar(Lname, pars, 1, Lopts)
  pars       <- setup_Linits(pars, 1, Lopts)

  # Adult Mosquito Dynamics
  pars$MYZname   <- "trivial"
  pars           <- setup_MYZpar("trivial", pars, 1, MYZopts)

  # Human Dynamics
  pars$Xname <- "trivial"
  pars <- setup_Xpar("trivial", pars, 1, list())

  pars = make_indices(pars)
  pars$model_name <- model_name
  return(pars)
}


#' @title Build a Model of Human / Host Epidemiology
#'
#' @description 
#' \loadmathjax
#' 
#' A modified version of [xds_setup] that
#' streamlines setup for models with a trival **MYZ** Component.
#'
#' The **`xds`** object defines `frame = class(frame) = 'human'`
#' to dispatch [xde_derivatives.human] or [dts_update.human] and associated functions.
#'
#' The **MYZ** Component module is set to `trivial.` The funcion [F_fqZ.trivial] is called to compute passes the
#' density of infectious adult mosquitoes, and \eqn{f} and \eqn{q} can still be
#' configured. In this case, the daily EIR is computed using the blood feeding
#' interface, including [Exposure], in the same way as a model with a non-trivial
#' **MYZ** Component module.
#'
#' To study human cohort dynamics by passing a function that computes the daily EIR,
#' consider using [xds_setup_cohort].
#'
#' @seealso [xds_setup] and [xds_setup_cohort]
#'
#' @param xds is `ode` or `dde` or `dts` for ordinary OR delay differential OR difference equations
#' @param Xname a character string defining a **X** Component module
#' @param nPatches the number of patches
#' @param residence a vector that describes the patch where each human stratum lives
#' @param HPop the number of humans in each patch
#' @param Xday is the run-time time step for **X** Component (in days): integer or 1/integer
#' @param searchB  a vector of search weights for blood feeding
#' @param TimeSpent  either a TimeSpent matrix or a string to call a function that sets it up
#' @param MYZopts list to configure the **MYZ** Component module
#' @param Xopts a named list to configure the **X** Component module
#' @param BFopts list to configure the blood feeding model
#' @param model_name a name for the model
#' @return an **`xds`** object
#' @export
xds_setup_human = function(Xname = "SIS",
                           Xopts = list(),

                           xds = 'ode',
                           ### Dynamical Components
                           ### Model Structure
                           nPatches=1,
                           residence=1,
                           HPop=1000,
                           Xday = 1,
                           ### Setup Parameters
                           searchB = 1,
                           TimeSpent = list(),
                           ### Options
                           MYZopts = list(),
                           BFopts = list(),
                           ### Name
                           model_name = "unnamed"
){
  stopifnot(length(HPop) == length(residence))
  membership=1
  pars <- make_xds_template('ode', 'human', nPatches, membership, residence)
  pars <- make_runtime(pars, Xday, 1, 1, "trivial")
  pars$compute = 'na'
  class(pars$compute) <- 'na'

  # Aquatic Mosquito Dynamics
  pars       <- setup_Lpar("trivial", pars, 1, list())
  pars       <- setup_Linits(pars, 1)

  # Mosquito Dynamics
  pars           <- setup_MYZpar("trivial", pars, 1, MYZopts)

  # Human Dynamics
  pars$Xname <- Xname
  pars       <- setup_Xpar(Xname, pars,  1, Xopts)
  pars       <- setup_Xinits(pars, HPop, 1, Xopts)
  pars       <- setup_Hpar_static(pars, 1)

  pars = make_indices(pars)

  wts        <- with(BFopts, checkIt(searchB, pars$nStrata))
  pars       <- change_blood_weights(pars, wts, 1, 1)

  if(is.matrix(TimeSpent))
    pars <- change_TimeSpent(TimeSpent, pars, 1)

  # Probably Not Necessary
  y0 <- as.vector(unlist(get_inits(pars)))
  pars <- BloodFeeding(0, y0, pars)
  pars <- Transmission(0, y0, pars)

  pars$model_name <- model_name

  return(pars)
}

#' @title Build a Model of Human / Host Cohort Dynamics
#' @description 
#' \loadmathjax
#' 
#' A modified version of [xds_setup] to setup up studies of cohort
#' dynamics.
#'
#' The **`xds`** object defines `frame = class(frame) = 'cohort'` but there
#' is no `cohort` case for [xds_solve]. Instead, cohort
#' dynamics are studied using [xds_solve_cohort], which was designed
#' to compare the outcomes for cohorts of different ages when exposure is
#' changing.
#'
#' The interface includes options to configure a function
#' describing `F_eir` as a function of time, with seasonal components
#' and a trend. Exposure in a cohort is a function of its age, including
#' a function that modifies exposure by age.
#'
#' @seealso [xds_setup] and [xds_setup_human] and [xds_solve_cohort]
#'
#' @param eir is the entomological inoculation rate
#' @param F_season a function describing a seasonal pattern over time
#' @param season_par parameters to configure a seasonality function using [make_function]
#' @param F_trend a function describing a temporal trend over time
#' @param trend_par parameters to configure a trends function using [make_function]
#' @param F_age a assigning a biting weight by age
#' @param age_par parameters to configure an age weights function using [make_function]
#' @param xds is `ode` or `dde` or `dts` for ordinary OR delay differential OR difference equations
#' @param Xname is a character string specifying an **X** Component module
#' @param Xopts a list to configure the **X** Component module
#' @param Xday is the run-time time step for **X** Component (in days): integer or 1/integer
#' @param HPop is the number of humans in each stratum
#' @param searchB is a vector of search weights for blood feeding
#' @param model_name is a name for the model (arbitrary)
#' @return an **`xds`** object
#' @export
xds_setup_cohort = function(eir=1,
                            F_season = F_flat, season_par = list(),
                            F_trend = F_flat, trend_par = list(),
                            F_age = F_flat, age_par = list(),
                            xds = 'ode',

                            # Dynamical Components
                            Xname = "SIS",
                            Xopts = list(),
                            Xday = 1,

                            # Model Structure
                            HPop=1000,
                            searchB = 1,

                            # Human Strata / Options
                            model_name = "unnamed"
){
  nPatches = length(HPop)
  residence = rep(1, length(HPop))
  membership = 1
  pars <- make_xds_template('ode', 'cohort', nPatches, membership, residence)
  pars <- make_runtime(pars, Xday, 1, 1, "trivial")
  class(pars$compute) <- "na"

  pars$EIRpar <- list()
  pars$EIRpar$eir <- eir
  pars$EIRpar$scale <- 1
  
  pars$EIRpar$F_season <- F_season
  pars$EIRpar$season_par <- season_par
  if(length(season_par)>0){
    pars$EIRpar$F_season <- make_function(season_par)
  } 
  
  pars$EIRpar$F_trend <- F_trend
  pars$EIRpar$trend_par <- trend_par
  if(length(trend_par)>0){
    pars$EIRpar$F_trend <- make_function(trend_par) 
  }
  
  pars$EIRpar$F_age <- F_age
  pars$EIRpar$age_par <- age_par
  if(length(age_par)>0){
    pars$EIRpar$F_age <- make_function(age_par) 
  } 
  
  pars <- set_eir(eir, pars)

  # Aquatic Mosquito Dynamics
  pars       <- setup_Lpar("trivial", pars, 1, list())
  pars       <- setup_Linits(pars, 1)

  # Adult Mosquito Dynamics
  pars           <- setup_MYZpar("trivial", pars, 1, list())

  # Human Dynamics
  pars$Xname <- Xname
  pars       <- setup_Xpar(Xname, pars,  1, Xopts)
  pars       <- setup_Xinits(pars, HPop, 1, Xopts)
  pars       <- setup_Hpar_static(pars, 1)

  pars = make_indices(pars)

  wts        <- checkIt(searchB, pars$nStrata)
  pars       <- change_blood_weights(pars, wts, 1, 1)

  # Probably Not Necessary
  y0 <- as.vector(unlist(get_inits(pars)))
  pars <- BloodFeeding(0, y0, pars)

  pars$model_name <- model_name

  return(pars)
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
#' and a trend. Exposure in a cohort is a function of its age, including
#' a function that modifies exposure by age. Models set up 
#' with `xds_setup_eir` are like models set up with 
#' [xds_setup_cohort], but they lack a function to model exposure by 
#' age.  
#'
#' @seealso [xds_setup] and [xds_setup_human] 
#'
#' @param eir is the entomological inoculation rate
#' @param F_season a function describing a seasonal pattern over time
#' @param season_par parameters to configure a seasonality function using [make_function]
#' @param F_trend a function describing a temporal trend over time
#' @param trend_par parameters to configure a trends function using [make_function]
#' @param xds is `ode` or `dde` or `dts` for ordinary OR delay differential OR difference equations
#' @param Xname is a character string specifying an **X** Component module
#' @param Xopts a list to configure the **X** Component module
#' @param Xday is the run-time time step for **X** Component (in days): integer or 1/integer
#' @param HPop is the number of humans in each stratum
#' @param searchB is a vector of search weights for blood feeding
#' @param model_name is a name for the model (arbitrary)
#' @return an **`xds`** object
#' @export
xds_setup_eir = function(eir=1,
                            F_season = F_flat, season_par = list(),
                            F_trend = F_flat, trend_par = list(),
                            xds = 'ode',

                            # Dynamical Components
                            Xname = "SIS",
                            Xopts = list(),
                            Xday = 1,

                            # Model Structure
                            HPop=1000,
                            searchB = 1,

                            # Human Strata / Options
                            model_name = "unnamed"
){
  nPatches = length(HPop)
  residence = rep(1, length(HPop))
  membership = 1
  pars <- make_xds_template('ode', 'eir', nPatches, membership, residence)
  pars <- make_runtime(pars, Xday, 1, 1, "trivial")
  class(pars$compute) <- "na"

  pars$EIRpar <- list()
  pars$EIRpar$eir <- eir
  pars$EIRpar$scale <- 1
  pars$EIRpar$F_season <- F_season
  pars$EIRpar$season_par <- season_par
  if(length(season_par)>0){
    pars$EIRpar$F_season <- make_function(season_par)
  } 
  pars$EIRpar$F_trend <- F_trend
  pars$EIRpar$trend_par <- trend_par
  if(length(trend_par)>0){
    pars$EIRpar$F_trend <- make_function(trend_par) 
  }
  
  pars <- set_eir(eir, pars)

  # Aquatic Mosquito Dynamics
  pars       <- setup_Lpar("trivial", pars, 1, list())
  pars       <- setup_Linits(pars, 1)

  # Adult Mosquito Dynamics
  pars           <- setup_MYZpar("trivial", pars, 1, list())

  # Human Dynamics
  pars$Xname <- Xname
  pars       <- setup_Xpar(Xname, pars,  1, Xopts)
  pars       <- setup_Xinits(pars, HPop, 1, Xopts)
  pars       <- setup_Hpar_static(pars, 1)

  pars = make_indices(pars)

  wts        <- checkIt(searchB, pars$nStrata)
  pars       <- change_blood_weights(pars, wts, 1, 1)

  # Probably Not Necessary
  y0 <- as.vector(unlist(get_inits(pars)))
  pars <- BloodFeeding(0, y0, pars)

  pars$model_name <- model_name

  return(pars)
}
