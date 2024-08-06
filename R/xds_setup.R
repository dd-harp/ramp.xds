# functions to set up models

#' @title Set up and configure an **`xds`** model object
#' @description This sets up an **`xds`** model object with `pars$frame = 'full'` each
#' dynamical component fully configured for a single host and vector species:
#' - **`Xpar`** defines the human / host infection dynamics, \eqn{\cal XH}, of class `Xname`
#' - **`MYZpar`** defines the adult mosquito ecology & infection dynamics, \eqn{\cal MYZ}, of class `MYZname`
#' - **`Lpar`** defines the aquatic mosquito ecology, \eqn{\cal L}, of class `Lname`
#' - Parameter values for the models are passed as named lists: `Xopts`, `MYZopts`, and `Lopts`
#' - Search weights, the mosquito dispersal matrix, and the time spent matrix can also be
#' configured using parameters passed at the command line.
#' - Runtime parameters for `dts` models can be configured by passing values: `Xday`, `MYZday`, and `Lday`
#'
#' Advanced features must be configured later, including:
#' - multiple-host species or multiple-vector species
#' - exogenous forcing by weather, resources, or other factors
#' - vector control, vaccines, or other mass
#'
#' @param xds is `xde`/`dts` for differential / difference equations
#' @param dlay is either "ode" or "dde"
#' @param MYZname is a character string defining a MYZ model
#' @param Xname is a character string defining a X model
#' @param Lname is a character string defining a L model
#' @param nPatches is the number of patches
#' @param membership is a vector that describes the patch where each aquatic habitat is found
#' @param residence is a vector that describes the patch where each human stratum lives
#' @param HPop is the number of humans in each patch
#' @param Xday is the run-time time step for X component (in days): integer or 1/integer
#' @param MYZday is the run-time time step for MYZ component (in days): integer or 1/integer
#' @param Lday is the run-time time step for L component (in days): integer or 1/integer
#' @param searchB is a vector of search weights for blood feeding
#' @param TimeSpent is either a TimeSpent matrix or a string to call a function that sets it up
#' @param calK is either a calK matrix or a string that defines how to set it up
#' @param searchQ is a vector of search weights for egg laying
#' @param MYZopts a list to configure the MYZ model
#' @param Xopts a list to configure the X model
#' @param Lopts a list to configure the L model
#' @param BFopts a list to configure the blood feeding model
#' @param model_name is a name for the model (arbitrary)
#' @return a [list]
#' @export
xds_setup = function(xds = 'xde', dlay = 'ode',
                 ### Dynamical Components
                     MYZname = "RM",
                     Xname = "SIS",
                     Lname = "trace",
                 ### Model Structure
                     nPatches = 1,
                     membership=1,
                     residence=1,
                     HPop=1000,
                 ### Runtime Time parameters
                     Xday = 1,
                     MYZday = 1,
                     Lday = 1,
                 ### Setup Parameters
                     searchB = 1,
                     TimeSpent = list(),
                     calK    = list(),
                     searchQ = 1,
                 ### Options
                     MYZopts = list(),
                     Xopts = list(),
                     Lopts = list(),
                     BFopts = list(),
                 ### Name
                     model_name = "unnamed"
){
  stopifnot(length(HPop) == length(residence))
  pars <- make_xds_object('xde', 'full', dlay, nPatches, membership, residence)
  pars <- make_runtime(pars, Xday, MYZday, Lday, Lname)
  class(pars$compute) <- 'xde'

  # Aquatic Mosquito Dynamics
  pars$Lname <- Lname
  pars       <- make_Lpar(Lname, pars, 1, Lopts)
  pars       <- make_Linits(pars, 1, Lopts)

  # Adult Mosquito Dynamics
  pars$MYZname   <- MYZname
  pars           <- make_MYZpar(MYZname, pars, 1, MYZopts)
  pars           <- make_MYZinits(pars, 1, MYZopts)

  # Human Dynamics
  pars$Xname <- Xname
  pars       <- make_Xpar(Xname, pars,  1, Xopts)
  pars       <- make_Xinits(pars, HPop, 1, Xopts)
  pars       <- setup_Hpar_static(pars, 1)

  pars = make_indices(pars)

  Qwts       <- with(Lopts, checkIt(searchQ, pars$nHabitats))
  pars       <- change_habitat_weights(pars, Qwts, 1)

  wts        <- with(BFopts, checkIt(searchB, pars$nStrata))
  pars       <- change_blood_weights(pars, wts, 1, 1)

  if(is.matrix(TimeSpent))
    pars <- change_TimeSpent(TimeSpent, pars, 1)

  if(is.matrix(calK))
    pars <- change_calK(calK, pars,1)

  # Probably Not Necessary
  y0 <- as.vector(unlist(get_inits(pars)))

  pars <- MBionomics(0, y0, pars,1)
  pars <- EggLaying(0, y0, pars)
  pars <- BloodFeeding(0, y0, pars)
  pars <- Transmission(0, y0, pars)

  pars$model_name <- model_name

  return(pars)
}

#' @title Set up an **`xds`** model object for mosquito ecology
#' @param xds is `xde`/`dts` for differential / difference equations
#' @param dlay is either "ode" or "dde"
#' @param MYZname is a character string defining a MYZ model
#' @param Lname is a character string defining a L model
#' @param nPatches is the number of patches
#' @param membership is a vector that describes the patch where each aquatic habitat is found
#' @param HPop is the human population density
#' @param MYZday is the run-time time step for MYZ component (in days): integer or 1/integer
#' @param Lday is the run-time time step for L component (in days): integer or 1/integer
#' @param calK is either a calK matrix or a string that defines how to set it up
#' @param searchQ is a vector of search weights for egg laying
#' @param kappa is a vector describing net infectiousness
#' @param MYZopts a list to configure the MYZ model
#' @param Lopts a list to configure the L model
#' @param model_name is a name for the model (arbitrary)
#' @return a [list]
#' @export
xds_setup_mosy = function(xds = 'xde', dlay = 'ode',
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
  pars <- make_xds_object('xde', 'mosy', dlay, nPatches, membership, residence)
  pars <- make_runtime(pars, 1, MYZday, Lday, Lname)
  class(pars$compute) = "na"

  # Adult Mosquito Dynamics
  pars$MYZname   <- MYZname
  pars           <- make_MYZpar(MYZname, pars, 1, MYZopts)
  pars           <- make_MYZinits(pars, 1, MYZopts)

  # Aquatic Mosquito Dynamics
  pars$Lname <- Lname
  pars       <- make_Lpar(Lname, pars, 1, Lopts)
  pars       <- make_Linits(pars, 1, Lopts)

  Xo <- list(kappa=kappa, HPop=HPop)
  pars <- make_Xpar("trace", pars, 1, Xo)

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


#' @title Set up an **`xds`** model object for aquatic mosquito population dynamics
#' @param xds is `xde`/`dts` for differential / difference equations
#' @param dlay is either "ode" or "dde"
#' @param nHabitats is the number of habitats
#' @param Lname is a character string defining a L model
#' @param Lday is the run-time time step for L component (in days): integer or 1/integer
#' @param Lopts a list to configure the L model
#' @param MYZopts a list to configure F_eggs from the trace model
#' @param model_name is a name for the model (arbitrary)
#' @return a [list]
#' @export
xds_setup_aquatic = function(xds = 'xde', dlay = 'ode',
                             nHabitats=1,
                             Lname = "basicL",
                             Lday = 1,
                             Lopts = list(),
                             MYZopts = list(),
                             model_name = "unnamed"){

  nPatches= nHabitats
  membership = 1:nHabitats
  pars <- make_xds_object('xde', 'aquatic', dlay, nPatches, membership)
  pars <- make_runtime(pars, 1, 1, Lday, Lname)
  class(pars$compute) = "na"

  # Aquatic Mosquito Dynamics
  pars$Lname <- Lname
  pars       <- make_Lpar(Lname, pars, 1, Lopts)
  pars       <- make_Linits(pars, 1, Lopts)

  # Adult Mosquito Dynamics
  pars$MYZname   <- "trace"
  pars           <- make_MYZpar("trace", pars, 1, MYZopts)

  # Human Dynamics
  pars$Xname <- "trace"
  pars <- make_Xpar("trace", pars, 1, list())

  pars = make_indices(pars)
  pars$model_name <- model_name
  return(pars)
}


#' @title Set up an **`xds`** model object for forced human infection dynamics
#' @description
#' Set up a model to explore human dynamics forced by infective mosquitoes
#' @details
#' Write me
#' @param xds is `xde`/`dts` for differential / difference equations'
#' @param dlay  either "ode" or "dde"
#' @param Xname a character string defining a X model
#' @param nPatches the number of patches
#' @param residence a vector that describes the patch where each human stratum lives
#' @param HPop the number of humans in each patch
#' @param Xday is the run-time time step for X component (in days): integer or 1/integer
#' @param searchB  a vector of search weights for blood feeding
#' @param TimeSpent  either a TimeSpent matrix or a string to call a function that sets it up
#' @param MYZopts list to configure the MYZ model
#' @param Xopts list to configure the X model
#' @param BFopts list to configure the blood feeding model
#' @param model_name a name for the model
#' @return a [list]
#' @export
xds_setup_human = function(xds = 'xde', dlay = 'ode',
                           ### Dynamical Components
                           Xname = "SIS",
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
                           Xopts = list(),
                           BFopts = list(),
                           ### Name
                           model_name = "unnamed"
){
  stopifnot(length(HPop) == length(residence))
  membership=1
  pars <- make_xds_object('xde', 'human', dlay, nPatches, membership, residence)
  pars <- make_runtime(pars, Xday, 1, 1, "trace")
  pars$compute = 'na'
  class(pars$compute) <- 'na'

  # Aquatic Mosquito Dynamics
  pars       <- make_Lpar("trace", pars, 1, list())
  pars       <- make_Linits(pars, 1)

  # Mosquito Dynamics
  pars           <- make_MYZpar("trace", pars, 1, MYZopts)

  # Human Dynamics
  pars$Xname <- Xname
  pars       <- make_Xpar(Xname, pars,  1, Xopts)
  pars       <- make_Xinits(pars, HPop, 1, Xopts)
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

#' @title Set up an **`xds`** model object for forced human infection dynamics in cohorts
#' @description
#' The user supplies a function `F_eir(time, birthday, ...)` and specifies a model, and
#' the `xds` is set up to simulate dynamics for cohorts of different
#' ages experiencing exposure.
#' @details
#' Please write me
#' @param F_eir is a cohort exposure function
#' @param xds is `xde`/`dts` for differential / difference equations
#' @param dlay is either "ode" or "dde"
#' @param bday the birthday of a cohort
#' @param scale the birthday of a cohort
#' @param model_name is a name for the model (arbitrary)
#' @param Xname is a character string defining a X model
#' @param HPop is the number of humans in each stratum
#' @param Xday is the run-time time step for X component (in days): integer or 1/integer
#' @param searchB is a vector of search weights for blood feeding
#' @param Xopts a list to configure the X model
#' @return a [list]
#' @export
xds_setup_cohort = function(F_eir,
                            xds = 'xde', dlay = 'ode',
                            Xopts = list(),
                            bday=0, scale=1,

                            # Dynamical Components
                            Xname = "SIS",
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
  pars <- make_xds_object('xde', 'cohort', dlay, nPatches, membership, residence)
  pars <- make_runtime(pars, Xday, 1, 1, "trace")
  class(pars$compute) <- "na"

  pars$EIRpar$bday = bday
  pars$EIRpar$scale = scale
  pars$F_eir = F_eir

  # Aquatic Mosquito Dynamics
  pars       <- make_Lpar("trace", pars, 1, list())
  pars       <- make_Linits(pars, 1)

  # Adult Mosquito Dynamics
  pars           <- make_MYZpar("trace", pars, 1, list())

  # Human Dynamics
  pars$Xname <- Xname
  pars       <- make_Xpar(Xname, pars,  1, Xopts)
  pars       <- make_Xinits(pars, HPop, 1, Xopts)
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
