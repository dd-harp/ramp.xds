# functions to set up models


#' @title Set up a model for xde_diffeqn
#' @param modelName is a name for the model (arbitrary)
#' @param MYZname is a character string defining a MYZ model
#' @param Xname is a character string defining a X model
#' @param Lname is a character string defining a L model
#' @param nPatches is the number of patches
#' @param nVectors is the number of vector species
#' @param nHosts is the number of vertebrate host species
#' @param HPop is the number of humans in each patch
#' @param membership is a vector that describes the patch where each aquatic habitat is found
#' @param MYZopts a list to configure the MYZ model
#' @param calK is either a calK matrix or a string that defines how to set it up
#' @param calKopts are the options to setup calK
#' @param EIPopts a list with options to setup EIPmod; must include EIPname
#' @param Xopts a list to configure the X model
#' @param BFopts a list to configure the blood feeding model
#' @param residence is a vector that describes the patch where each human stratum lives
#' @param searchB is a vector of search weights for blood feeding
#' @param F_circadian is a function describing mosquito daily activity
#' @param TimeSpent is either a TimeSpent matrix or a string to call a function that sets it up
#' @param TimeSpentOpts are the options to setup TimeSpent
#' @param searchQ is a vector of search weights for egg laying
#' @param Lopts a list to configure the L model
#' @param dlay is either "ode" or "dde"
#' @return a [list]
#' @export
xde_setup = function(modelName = "unnamed",

                     # Dynamical Components
                     MYZname = "RM",
                     Xname = "SIS",
                     Lname = "trace",

                     # Model Structure
                     nPatches = 1,
                     nVectors = 1,
                     nHosts = 1,
                     HPop=1000,
                     membership=1,

                     # Adult Mosquito Options
                     MYZopts = list(),
                     calK ="herethere",
                     calKopts = list(),
                     EIPopts = list(EIPname="static_xde", eip=12),

                     # Human Strata / Options
                     Xopts = list(),

                     # Blood Feeding
                     BFopts = list(),
                     residence=1,
                     searchB = 1,
                     F_circadian = NULL,
                     TimeSpent = "athome",
                     TimeSpentOpts=list(),

                     # Aquatic Mosquito Options
                     searchQ = 1,
                     Lopts = list(),
                     dlay = 'ode'

){

  pars <- make_xds_object('xde', 'full', dlay)
  class(pars$compute) <- 'xde'

  pars$modelName = modelName
  pars$Xname = Xname
  pars$MYZname = MYZname
  pars$Lname = Lname


  # Fixed Structural Elements
  pars$nPatches = nPatches
  pars$nVectors = nVectors
  pars$nHosts = nHosts
  pars$nHabitats = length(membership)
  pars$membership = membership
  pars$habitat_matrix = create_habitat_matrix(pars$nPatches, pars$membership)

  # Adult Mosquito Dynamics
  calK = make_calK(nPatches, calK, calKopts)

  pars = xde_setup_MYZpar(MYZname, pars, 1, EIPopts, MYZopts, calK)
  pars = setup_MYZinits(pars, 1, MYZopts)

  # Human Demography
  pars = setup_Hpar_static(pars, 1, HPop)
  # Blood Feeding
  pars = setup_BloodFeeding(pars, 1, 1, BFopts, residence, searchB, F_circadian)
  pars = make_TimeSpent(pars, 1, TimeSpent, TimeSpentOpts)
  # Vertebrate Host Dynamics
  pars = xde_setup_Xpar(Xname, pars, 1, Xopts)
  pars = setup_Xinits(pars, 1, Xopts)

  # Aquatic Mosquito Dynamics
  pars = xde_setup_Lpar(Lname, pars, 1, Lopts)
  pars = setup_Linits(pars, 1, Lopts)
  # Egg Laying
  pars = setup_egg_laying_static(pars, searchQ, 1, Lopts)

  pars = make_indices(pars)

  y0 <- get_inits(pars)
  pars <- Resources(0, y0, pars)
  pars <- Bionomics(0, y0, pars)
  pars <- Transmission(0, y0, pars)

  return(pars)
}

#' @title Set up a model for xde_diffeqn_mosy
#' @param modelName is a name for the model (arbitrary)
#' @param MYZname is a character string defining a MYZ model
#' @param Lname is a character string defining a L model
#' @param nPatches is the number of patches
#' @param nVectors is the number of vector species
#' @param membership is a vector that describes the patch where each aquatic habitat is found
#' @param MYZopts a list to configure the MYZ model
#' @param calK is either a calK matrix or a string that defines how to set it up
#' @param calKopts are the options to setup calK
#' @param searchQ is a vector of search weights for egg laying
#' @param Lopts a list to configure the L model
#' @param kappa values -- net infectivity to force adult infection dynamics
#' @param dlay is either "ode" or "dde"
#' @return a [list]
#' @export
xde_setup_mosy = function(modelName = "unnamed",

                     # Dynamical Components
                     MYZname = "basicM",
                     Lname = "basic",

                     # Model Structure
                     nPatches = 1,
                     nVectors = 1,
                     membership=1,

                     # Adult Mosquito Options
                     MYZopts = list(),
                     calK ="herethere",
                     calKopts = list(),

                     # Aquatic Mosquito Options
                     searchQ = 1,
                     Lopts = list(),

                     # forcing
                     kappa=NULL,
                     dlay = 'ode'
){
  pars <- make_xds_object('xde', 'mosy', dlay)
  class(pars$compute) = "na"

  pars$modelName = modelName
  pars$MYZname = MYZname
  pars$Lname = Lname

  # Structure
  pars$nPatches = nPatches
  pars$nHabitats = length(membership)
  pars$membership = membership
  pars$habitat_matrix = create_habitat_matrix(pars$nPatches, pars$membership)
  pars$nVectors = 1

  # Dynamics
  calK = make_calK(nPatches, calK, calKopts)
  pars = xde_setup_MYZpar(MYZname, pars, 1, EIPopts=list(), MYZopts, calK)
  pars = setup_MYZinits(pars, 1, MYZopts)

  # Aquatic Mosquito Dynamics
  pars = xde_setup_Lpar(Lname, pars, 1, Lopts)
  pars = setup_Linits(pars, 1, Lopts)
  pars = setup_egg_laying_static(pars, searchQ, 1, Lopts)

  if(is.null(kappa))  kappa = rep(0, nPatches)
  pars$kappa[[1]] = checkIt(kappa, nPatches)

  pars = make_indices(pars)

  return(pars)
}


#' @title Set up a model for xde_diffeqn_aqua
#' @param modelName is a name for the model (arbitrary)
#' @param nHabitats is the number of habitats
#' @param nVectors is the number of vector species
#' @param Lname is a character string defining a L model
#' @param Lopts a list to configure the L model
#' @param MYZopts a list to configure F_eggs from the trace model
#' @param LSMname is a character string defining a LSM model
#' @param dlay is either "ode" or "dde"
#' @return a [list]
#' @export
xde_setup_aquatic = function(modelName = "unnamed",
                     nHabitats = 1,
                     nVectors = 1,
                     Lname = "basic",
                     Lopts = list(),
                     MYZopts = list(),
                     LSMname = "null",
                     dlay = 'ode'){

  pars <- make_xds_object('xde', 'aquatic', dlay)
  class(pars$compute) = "na"

  pars$modelName = modelName
  pars$MYZname = "trace"
  pars$Lname = Lname

  pars$nHabitats = nHabitats
  membership = 1:nHabitats
  pars$membership = membership
  pars$habitat_matrix = create_habitat_matrix(pars$nHabitats, pars$membership)
  searchQ = rep(1, nHabitats)
  pars = xde_setup_Lpar(Lname, pars, 1, Lopts)
  pars = setup_Linits(pars, 1, Lopts)
  pars$nPatches = nHabitats

  pars$nVectors = nVectors
  pars = xde_setup_MYZpar("trace", pars, 1, EIPopts=list(), MYZopts, calK=NULL)

  pars <- setup_lsm_null(pars)

  pars = make_indices(pars)


  return(pars)
}


#' @title Set up a model for xde_diffeqn_human
#' @param modelName is a name for the model (arbitrary)
#' @param Xname is a character string defining a X model
#' @param HPop is the number of humans in each patch
#' @param MYZopts a list to configure the MYZ model
#' @param Xopts a list to configure the X model
#' @param BFopts a list to configure the blood feeding model
#' @param residence is a vector that describes the patch where each human stratum lives
#' @param searchB is a vector of search weights for blood feeding
#' @param F_circadian is a function describing mosquito daily activity
#' @param TimeSpent is either a TimeSpent matrix or a string to call a function that sets it up
#' @param TimeSpentOpts are the options to setup TimeSpent
#' @param dlay is either "ode" or "dde"
#' @return a [list]
#' @export
xde_setup_human = function(modelName = "unnamed",

                     # Dynamical Components
                     Xname = "SIS",

                     # Model Structure
                     HPop=1000,

                     # Adult Mosquito Options
                     MYZopts = list(),

                     # Human Strata / Options
                     Xopts = list(),

                     # Blood Feeding
                     BFopts = list(),
                     residence=1,
                     searchB = 1,
                     F_circadian = NULL,
                     TimeSpent = "athome",
                     TimeSpentOpts=list(),
                     dlay = 'ode'

){

  pars <- make_xds_object('xde', 'human', dlay)
  pars$compute = "na"

  pars$modelName = modelName
  pars$Xname = Xname
  pars$MYZname = "trace"

  # Structure
  nStrata = length(HPop)
  pars$nPatches = as.integer(nStrata)
  pars$nStrata = nStrata

  pars = setup_Hpar_static(pars, 1, HPop)
  pars = setup_BloodFeeding(pars, 1, 1, BFopts, residence, searchB, F_circadian)
  pars = make_TimeSpent(pars, 1, TimeSpent, TimeSpentOpts)

  # Dynamics
  pars = xde_setup_MYZpar("trace", pars, 1, EIPopts=list(), MYZopts, calK=NULL)

  pars = xde_setup_Xpar(Xname, pars, 1, Xopts)
  pars = setup_Xinits(pars, 1, Xopts)

  pars = make_indices(pars)

  return(pars)
}

#' @title Set up a model for xde_diffeqn_cohort
#' @param F_eir is a function F_eir(t, pars) that returns the daily FoI
#' @param bday the birthday of a cohort
#' @param scale the birthday of a cohort
#' @param modelName is a name for the model (arbitrary)
#' @param Xname is a character string defining a X model
#' @param HPop is the number of humans in each patch
#' @param searchB is a vector of search weights for blood feeding
#' @param Xopts a list to configure the X model
#' @param dlay is either "ode" or "dde"
#' @return a [list]
#' @export
xde_setup_cohort = function(F_eir, bday=0, scale=1,
                           modelName = "unnamed",

                           # Dynamical Components
                           Xname = "SIS",

                           # Model Structure
                           HPop=1000,
                           searchB = 1,

                           # Human Strata / Options
                           Xopts = list(),
                           dlay = 'ode'

){

  pars <- make_xds_object('xde', 'cohort', dlay)
  class(pars$compute) <- "na"

  pars$nVectors = 1
  pars$nHosts = 1
  pars$nPatches = 1

  pars$modelName = modelName
  pars$Xname = Xname

  pars$EIRpar = list()
  pars$EIRpar$bday = bday
  pars$EIRpar$scale = scale
  pars$F_eir = F_eir

  # Structure
  nStrata = length(HPop)
  residence = rep(1, nStrata)

  pars = setup_Hpar_static(pars, 1, HPop)
  pars = setup_BloodFeeding(pars, 1, 1, list(), residence, searchB, NULL)
  pars$BFpar$TimeSpent[[1]] = make_TimeSpent_athome(1, residence)
  pars = make_TaR(0, pars, 1, 1)

  # Dynamics
  pars = xde_setup_Xpar(Xname, pars, 1, Xopts)
  pars = setup_Xinits(pars, 1, Xopts)

  pars = make_indices(pars)

  return(pars)
}
