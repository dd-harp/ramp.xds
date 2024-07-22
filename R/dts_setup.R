# functions to set up models


#' @title Make base parameters, assuming nVectors = nHosts = 1
#' @return a [list]
#' @export
make_parameters_dts = function(){
  pars = list()
  class(pars) <- "dts"

  pars$xde = "dts"
  class(pars$xde) = "dts"

  dts_list = list()
  class(dts_list) <- "dts"

  pars$MYZpar = dts_list
  pars$Lpar = dts_list
  pars$Xpar = dts_list
  pars$Hpar = dts_list
  pars$vars = list()

  pars$Lambda = list()
  pars <- setup_EGG_LAYING(pars)
  pars <- setup_BFpar_static(pars)

  pars$Linits = list()
  pars$MYZinits = list()
  pars$Xinits = list()

  pars$ix = list()
  pars$ix$X = list()
  pars$ix$MYZ = list()
  pars$ix$L = list()


  pars$outputs = list()
  pars$compute = list()

  pars$HostAvailability = list()

  pars <- setup_abiotic_null(pars)
  pars <- setup_shock_null(pars)
  pars <- setup_control_null(pars)
  pars <- setup_vc_null(pars)
  pars <- setup_behavior_null(pars)
  pars <- setup_habitat_dynamics_static(pars)
  pars <- setup_bionomics_static(pars)
  pars <- setup_visitors_static(pars)
  pars <- setup_resources_null(pars)
  pars <- setup_travel_static(pars)

  pars <- dts_setup_exposure_pois(pars)
  pars$AR = list()

  return(pars)
}

#' @title Set up a model for dts_diffeqn
#' @param modelName is a name for the model (arbitrary)
#' @param MYZname is a character string defining a MYZ model
#' @param Xname is a character string defining a X model
#' @param Lname is a character string defining a L model
#' @param Xday is the run-time time step for X component (in days): integer or 1/integer
#' @param MYZday is the run-time time step for MYZ component (in days): integer or 1/integer
#' @param Lday is the run-time time step for L component (in days): integer or 1/integer
#' @param nPatches is the number of patches
#' @param nVectors is the number of vector species
#' @param nHosts is the number of vertebrate host species
#' @param HPop is the number of humans in each patch
#' @param membership is a vector that describes the patch where each aquatic habitat is found
#' @param MYZopts a list to configure the MYZ model
#' @param EIPopts a list with options to setup EIPmod; must include EIPname
#' @param calK is either a calK matrix or a string that defines how to set it up
#' @param calKopts are the options to setup calK
#' @param Xopts a list to configure the X model
#' @param BFopts a list to configure the blood feeding model
#' @param residence is a vector that describes the patch where each human stratum lives
#' @param searchB is a vector of search weights for blood feeding
#' @param F_circadian is a function describing mosquito daily activity
#' @param TimeSpent is either a TimeSpent matrix or a string to call a function that sets it up
#' @param TimeSpentOpts are the options to setup TimeSpent
#' @param searchQ is a vector of search weights for egg laying
#' @param Lopts a list to configure the L model
#' @return a [list]
#' @export
dts_setup = function(modelName = "unnamed",

                     # Dynamical Components
                     MYZname = "RM_dts",
                     Xname = "SIS",
                     Lname = "trace",

                     # Runtime Time parameters
                     Xday = 1,
                     MYZday = 1,
                     Lday = 1,

                     # Model Structure
                     nPatches = 1,
                     nVectors = 1,
                     nHosts = 1,
                     HPop=1000,
                     membership=1,

                     # Adult Mosquito Options
                     MYZopts = list(),
                     EIPopts = list(EIPname = "fixedlag_dts", eip=12),
                     calK ="herethere",
                     calKopts = list(),

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
                     Lopts = list()

){
  pars <- make_xds_object('dts', 'full')
  class(pars$compute) = "dts"

#  pars$frame <- "full"
#  class(pars$frame) <- "full"

  pars$modelName = modelName
  pars$Xname = Xname
  pars$MYZname = MYZname
  pars$Lname = Lname

  pars$Dday = set_Dday(Xday, MYZday, Lday, Lname)
  pars$Lday = Lday
  pars$MYZday = MYZday
  pars$Xday = Xday

  # Fixed Structural Elements
  pars$nPatches = nPatches
  pars$nVectors = nVectors
  pars$nHosts = nHosts
  pars$nHabitats = length(membership)
  pars$membership = membership
  pars$habitat_matrix = create_habitat_matrix(pars$nPatches, pars$membership)

  # Adult Mosquito Dynamics
  calK = make_calK(nPatches, calK, calKopts)
  pars = dts_setup_MYZpar(MYZname, pars, 1, EIPopts, MYZopts, calK)
  pars = setup_MYZinits(pars, 1, MYZopts)

  # Human Demography
  pars = setup_Hpar_static(pars, 1, HPop)
  # Blood Feeding
  pars = setup_BloodFeeding(pars, 1, 1, BFopts, residence, searchB, F_circadian)
  pars = make_TimeSpent(pars, 1, TimeSpent, TimeSpentOpts)
  # Vertebrate Host Dynamics
  pars = dts_setup_Xpar(Xname, pars, 1, Xopts)
  pars = setup_Xinits(pars, 1, Xopts)

  # Aquatic Mosquito Dynamics
  pars = dts_setup_Lpar(Lname, pars, 1, Lopts)
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

#' @title Set up a model for dts_diffeqn_mosy
#' @param modelName is a name for the model (arbitrary)
#' @param MYZname is a character string defining a MYZ model
#' @param Lname is a character string defining a L model
#' @param MYZday is an integer: positive > 1 for lumping; negative < -1 for splitting
#' @param Lday is an integer: positive > 1 for lumping; negative < -1 for splitting'
#' @param nPatches is the number of patches
#' @param nVectors is the number of vector species
#' @param membership is a vector that describes the patch where each aquatic habitat is found
#' @param MYZopts a list to configure the MYZ model
#' @param calK is either a calK matrix or a string that defines how to set it up
#' @param calKopts are the options to setup calK
#' @param searchQ is a vector of search weights for egg laying
#' @param Lopts a list to configure the L model
#' @param kappa values -- net infectivity to force adult infection dynamics
#' @return a [list]
#' @export
dts_setup_mosy = function(modelName = "unnamed",

                          # Dynamical Components
                          MYZname = "basicM",
                          Lname = "basic",
                          MYZday = 1,
                          Lday = 1,


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
                          kappa=NULL
){

  pars <- make_xds_object("dts", "mosy")
  class(pars$compute) = "na"

  pars$frame <- "mosy"
  class(pars$frame) <- "mosy"

  pars$modelName = modelName
  pars$MYZname = MYZname
  pars$Lname = Lname

  pars$Lday = Lday
  pars$Lsplit = ifelse(Lday<1, as.integer(1/Lday), 1)
  pars$MYZday = MYZday
  pars$MYZsplit = ifelse(MYZday<1, as.integer(1/MYZday), 1)

  # Structure
  pars$nPatches = nPatches
  pars$nHabitats = length(membership)
  pars$membership = membership
  pars$habitat_matrix = create_habitat_matrix(pars$nPatches, pars$membership)
  pars$nVectors = 1

  # Dynamics
  calK = make_calK(nPatches, calK, calKopts)
  pars = dts_setup_MYZpar(MYZname, pars, 1, EIPopts=list(), MYZopts, calK)
  pars = setup_MYZinits(pars, 1, MYZopts)

  # Aquatic Mosquito Dynamics
  pars = dts_setup_Lpar(Lname, pars, 1, Lopts)
  pars = setup_Linits(pars, 1, Lopts)
  pars = setup_egg_laying_static(pars, searchQ, 1, Lopts)

  if(is.null(kappa))  kappa = rep(0, nPatches)
  pars$kappa[[1]] = checkIt(kappa, nPatches)

  pars = make_indices(pars)

  return(pars)
}


#' @title Set up a model for dts_diffeqn_aqua
#' @param modelName is a name for the model (arbitrary)
#' @param nHabitats is the number of habitats
#' @param nVectors is the number of vector species
#' @param Lname is a character string defining a L model
#' @param Lday is an integer: positive > 1 for lumping; negative < -1 for splitting'
#' @param Lopts a list to configure the L model
#' @param MYZopts a list to configure F_eggs from the Gtrace model
#' @param LSMname is a character string defining a LSM model
#' @return a [list]
#' @export
dts_setup_aquatic = function(modelName = "unnamed",
                             nHabitats = 1,
                             nVectors = 1,
                             Lname = "basic",
                             Lday = 1,
                             Lopts = list(),
                             MYZopts = list(),
                             LSMname = "null"){

  pars <- make_xds_object("dts", "aquatic")
  class(pars$compute) = "na"

  pars$modelName = modelName
  pars$MYZname = "Gtrace"
  pars$Lname = Lname
  pars$Lday = Lday
  pars$Lsplit = ifelse(Lday<1, as.integer(1/Lday), 1)

  pars$nVectors = nVectors
  pars = dts_setup_MYZpar("Gtrace", pars, 1, MYZopts, "null", calK=NULL)

  pars$nHabitats = nHabitats
  membership = 1:nHabitats
  pars$membership = membership
  pars$habitat_matrix = create_habitat_matrix(pars$nHabitats, pars$membership)
  searchQ = rep(1, nHabitats)
  pars = dts_setup_Lpar(Lname, pars, 1, Lopts)
  pars = setup_Linits(pars, 1, Lopts)

  pars <- setup_lsm_null(pars)

  pars = make_indices(pars)


  return(pars)
}


#' @title Set up a model for dts_diffeqn_human
#' @param modelName is a name for the model (arbitrary)
#' @param Xname is a character string defining a X model
#' @param Xday is an integer: positive > 1 for lumping; negative < -1 for splitting
#' @param HPop is the number of humans in each patch
#' @param MYZopts a list to configure the MYZ model
#' @param Xopts a list to configure the X model
#' @param BFopts a list to configure the blood feeding model
#' @param residence is a vector that describes the patch where each human stratum lives
#' @param searchB is a vector of search weights for blood feeding
#' @param F_circadian is a function describing mosquito daily activity
#' @param TimeSpent is either a TimeSpent matrix or a string to call a function that sets it up
#' @param TimeSpentOpts are the options to setup TimeSpent
#' @return a [list]
#' @export
dts_setup_human = function(modelName = "unnamed",

                           # Dynamical Components
                           Xname = "SIS",
                           Xday = 1,

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
                           TimeSpentOpts=list()

){

  pars <- make_xds_object('dts', 'human')
  class(pars$compute) = "na"


  pars$modelName = modelName
  pars$Xname = Xname
  pars$MYZname = "Ztrace"
  pars$Xday = Xday

  # Structure
  nStrata = length(HPop)
  pars$nPatches = as.integer(nStrata)
  pars$nStrata = nStrata

  pars = setup_Hpar_static(pars, 1, HPop)
  pars = setup_BloodFeeding(pars, 1, 1, BFopts, residence, searchB, F_circadian)
  pars = make_TimeSpent(pars, 1, TimeSpent, TimeSpentOpts)

  # Dynamics
  pars = dts_setup_MYZpar("Ztrace", pars, 1, MYZopts, "null", calK=NULL)

  pars = dts_setup_Xpar(Xname, pars, 1, Xopts)
  pars = setup_Xinits(pars, 1, Xopts)

  pars = make_indices(pars)

  return(pars)
}

#' @title Set up a model for dts_diffeqn_cohort
#' @param F_eir is a function F_eir(t, pars) that returns the daily FoI
#' @param bday the birthday of a cohort
#' @param scale the birthday of a cohort
#' @param modelName is a name for the model (arbitrary)
#' @param Xname is a character string defining a X model
#' @param Xday is an integer: positive > 1 for lumping; negative < -1 for splitting
#' @param HPop is the number of humans in each patch
#' @param searchB is a vector of search weights for blood feeding
#' @param Xopts a list to configure the X model
#' @return a [list]
#' @export
dts_setup_cohort = function(F_eir, bday=0, scale=1,
                            modelName = "unnamed",

                            # Dynamical Components
                            Xname = "SIS",
                            Xday = 1,

                            # Model Structure
                            HPop=1000,
                            searchB = 1,

                            # Human Strata / Options
                            Xopts = list()

){
  pars <- make_xds_object('dts', 'cohort')
  class(pars$compute) = "cohort"

  pars$nVectors = 1
  pars$nHosts = 1
  pars$nPatches = 1

  pars$modelName = modelName
  pars$Xname = Xname
  pars$Xday = Xday
  pars$F_eir = F_eir
  pars$EIRpar = list()
  pars$EIRpar$bday = bday
  pars$EIRpar$scale = scale

  # Structure
  nStrata = length(HPop)
  residence = rep(1, nStrata)

  pars = setup_Hpar_static(pars, 1, HPop)
  pars = setup_BloodFeeding(pars, 1, 1, list(), residence, searchB, NULL)
  pars$BFpar$TimeSpent[[1]] = make_TimeSpent_athome(1, residence)
  pars = make_TaR(0, pars, 1, 1)

  # Dynamics
  pars = dts_setup_Xpar(Xname, pars, 1, Xopts)
  pars = setup_Xinits(pars, 1, Xopts)

  pars = make_indices(pars)

  return(pars)
}
