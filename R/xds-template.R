# function make_xds_template and supporting function F_H.setup

#' @title Make an **`xds`** Object Template
#' @description Creates and returns structured template for an
#' **`xds`** object. The returned object has set up generic features
#' and placeholders for options that must be configured
#' to fully define an **`xds`** object
#' @details This function sets up the basic structures required
#' to configure and run a basic model. The returned object is a list
#' with various required elements attached and configured, but without
#' specifying the dynamical components or any advanced features.
#'
#' First, the function sets up some short text strings (assigned to the same `S3` class)
#' to dispatch various **cases** of of various `S3` functions:
#' - **`xds`** is either "ode" or "dde" for ordinary / delay differential equations; or "dts" for difference equations;
#' - **`frame`** is one of several cases:
#'      - "full" includes all three dynamical components: a human/host dynamical component, \eqn{\cal XH}; and adult mosquito dynamical component, \eqn{\cal MYZ}; and an aquatic mosquito dynamical component, \eqn{\cal L}.
#' in some form (possibly the trivial case) (see [xds_setup()])
#'      - "mozy" is for mosquito ecology models (see [xds_setup_mosy()]), including models without pathogen infection dynamics in mosquitoes
#'      - "aquatic" is for aquatic mosquito ecology models (see [xds_setup_aquatic()]), forced by a function describing egg laying
#'      - "human" is for human/host infection dynamics(see [xds_setup_human()]), forced by the infective density of adult mosquitoes, \eqn{fqZ}
#'      - "cohort" is for human/host cohort infection dynamics (see [xds_setup_cohort()]), forced by a function `F_eir`
#' - **`forcing`** is set to "static"
#'
#' Second, the function sets the values of the **structural parameters**:
#' - **`nVectors`** or \eqn{n_s}, the number of vector species is set to 1;
#' - **`nHosts`** or \eqn{n_i}, the number of host species is set to 1;
#' - **`nPatches`** or \eqn{n_p} is the number of patches
#' - **`nHabitats`** or \eqn{n_q}, the number of aquatic habitats, is set to `length(membership)`
#' - **`nStrata`** or \eqn{n_h}, the number of human/ host population strata, is set to `length(HPop)`
#'
#' Next, the function sets up empty lists to hold the objects that define components:
#' - `Xpar`
#' -
#' - **Transmission**  calls [setup_TRANSMISSION()] and [setup_visitors_static()] sets up a static
#'
#' model for the availability of visitors; by default, there are no visitors
#' Next, the function sets up egg laying, blood feeding, and transmission:
#' - **Egg Laying** calls [create_habitat_matrix()], then [setup_EGG_LAYING()]
#' - **Blood Feeding** calls [create_residence_matrix()], then [setup_BLOOD_FEEDING()]
#' - **Transmission**  calls [setup_TRANSMISSION()] and [setup_visitors_static()] sets up a static
#' model for the availability of visitors; by default, there are no visitors
#'
#' Finally, the function sets up a few other miscellaneous options:
#' - [Exposure] is called *after* [Transmission] to compute environmentally heterogeneous exposure
#' and malaria importation through travel:
#'      - [setup_exposure_pois] sets up a Poisson model for environmental heterogeneity
#'      - [setup_travel_static] sets up a model with no exposure through travel
#'
#' @param xds is used to dispatch various functions to set up and solve systems of differential equations. 'xde' for ordinary or delay differential equations; 'dts' for "discrete time systems"
#' @param frame model component subset
#' @param nPatches is the number of patches
#' @param membership is the habitat membership vector
#' @param residence is the strata residence vector
#' @return an `xds` object
#' @seealso Related: [xds_setup] and [setup_no_forcing]. Illustrated in a vignette: [5-3-4 Example](https://dd-harp.github.io/ramp.xds/articles/ex_534.html)
#'
#' @export
make_xds_template = function(xds='ode', frame='full',
                           nPatches=1, membership=1, residence=1){
  pars = list()
  class(pars) <- 'xds_obj'
  xds <- xds
  if(xds == 'ode' | xds == 'dde') xds <- c(xds, 'xde')
  class(xds) <- xds
  pars$xds <- xds
  xdlst = list()
  class(xdlst) = xds

  frame <- frame
  class(frame) <- frame
  pars$frame <- frame


  pars$compute <- 'frame'

  pars$model_name  <- 'unnamed'
  pars$Xname       <- 'unspecified'
  pars$Xpar        <- xdlst
  pars$Hpar        <- list()
  pars$MYZname     <- 'unspecified'
  pars$MYZpar      <- xdlst
  pars$Lname       <- 'unspecified'
  pars$Lpar        <- xdlst

  vars <- list()
  class(vars) <- 'static'
  pars$vars = vars

  pars$nVectors  = 1
  pars$nHosts    = 1
  pars$nPatches  = nPatches
  pars$nHabitats = length(membership)
  pars$habitat_matrix = create_habitat_matrix(nPatches, membership)

  pars$nStrata   = length(residence)
  pars$residence_matrix = list()
  pars$residence_matrix[[1]] = create_residence_matrix(nPatches, residence)

  pars$Lambda = list()
  pars <- setup_EGG_LAYING(pars)
  pars <- setup_BLOOD_FEEDING(pars)
  pars <- setup_TRANSMISSION(pars)

  pars   <- setup_exposure_pois(pars)
  pars    <- setup_travel_static(pars)

  pars$Linits = list()
  pars$MYZinits = list()
  pars$Xinits = list()

  pars$ix = list()
  pars$ix$X = list()
  pars$ix$MYZ = list()
  pars$ix$L = list()

  pars$outputs = list()
  pars$compute = list()

  pars <- setup_no_forcing(pars)
  pars <- setup_no_health(pars)
  pars <- setup_no_vector_control(pars)
  pars <- setup_resources_static(pars)

  return(pars)
}

#' @title Set `xds` to `dde`
#' @description Creates and returns structured template for an
#' @param pars a **`ramp.xds`** object
#' @return a **`ramp.xds`** object
xds_dde = function(pars){
  UseMethod("xds_dde", pars$xds)
}

#' @title Set `xds` for `dde`
#' @description Do not change `xds`
#' @param pars a **`ramp.xds`** object
#' @return a **`ramp.xds`** object
xds_dde.dde = function(pars){
  return(pars)
}

#' @title Set `xds` for `dts`
#' @description Do not change `xds`
#' @param pars a **`ramp.xds`** object
#' @return a **`ramp.xds`** object
xds_dde.dts = function(pars){
  return(pars)
}

#' @title Set `xds` to `dde`
#' @description Change `xds` from `ode` to `dde`
#' @param pars a **`ramp.xds`** object
#' @return a **`ramp.xds`** object
xds_dde.ode = function(pars){
  pars$xds = 'dde'
  class(pars$xds) = c('dde', 'xde')
  return(pars)
}
