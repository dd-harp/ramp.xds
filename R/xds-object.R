# function make_xds_object and supporting function F_H.setup

#' @title Make **`xds`** Object
#' @description Create the template for a fully defined **`xds`** object
#' @details This function sets up everything that is required to configure and run a basic model.
#'
#' First, the function sets up some short text strings (assigned to the same `S3` class)
#' to dispatch various **cases** of of various `S3` functions:
#' - *`xds`* is either "xde" for differential equations or "dts" for difference equations;
#' - *`frame`* is one of several cases:
#'      - "full" includes all three dynamical components: a human/host dynamical component, \eqn{\cal XH}; and adult mosquito dynamical component, \eqn{\cal MYZ}; and an aquatic mosquito dynamical component, and \eqn{\cal L}.
#' in some form (possible the trace case) (see [xds_setup()])
#'      - "mozy" is for mosquito ecology models (see [xds_setup_mosy()]), including models without pathogen infection dynamics in mosquitoes
#'      - "aquatic" is for aquatic mosquito ecology models (see [xds_setup_mosy()]), forced by a function describing egg laying
#'      - "human" is for human/host infection dynamics(see [xds_setup_human()]), forced by the infective density of adult mosquitoes, \eqn{fqZ}
#'      - "cohort" is for human/host cohort infection dynamics (see [xds_setup_cohort()]), forced by a function `F_eir`
#' - *`dlay`* is either "ode" or "dde" and it only affects dispatching for differential equations
#' - *`forcing`* is set to "static"
#' - *`compute`* dispatches various cases of [compute_terms()] linked to `frame`
#'
#' Next, the function sets the basic **structural parameters**:
#' - *`nVectors`* or \eqn{n_s}, the number of vector species is set to 1;
#' - *`nHosts`* or \eqn{n_i}, the number of host species is set to 1;
#' - *`nPatches`* or \eqn{n_p} is the number of patches
#' - *`nHabitats`* or \eqn{n_q} is the number of aquatic habitats is `length(membership)`
#' - *`nStrata`* or \eqn{n_q} is the number of human population strata is `length(HPop)`
#'
#' Next, the function sets up egg laying, blood feeding, and transmission:
#' - **Egg Laying** is set up by calling [create_habitat_matrix()] and then [setup_EGG_LAYING()]
#' - **Blood Feeding** is set up by calling [create_residence_matrix()]and then calls [setup_BLOOD_FEEDING()]:
#' - **Transmission**  is set up by calling [setup_TRANSMISSION()] and
#' [setup_visitors_static()] sets up a static model for the availability of visitors, with no visitors
#'
#' Finally, the function sets up a few other miscellaneous options:
#' - [Exposure] is called *after* [Transmission] to compute environmentally heterogeneous exposure
#' and malaria importation through travel:
#'      - [setup_exposure_pois()] sets up a Poisson model for environmental heterogeneity
#'      - [setup_travel_static()] sets up a model with no exposure through travel
#'
#' @param xds is either "xde" or "dts" for differential / difference equations
#' @param frame model component subset
#' @param dlay is either "ode" for ordinary and "dde" for delay differential equations
#' @param nPatches is the number of patches
#' @param membership is the habitat membership vector
#' @param residence is the strata residence vector
#' @return an `xds` template as a compound [list]
#' @seealso [xds_setup()]
#' @export
make_xds_object = function(xds='xde', frame='full', dlay = 'ode',
                           nPatches=1, membership=1, residence=1){
  pars = list()
  class(pars) <- 'xds_obj'
  xds <- xds
  class(xds) <- xds
  pars$xds <- xds
  xdlst = list()
  class(xdlst) = xds

  frame <- frame
  class(frame) <- frame
  pars$frame <- frame

  dlay <- dlay
  class(dlay) <- dlay
  pars$dlay <- dlay

  forcing <- 'static'
  class(forcing) <- 'static'
  pars$forcing <- forcing

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

  pars <- setup_exposure_pois(pars)
  pars <- setup_travel_static(pars)
  pars <- setup_visitors_static(pars)

  pars$Linits = list()
  pars$MYZinits = list()
  pars$Xinits = list()

  pars$ix = list()
  pars$ix$X = list()
  pars$ix$MYZ = list()
  pars$ix$L = list()

  pars$outputs = list()
  pars$compute = list()

  pars <- setup_vc_no_control(pars)

  return(pars)
}
