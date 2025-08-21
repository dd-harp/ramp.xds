# function make_xds_template and supporting function F_H.setup

#' @title Make an **`xds`** model object Template
#' @description Creates and returns structured template for an
#' **`xds`** model object. 
#' The returned model object has set up generic features
#' and placeholders for options that must be configured
#' to fully define an **`xds`** model object
#' @details This function sets up the basic structures required
#' to configure and run a basic model. The returned model object is a list
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
#' - **`nVectorSpecies`** or \eqn{n_s}, the number of vector species is set to 1;
#' - **`nHostSpecies`** or \eqn{n_i}, the number of host species is set to 1;
#' - **`nPatches`** or \eqn{n_p} is the number of patches
#' - **`nHabitats`** or \eqn{n_q}, the number of aquatic habitats, is set to `length(membership)`
#' - **`nStrata`** or \eqn{n_h}, the number of human/ host population strata, is set to `length(HPop)`
#'
#' Next, the function sets up empty lists to hold the model objects that define components:
#' - `Xpar`
#' -
#' - **Transmission**  calls [setup_TRANSMISSION()] 
#'
#' model for the availability of visitors; by default, there are no visitors
#' Next, the function sets up egg laying, blood feeding, and transmission:
#' - **Egg Laying** calls [create_habitat_matrix()], then [setup_EGG_LAYING()]
#' - **Blood Feeding** calls [create_residence_matrix()], then [setup_BLOOD_FEEDING()]
#' - **Transmission**  calls [setup_TRANSMISSION()] sets up a static
#' model for the availability of visitors; by default, there are no visitors
#'
#' Finally, the function sets up a few other miscellaneous options:
#' - [Exposure] is called *after* [Transmission] to compute environmentally heterogeneous exposure
#' and malaria importation through travel:
#'      - [setup_exposure_pois] sets up a Poisson model for environmental heterogeneity
#'      - [setup_no_travel] sets up a model with no exposure through travel
#'
#' @note `xds` stands for extensible differential equation
#'
#' @param xds is used to dispatch various functions to set up and solve systems of differential equations. 'xde' for ordinary or delay differential equations; 'dts' for "discrete time systems"
#' @param frame model component subset
#' @param nPatches is the number of patches
#' @param membership is the habitat membership vector
#' @param residence is the strata residence vector
#' @return an `xds` model object
#' @seealso Related: [xds_setup] and [setup_no_forcing]. Illustrated in a vignette: [5-3-4 Example](https://dd-harp.github.io/ramp.xds/articles/ex_534.html)
#'
#' @export
make_xds_object_template = function(xds='ode', frame='full',
                           nPatches=1, membership=1, residence=1){
  
  xds_obj = list()
  class(xds_obj) <- 'xds_obj'
  
  stopifnot(xds %in% c("ode", "dde", "dts"))
 
  xde <- xds
  class(xde) <- xds  
  xds_obj$xde <- xde
  
  if(xds == 'ode') class(xds) <- 'xde'
  if(xds == 'dde') class(xds) <- 'xde'
  if(xds == 'dts') class(xds) <- 'dts'
  xds_obj$xds <- xds
  
  xdlst = list()
  class(xdlst) = xds

  frame <- frame
  class(frame) <- frame
  xds_obj$frame <- frame
 
  forced_by <- "none" 
  class(forced_by) <- "none" 
  xds_obj$forced_by <- forced_by 

  xds_obj$compute <- 'frame'

  xds_obj$model_name  <- 'unnamed'
  xds_obj$Xname       <- 'unspecified'
  xds_obj$Xpar        <- xdlst
  xds_obj$Hpar        <- list()
  xds_obj$MYZname     <- 'unspecified'
  xds_obj$MYZpar      <- xdlst
  xds_obj$Lname       <- 'unspecified'
  xds_obj$Lpar        <- xdlst
  xds_obj             <- setup_other_variables(xds_obj) 

  vars <- list()
  class(vars) <- 'static'
  xds_obj$vars = vars

  xds_obj$nVectorSpecies  = 1
  xds_obj$nHostSpecies    = 1
  xds_obj$nPatches  = nPatches
  xds_obj$nHabitats = length(membership)
  xds_obj$habitat_matrix = create_habitat_matrix(nPatches, membership)

  xds_obj$nStrata   = length(residence)
  xds_obj$residence_matrix = list()
  xds_obj$residence_matrix[[1]] = create_residence_matrix(nPatches, residence)

  xds_obj$Lambda = list()
  xds_obj <- setup_EGG_LAYING(xds_obj)
  xds_obj <- setup_BLOOD_FEEDING(xds_obj)
  xds_obj <- setup_TRANSMISSION(xds_obj)

  xds_obj   <- setup_exposure_pois(xds_obj)
  xds_obj$travel <- list()
  xds_obj <- setup_no_travel(xds_obj, 1)
  xds_obj$travel_eir <- list()
  xds_obj <- setup_travel_eir(xds_obj, 1)

  xds_obj$Linits = list()
  xds_obj$MYZinits = list()
  xds_obj$Xinits = list()

  xds_obj$ix = list()
  xds_obj$ix$X = list()
  xds_obj$ix$MYZ = list()
  xds_obj$ix$L = list()

  xds_obj$outputs = list()
  xds_obj$compute = list()

  xds_obj <- setup_no_forcing(xds_obj)
  xds_obj <- setup_no_health(xds_obj)
  xds_obj <- setup_no_vector_control(xds_obj)
  xds_obj <- setup_resources_static(xds_obj)

  return(xds_obj)
}

#' @title Set `xds` to `dde`
#' @description Creates and returns structured template for an
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
xds_dde = function(xds_obj){
  UseMethod("xds_dde", xds_obj$xde)
}

#' @title Set `xds` for `dde`
#' @description Do not change `xds`
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
xds_dde.dde = function(xds_obj){
  return(xds_obj)
}

#' @title Set `xds` for `dts`
#' @description Do not change `xds`
#' @param xds_obj a **`ramp.xds`** modelobject
#' @return a **`ramp.xds`** model object
#' @export
xds_dde.dts = function(xds_obj){
  return(xds_obj)
}

#' @title Set `xds` to `dde`
#' @description Change `xds` from `ode` to `dde`
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
xds_dde.ode = function(xds_obj){
  xds_obj$xde = 'dde'
  class(xds_obj$xde) ='dde'
  return(xds_obj)
}
