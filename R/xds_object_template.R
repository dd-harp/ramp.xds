# function make_xds_template and supporting function F_H.setup

#' @title Make an **`xds`** model object template
#' 
#' @description 
#' This function is called by [xds_setup] to create 
#' a structured object, called an 
#' **`xds`** model object template. The template has 
#' configured interfaces, created stubs for the model objects, 
#' created junctions, and 
#' set up empty ports. A template has set up all the basic
#' structural elements, but it has not configured 
#' any of the component model objects.
#'  
#' @details This function sets up the interfaces and the 
#' core objects used by **`ramp.xds.`** 
#'
#' First, the function sets up some 
#' short text strings (assigned to the same `S3` class)
#' to dispatch various **cases** of of various `S3` functions:
#' 
#' - **`xds`** is either "xde"  for differential equations, or "dts" for discrete time systems 
#' 
#' - **`xde`** is either "ode"  for ordinary differential equations; "dde" for delay differential equations; or "dts" for discrete time systems 
#' 
#' - **`frame`** is one of several cases:
#'      - "full" includes all three dynamical components: a human/host dynamical component, \eqn{\cal XH}; and adult mosquito dynamical component, \eqn{\cal MYZ}; and an aquatic mosquito dynamical component, \eqn{\cal L}.
#' in some form (possibly the trivial case) (see [xds_setup()])
#'      - "mozy" is for mosquito ecology models (see [xds_setup_mosy()]), including models without pathogen infection dynamics in mosquitoes
#'      - "aquatic" is for aquatic mosquito ecology models (see [xds_setup_aquatic()]), forced by a function describing egg laying
#'      - "human" is for human/host infection dynamics(see [xds_setup_human()]), forced by the infective density of adult mosquitoes, \eqn{fqZ}
#'      - "eir" is for human/host cohort infection dynamics (see [xds_setup_eir()]), forced by a function `F_eir`
#'      - "cohort" is for human/host cohort infection dynamics (see [xds_setup_cohort()]), forced by a function `F_eir`
#' 
#' - **`forcing`** is set to "static"
#'
#' Second, the function sets the values of the **structural parameters**:
#' - **`nPatches`** or \eqn{N_p} is the number of patches
#' - **`nHabitats`** or \eqn{N_q}, the number of aquatic habitats, is set to `length(membership)`
#' - **`nStrata`** or \eqn{N_h}, the number of human/ host population strata, is set to `length(HPop)`
#' - **`nVectorSpecies`** or \eqn{N_s}, the number of vector species is set to 1;
#' - **`nHostSpecies`** or \eqn{N_i}, the number of host species is set to 1;
#'
#' Next, the function sets up empty lists to hold the model objects that define components:
#' - `XH_obj`
#' -
#' - **Transmission**  calls [setup_transmission()] 
#'
#' model for the availability of visitors; by default, there are no visitors
#' Next, the function sets up egg laying, blood feeding, and transmission:
#' - **Egg Laying** calls [make_habitat_matrix()], then [setup_ML_interface()]
#' - **Blood Feeding** calls [make_residency_matrix()], then [setup_XY_interface()]
#' - **Transmission**  calls [setup_transmission()] sets up a static
#' model for the availability of visitors; by default, there are no visitors
#'
#' Finally, the function sets up a few other miscellaneous options:
#' - [Exposure] is called *after* [Transmission] to compute environmentally heterogeneous exposure
#' and malaria importation through travel:
#'      - [setup_exposure] sets up a Poisson model for environmental heterogeneity
#'      - [setup_travel_object] sets up a model with no exposure through travel
#'
#' @note `xds` stands for extensible differential equation
#'
#' @param xds is used to dispatch various functions to set up and solve systems of differential equations. 'xde' for ordinary or delay differential equations; 'dts' for "discrete time systems"
#' @param frame model component subset
#' @param nPatches is the number of patches
#' @param membership is the habitat membership vector
#' @param residency is the strata residency vector
#' 
#' @return an `xds` model object
#' 
#' @seealso Related: [xds_setup] and [setup_forcing_object]. Illustrated in a vignette: [5-3-4 Example](https://dd-harp.github.io/ramp.xds/articles/ex_534.html)
#'
#' @export
make_xds_object_template = function(xds='ode', frame='full',
                           nPatches=1, membership=1, residency=1){
  
  xds_obj = list()
  class(xds_obj) <- 'xds_obj'
  
  xds_obj$model_name  <- 'unnamed'
  
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
  
  xds_obj$nVectorSpecies  = 1
  xds_obj$nHostSpecies    = 1
  xds_obj$nPatches  = nPatches
  xds_obj$nHabitats = length(membership)
  xds_obj$nStrata   = length(residency)
  xds_obj$nOtherVariables = 0 

  xds_obj$terms <- list() 
  
  xds_obj <- setup_ML_interface(xds_obj, membership)
  xds_obj <- setup_habitat_object(xds_obj)
  
  xds_obj <- setup_XY_interface(xds_obj, residency)
  xds_obj <- setup_travel_object(xds_obj)
  xds_obj <- setup_visitor_object(xds_obj)
  xds_obj <- setup_blood_host_object(xds_obj)
  xds_obj <- setup_transmission(xds_obj)
  xds_obj <- setup_exposure("pois", xds_obj) 
  xds_obj$variables = list() 
  
  xds_obj$Xname       <- 'unspecified'
  xds_obj$XH_obj      <- xdlst
  xds_obj$MYname      <- 'unspecified'
  xds_obj$MY_obj      <- xdlst
  xds_obj$Lname       <- 'unspecified'
  xds_obj$L_obj       <- xdlst
  xds_obj             <- setup_other_variables(xds_obj) 

  # Junctions
  xds_obj <- setup_sugar_object(xds_obj)
  xds_obj <- setup_traps_object(xds_obj)
  xds_obj <- setup_forcing_object(xds_obj)
  xds_obj <- setup_health_object(xds_obj)
  xds_obj <- setup_vector_control_object(xds_obj)
  
  xds_obj$outputs = list()

  return(xds_obj)
}
