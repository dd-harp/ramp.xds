# X 

#' @title Make an **`xds`** model object template
#'
#' @description
#' This function is called by [xds_setup] to create a structured
#' **`xds`** model object template. The template has
#' configured interfaces, created stubs for the model objects,
#' set up empty ports, and created junctions. A template has set up all the basic
#' structural elements, but it has not configured
#' any of the component model objects.
#'
#' @details This function sets up the interfaces and the
#' core objects used by **`ramp.xds`**.
#'
#' @note First, the function sets up some
#' short text strings (assigned to the same `S3` class)
#' to dispatch **cases** of `S3` functions:
#'
#' - **`xds`** is either "xde" for differential equations or "dts" for discrete-time systems
#'
#' - **`xde`** is either "ode" for ordinary differential equations, "dde" for delay differential equations, or "dts" for discrete-time systems
#'
#' - **`frame`** is one of several cases:
#'      - "full" includes all three dynamical components: a human/host dynamical component, \eqn{\cal XH}; an adult mosquito dynamical component, \eqn{\cal MYZ}; and an aquatic mosquito dynamical component, \eqn{\cal L}, in some form (possibly the trivial case) (see [xds_setup()])
#'      - "mosy" is for mosquito ecology models (see [xds_setup_mosy()]), including models without pathogen infection dynamics in mosquitoes
#'      - "aquatic" is for aquatic mosquito ecology models (see [xds_setup_aquatic()]), forced by a function describing egg laying
#'      - "human" is for human/host infection dynamics (see [xds_setup_human()]), forced by the infective density of adult mosquitoes, \eqn{fqZ}
#'      - "eir" is for human/host cohort infection dynamics (see [xds_setup_eir()]), forced by a function `F_eir`
#'
#' - **`forcing`** is set to "none"
#'
#' @details Second, the function sets the values of the **structural parameters**:
#' - **`nPatches`** or \eqn{N_p} is the number of patches
#' - **`nHabitats`** or \eqn{N_q}, the number of aquatic habitats, is set to `length(membership)`
#' - **`nStrata`** or \eqn{N_h}, the number of human/host population strata, is set to `length(residence)`
#' - **`nVectorSpecies`** or \eqn{N_s}, the number of vector species is set to 1;
#' - **`nHostSpecies`** or \eqn{N_i}, the number of host species is set to 1;
#'
#' Next, the function sets up egg-laying, blood-feeding, importation, transmission, and exposure.
#' 
#' Next, the function sets up empty lists to hold the model objects for all three dynamical components:
#' - `XH_obj`
#' - `MY_obj`
#' - `L_obj`
#' 
#' Finally, the function sets up some junctions.
#'
#' @note `xds` stands for an eXtensible Dynamical System
#'
#' @param xds selects the solver: `xde` for ordinary or delay differential equations, or `dts` for discrete-time systems.
#' @param frame model component subset
#' @param nPatches is the number of patches
#' @param membership the patch where each aquatic habitat is found
#' @param residence the patch where each stratum resides
#'
#' @keywords internal
#'
#' @return an **`xds`** model object
#'
#' @seealso Related: [xds_setup] and [setup_forcing_object]. Illustrated in a vignette: [5-3-4 Example](https://dd-harp.github.io/ramp.xds/articles/ex_534.html)
#'
#' @export
make_xds_object_template = function(xds='xde', frame='full',
                           nPatches=1, membership=1, residence=1){

  xds_obj = list()
  xds_obj$membership=membership
  xds_obj$residence=residence
  xds_obj$nPatches = nPatches
  xds_obj$nHabitats = length(membership)
  xds_obj$nStrata = length(residence)
  xds_obj$nVectorSpecies  = 1
  xds_obj$nHostSpecies    = 1
  
  class(xds_obj) <- 'xds_obj'

  xds_obj$model_name  <- 'unnamed'

  stopifnot(xds %in% c("xde", "dts"))
  class(xds) <- xds
  xds_obj$xds <- xds
  xds_obj$xde <- xds
  if(xds == "xde"){
    xds_obj$xde = "ode"
    class(xds_obj$xde) = "ode"
  }
  
  xdlst = list()
  class(xdlst) = xds

  frame <- frame
  class(frame) <- frame
  xds_obj$frame <- frame

  forced_by <- "none"
  class(forced_by) <- "none"
  xds_obj$forced_by <- forced_by

  xds_obj$patches = list()

  xds_obj$habitats = make_static_obj()
  
  xds_obj$terms = list() 

  # Egg Laying Interface
  xds_obj <- setup_habitats(xds_obj, membership)
  xds_obj <- setup_egg_laying_interface(xds_obj)
  
  # Blood Feeding Interface 
  xds_obj <- setup_blood_feeding_interface(xds_obj, residence)
  xds_obj <- setup_importation(xds_obj)
  xds_obj <- setup_transmission(xds_obj)
  xds_obj <- setup_exposure_obj(xds_obj)

  xds_obj$XHname       <- 'none'
  xds_obj$XH_obj      <- xdlst
  xds_obj$MYname      <- 'none'
  xds_obj$MY_obj      <- xdlst
  xds_obj$Lname       <- 'none'
  xds_obj$L_obj       <- xdlst
  
  xds_obj$variables = list()
  xds_obj$nOtherVariables = 0
  xds_obj <- setup_other_variables(xds_obj)

  # Junctions
  xds_obj <- setup_resources_object(xds_obj)
  xds_obj <- setup_forcing_object(xds_obj)
  xds_obj <- setup_health_object(xds_obj)
  xds_obj <- setup_vector_control_object(xds_obj)

  xds_obj$outputs = list()
  xds_obj$analysis = list()

  return(xds_obj)
}
