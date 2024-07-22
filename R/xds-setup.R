
#' @title Make an `xds` object
#' @param xds is `xde` for differential equations; or `dts` for discrete-time systems
#' @param frame the equation frame / subset of dynamical components in the model
#' @return an unspecified `xds` object as a compound [list]
#' @export
make_xds_object = function(xds='xde', frame='full'){
  pars = list()

  xds <- xds
  class(xds) <- xds
  pars$xds <- xds

  frame <- frame
  class(frame) <- frame
  pars$frame <- frame

  dlay <- 'ode'
  class(dlay) <- 'ode'
  pars$dlay <- dlay

  forcing <- 'static'
  class(forcing) <- 'static'
  pars$forcing <- forcing

  pars$compute <- 'frame'

  pars$modelName <- 'not_named'
  pars$Xname <- 'none'
  pars$MYZname <- 'none'
  pars$Lname <- 'none'

  pars$nVectors  = 1
  pars$nHosts    = 1
  pars$nPatches  = 1
  pars$nStrata   = 1
  pars$nHabitats = 1

  lst = list()
  class(lst) = xds

  pars$MYZpar = lst
  pars$Lpar   = lst
  pars$Xpar   = lst
  pars$Hpar   = lst

  pars$Linits = list()
  pars$MYZinits = list()
  pars$Xinits = list()

  pars$ix = list()
  pars$ix$X = list()
  pars$ix$MYZ = list()
  pars$ix$L = list()

  vars <- list()
  class(vars) <- 'static'
  pars$vars = vars

  pars$Lambda = list()
  pars <- setup_EGG_LAYING(pars)
  pars <- setup_BFpar_static(pars)
  pars <- setup_exposure_pois(pars)
  pars <- setup_travel_static(pars)
  pars <- setup_visitors_static(pars)



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
  pars <- setup_resources_null(pars)

  return(pars)
}
