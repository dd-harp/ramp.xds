# function make_xds_object and supporting function F_H.setup

#' @title Make an `xds` object
#' @param xds is `xde`/`dts` for differential / difference equations
#' @param frame model component subset
#' @param dlay is `ode` for ordinary and `dde` for delay
#' @param nPatches is the number of patches
#' @param membership is the habitat membership vector
#' @param residence is the strata residence vector
#' @param HPop is the initial human population density
#' @return an unspecified `xds` object as a compound [list]
#' @export
make_xds_object = function(xds='xde', frame='full', dlay = 'ode',
                           nPatches=1, membership=1, residence=1, HPop=1000){
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
  pars$Xname       <- 'na'
  pars$Xpar        <- xdlst
  pars$Hpar        <- list()
  pars$MYZname     <- 'na'
  pars$MYZpar      <- xdlst
  pars$Lname       <- 'na'
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

  stopifnot(length(HPop) == length(residence))
  pars$Xpar[[1]] <- list()
  class(pars$Xpar[[1]]) <- 'xds_obj'
  pars$Xpar[[1]]$H0 = HPop

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

  pars$Xpar[[1]] <- list()

  return(pars)
}

#' @title Get the human population size
#' @description This makes the initial human population sizes available
#' during [make_xds_object]
#' @note This method dispatches on the type of `pars$Xpar[[i]]`
#' @inheritParams F_H
#' @return a [numeric] vector of length `nStrata`
#' @export
F_H.xds_obj <- function(y, pars, i) {
  return(pars$Xpar[[1]]$H0)
}
