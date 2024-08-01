
#' @title Change the time spent matrix
#' @param calK a mosquito dispersal [matrix]
#' @param pars a [list]
#' @param s the vector species index
#' @return a [list]
#' @export
change_calK = function(calK, pars, s=1){
  stopifnot(dim(calK) == dim(pars$MYZpar[[s]]$baseline$calK))
  pars$MYZpar[[s]]$baseline$calK <- calK
  pars <- make_Omega(pars, s)
  y0 = as.vector(unlist(get_inits(pars)))
  pars <- update_MYZinits(pars, y0, 1)
  return(pars)
}

#' @title Make a mosquito dispersal matrix, called calK
#' @param calK a matrix or a string
#' @param s the vector species index
#' @param pars an `xds` object
#' @param opts a list of options to configure calK
#' @return a modified `xds` object
#' @export
make_calK = function(calK, s, pars, opts = list()){
  if(is.matrix(calK)) class(opts) <- "as_matrix"
  if(is.character(calK)) class(opts)<- calK
  UseMethod("make_calK", opts)
}

#' @title Dispersal to every other patch, with equal probability
#' @description Implements [make_calK] for the herethere model
#' @inheritParams make_calK
#' @return a [matrix]
#' @export
make_calK.herethere = function(calK, s, pars, opts = list()){
  create_calK_herethere(pars$nPatches)
  change_calK(calK, pars, s)
}

#' @title Develop a mosquito dispersal matrix from a kernel and xy-coordinates
#' @param nPatches is the number of patches in the model
#' @export
create_calK_herethere = function(nPatches) {
  calK <- matrix(1/(nPatches-1), nPatches, nPatches)
  diag(calK) <- 0
  return(calK)
}

#' @title Pass a pre-configured calK
#' @description Implements [make_calK] for as_matrix
#' @inheritParams make_calK
#' @return a [matrix]
#' @export
make_calK.as_matrix = function(calK, s, pars, opts=list()){
  change_calK(calK, pars, s)
}

#' @title Develop a mosquito dispersal matrix from a kernel and xy-coordinates
#' @description Implements [make_calK] for kernels
#' @inheritParams make_calK
#' @return a [matrix]
#' @export
make_calK.xy = function(calK = "xy", s, pars, opts=list()) {
  calK <- with(opts, create_calK_xy(xy, ker))
  change_calK(calK, pars, s)
}

#' @title Develop a mosquito dispersal matrix from a kernel and xy-coordinates
#' @param xy is a vector of the xy-coordinates of patch locations
#' @param ker is a function that weights putative locations by distance
#' @export
create_calK_xy = function(xy, ker) {
  dmat <- as.matrix(stats::dist(xy), upper=T)
  calK <- ker(dmat)
  diag(calK) <- 0
  calK = calK %*% diag(1/rowSums(calK))
  return(calK)
}
