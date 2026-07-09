
#' @title Make the VC Matrix
#' 
#' @description Compute the \eqn{N_p \times N_p} 
#' matrix \eqn{\mathcal{V}} whose columns describe
#' the number of infective bites arising in each patch 
#' from all the mosquitoes biting a single human on a
#' single day in each patch.
#' 
#' @param xds_obj a **`ramp.xds`** model object  
#' @param s the vector species index
#' 
#' @importFrom MASS ginv
#' @importFrom expm expm
#' @return a numeric [matrix]
#' @keywords internal
#' 
#' @export
make_VC.SI <- function(xds_obj, s=1){
  make_VC_gem(xds_obj, s)
}


#' @title Initialize VC 
#' 
#' @description To compute vectorial capacity, we
#' need to  
#' 
#' @param xds_obj a **`ramp.xds`** model object  
#' @param i the vector species index
#' 
#' @importFrom MASS ginv
#' @importFrom expm expm
#' @return a **`ramp.xds`** model object  
#' @keywords internal
#' @export
setup_V_ix.SI <- function(xds_obj, i){with(xds_obj,{
  
  YY_ix <- seq(from = max_ix+1, length.out=nPatches^2)
  max_ix <- tail(YY_ix, 1)
  
  VC_ix <- seq(from = max_ix+1, length.out=nPatches^2)
  max_ix <- tail(VC_ix, 1)
  
  xds_obj$V_obj[[i]]$ix = list(YY_ix=YY_ix, VC_ix=VC_ix)
  xds_obj$max_ix = max_ix 
  
  return(xds_obj)
})}

#' @title Initialize VC 
#' 
#' @description To compute vectorial capacity, we
#' need to  
#' 
#' @param xds_obj a **`ramp.xds`** model object  
#' @param s the vector species index
#' 
#' @importFrom MASS ginv
#' @importFrom expm expm
#' @return a **`ramp.xds`** model object  
#' @keywords internal
#' @export
setup_VC.SI <- function(xds_obj, s){
  f = get_f(xds_obj, s) 
  q = get_q(xds_obj, s) 
  M = get_M(xds_obj, s) 
  W <- xds_obj$XY_interface$W[[s]]
  ## Iniitial Conditions
  Y0 <- f*q*M/W
  xds_obj <- change_MY_inits(xds_obj, s, list(M=0*M, Y=Y0))
  YY0 <- diag(xds_obj$nPatches)
  diag(YY0) <- get_Upsilon(xds_obj, s) %*% Y0
  
  # Set up the VC tracking variable
  vc_obj <- list()
  class(vc_obj) = "SI"
  vc_obj$nVars = 2*(xds_obj$nPatches)^2
  vc_obj$inits = list()
  vc_obj$inits$YY = YY0
  vc_obj$inits$VC = 0*YY0
  
  ix <- xds_obj$nOtherVariables + 1
  xds_obj$nOtherVariables = ix
  vc_obj$V_i <- ix 
  vc_obj$s <- s 
  xds_obj$V_obj[[ix]] <- vc_obj 
  
  return(xds_obj)
}


#' @title Compute the VC Matrix
#' 
#' @description Compute vectorial capacity for 
#' the `SI` module
#'
#' @inheritParams dMYdt
#' 
#' @return a numeric [matrix]
#' @keywords internal
#' @export
dVdt.SI = function(t, y, xds_obj, i){
  with(xds_obj$V_obj[[i]],{
    YY <- matrix(y[ix$YY_ix], xds_obj$nPatches, xds_obj$nPatches)
    with(xds_obj$MY_obj[[s]], {
      dYY = - Omega %*% YY
      dVC = Upsilon %*% (diag(f*q) %*% YY)
      return(c(as.vector(dYY), as.vector(dVC)))
    })})}
