
#' @title Make the VC Matrix
#' 
#' @param xds_obj a **`ramp.xds`** model object  
#' @param s the vector species index
#' 
#' @importFrom MASS ginv
#' @importFrom expm expm
#' @keywords internal
#' @describeIn destination description
#' @return a numeric [matrix]
#' @export
make_VC.GeM <- function(xds_obj, s=1){
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
#' @keywords internal
#' @return a **`ramp.xds`** model object  
#' @export
setup_V_ix.GeM <- function(xds_obj, i){with(xds_obj,{
  
  ZZ_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(ZZ_ix, 1)
  
  VC_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(VC_ix, 1)
  
  xds_obj$V_obj[[i]]$ix = list(ZZ_ix=ZZ_ix, VC_ix=VC_ix)
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
#' @keywords internal
#' @return a **`ramp.xds`** model object  
#' @export
setup_VC.GeM <- function(xds_obj, s){
  f = get_f(xds_obj, s) 
  q = get_q(xds_obj, s) 
  M = get_M(xds_obj, s) 
  W <- xds_obj$XY_interface$W[[s]]
  ## Iniitial Conditions
  Y0 <- f*q*M/W
  xds_obj <- change_MY_inits(xds_obj, s, list(M=0*M, Y=Y0, Z=0*M))
  YY0 <- diag(xds_obj$nPatches)
  diag(YY0) <- Y0
  ZZ0 <- get_Upsilon(xds_obj, s) %*% YY0
  
  # Set up the VC tracking variable
  vc_obj <- list()
  class(vc_obj) = "macdonald"
  vc_obj$nVars = 2*(xds_obj$nPatches)^2
  vc_obj$inits = list()
  vc_obj$inits$ZZ = ZZ0
  vc_obj$inits$VC = 0*ZZ0
  
  ix <- xds_obj$nOtherVariables + 1
  xds_obj$nOtherVariables = ix
  vc_obj$V_i <- ix 
  vc_obj$s <- s 
  xds_obj$V_obj[[ix]] <- vc_obj 
}


#' @title Compute the VC Matrix
#' 
#' @description Compute vectorial capacity for 
#' the `GeM` module
#'
#' @inheritParams dMYdt
#' 
#' @return a numeric [matrix]
#' @keywords internal
#' @export
dVdt.GeM = function(t, y, xds_obj, i){
  with(xds_obj$V_obj[[i]],{
    ZZ <- as.matrix(y[ix$ZZ_ix], nPatches, nPatches)
    with(xds_obj$MY_obj[[s]], {
      dZZ = - Omega %*% ZZ
      dVC = f*q*ZZ
      return(c(as.vector(dZZ), as.vector(dVC)))
    })})}
