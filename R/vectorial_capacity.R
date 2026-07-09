
#' @title Make VC 
#' 
#' @description Using formulas,
#' compute the \eqn{N_p \times N_p} 
#' matrix \eqn{[V]} whose columns describe
#' the number of infective bites arising in each patch 
#' from all the mosquitoes biting a single human on a
#' single day in each patch. 
#' 
#' 
#' @param xds_obj a **`ramp.xds`** model object  
#' @param s the vector species index
#' 
#' @importFrom MASS ginv
#' @importFrom expm expm
#' @return a numeric [matrix]
#'
#' @export
make_VC <- function(xds_obj, s=1){
  UseMethod("make_VC", xds_obj$MY_obj[[s]])
}

#' @title Setup VC initial values 
#' 
#' @description To compute vectorial capacity, we
#' need to  
#' 
#' @param xds_obj a **`ramp.xds`** model object  
#' @param s the vector species index
#' 
#' @importFrom MASS ginv
#' @importFrom expm expm
#' @return a numeric [matrix]
#' @keywords internal
#' @export
setup_VC <- function(xds_obj, s){
  UseMethod("setup_VC", xds_obj$MY_obj[[s]])
}

#' @title Compute the VC Matrix
#' 
#' @description Compute the \eqn{N_p \times N_p} 
#' matrix \eqn{\mathcal{V}} whose columns describe
#' the number of infective bites arising in each patch 
#' from all the mosquitoes biting a single human on a
#' single day in each patch. 
#' 
#' This creates a new model object to compute VC then
#' + sets the initial condition to be the number of mosquitoes
#' biting a single infectious human on a single day, \eqn{fqM/W} 
#' + sets mean forcing to zero
#' + adds a variable that tracks infective biting, \eqn{fqZ}
#' + runs the model until the expected number of living mosquitoes from that
#' cohort is one in a million 
#' + saves the 
#'  
#' @param xds_obj a **`ramp.xds`** model object  
#' @param s the vector species index
#' @param tol a tolerance parameter
#' 
#' @importFrom MASS ginv
#' @importFrom expm expm
#' @return a numeric [matrix]
#' @export
compute_VC <- function(xds_obj, s=1, tol=1e-5){
  with(xds_obj$outputs, if(!exists("VCmatrix")) 
    xds_obj$outputs$VCmatrix <- list())
  # Set up the VC model 
  vc_model <- burnin(xds_obj)
  vc_model <- last_to_inits(vc_model)
  vc_model <- setup_L_obj("trivial", vc_model, s, list(Lambda=rep(0, xds_obj$nPatches))) 

  vc_model <- setup_VC(vc_model, s)
  
  vc_model <- make_indices(vc_model)

  
  # Solve the system
  Tx = round(-log(tol)/max(get_g(vc_model, s))) 
  vc_model <- xds_solve(vc_model, Tmax=365) 
  
  # Pull the VC matrix 
  get_last(vc_model) -> last_y
  ix <- vc_model$nOtherVariables 
  get_V_ix(vc_model, ix)$VC_ix -> VC_ix
  
  vc_matrix <- with(vc_model, matrix(last_y[VC_ix], nPatches, nPatches))
  xds_obj$outputs$VCmatrix[[s]] <- vc_matrix  
  return(xds_obj)
}

#' @title Get VC 
#' 
#' @description Get the VC matrix
#' 
#' @param xds_obj a **`ramp.xds`** model object  
#' @param s the vector species index
#' @param computed if true, get the VC matrix that was computed; if false, by formula
#' 
#' @importFrom MASS ginv
#' @importFrom expm expm
#' @return a numeric [matrix]
#'
#' @export
get_VC_matrix <- function(xds_obj, s=1, computed=TRUE){
  if(computed) 
    return(xds_obj$outputs$VCmatrix[[s]])
  else
    return(xds_obj$analysis$VCmatrix[[s]])
}

#' @title Get VC 
#' 
#' @description Get the VC matrix
#' 
#' @inheritParams get_VC_matrix
#' 
#' @importFrom MASS ginv
#' @importFrom expm expm
#' @return a numeric [matrix]
#'
#' @export
get_VC <- function(xds_obj, s=1, computed=TRUE){
  colSums(get_VC_matrix(xds_obj, s, computed))
}

#' @title Make the VC Matrix
#' 
#' @description Compute the VC matrix
#' for models that use the parameter set for the
#' generalized Macdonald model
#'
#' 
#' @note This is called for the models `macdonald`, 
#' `GeM`, and `SI`
#' 
#' 
#' @param xds_obj a **`ramp.xds`** model object  
#' @param s the vector species index
#' 
#' @importFrom MASS ginv
#' @importFrom expm expm
#' 
#' @return a numeric [matrix]
#' @export
make_VC_gem <- function(xds_obj, s=1){
  
  if(with(xds_obj$analysis, !exists("VC"))) 
    xds_obj$analysis$VC = list()
  if(with(xds_obj$analysis, !exists("AC"))) 
    xds_obj$analysis$AC = list()

  xds_obj1 <- xds_steady(xds_obj)
  xds_obj1 <- last_to_inits(xds_obj1)
  
  with(xds_obj1$MY_obj[[s]],{
    Omega <- compute_Omega_xde(g, sigma, mu, K_matrix)
    Omega_inv = ginv(Omega)
    M = get_M(xds_obj1,s)
    W <- xds_obj1$XY_interface$W[[s]]
    Upsilon = expm(-Omega*eip)
    fq <- diag(nPatches) 
    diag(fq) <- f*q
    fqMW <- diag(nPatches)
    ix = which(W==0)
    W[ix]=1; q[ix]=0 
    diag(fqMW) <- f*q*M/W
    xds_obj$analysis$ACmatrix[[s]] = fq %*% Omega_inv %*% Upsilon %*% Omega_inv %*% fq
    xds_obj$analysis$VCmatrix[[s]] = fq %*% Omega_inv %*% Upsilon %*% fqMW
    
    return(xds_obj)
})}
