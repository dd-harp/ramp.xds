
#' @title Get the Mosquito Dispersal Matrix 
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return an **`xds`** model object
#' 
#' @export
get_K_matrix = function(xds_obj, s=1){
  return(xds_obj$MY_obj[[s]]$K_matrix)
}

#' @title Change Mosquito Dispersal Matrix 
#' 
#' @param K_matrix a mosquito dispersal [matrix]
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return an **`xds`** model object
#' 
#' @export
change_K_matrix = function(K_matrix, xds_obj, s=1){
  stopifnot(dim(K_matrix) == dim(xds_obj$MY_obj[[s]]$K_matrix))
  xds_obj$MY_obj[[s]]$K_matrix <- K_matrix
  return(xds_obj)
}

#' @title Setup a Human Fraction Bionomic Object 
#' 
#' @description Set up an object
#' to compute the human fraction, \eqn{K} 
#' 
#' @param nPatches the number of patches in the model 
#' @param MY_obj an **`MY`** model object 
#' 
#' @return a **`MY`** model object
#' 
#' @export
setup_K_obj = function(nPatches, MY_obj){
  MY_obj$K_matrix = diag(nPatches)
  MY_obj$K_obj <- list()  
  class(MY_obj$K_obj) <- "static" 
  return(MY_obj)
}

#' @title Compute the blood feeding rate, f
#' 
#' @description  It should
#' set the values of the bionomic parameters to baseline values
#' 
#' @note This method dispatches on the type of `f_obj` attached to the `MY_obj`.
#' 
#' @param t current simulation time
#' @param xds_obj a **`xds`** model object
#' @param s vector species index
#' 
#' @return a [numeric] vector of length `nPatches`
#' 
#' @export
F_K_matrix = function(t, xds_obj, s) {
  UseMethod("F_K_matrix", xds_obj$MY_obj[[s]]$K_obj)
}

#' @title Constant baseline blood feeding rate
#' 
#' @description Implements [F_K_matrix] for a static model
#' 
#' @note This method dispatches on the type of `f_obj` attached to the `MY_obj`.
#' 
#' @inheritParams F_feeding_rate 
#' 
#' @return the **`xds`** model object
#' @export
F_K_matrix.static = function(t, xds_obj, s){
  return(xds_obj)
}

#' @title Setup Mosquito Dispersal Matrix 
#' 
#' @param Kname a matrix or a string
#' 
#' @param s the vector species index
#' @param xds_obj an **`xds`** model object
#' @param options a list of options to configure K_matrix
#' 
#' @return an **`xds`** model object
#' @export
setup_K_matrix = function(Kname, s, xds_obj, options = list()){
  if(is.matrix(Kname)) class(options) <- "as_matrix"
  if(is.character(Kname)) class(options) <- Kname 
  UseMethod("setup_K_matrix", options)
}

#' @title Setup a Here-There Dispersal Matrix
#' 
#' @description Implements [setup_K_matrix] for the herethere model:
#' dispersal to every other patch, with equal probability
#' 
#' @inheritParams setup_K_matrix
#' 
#' @return a [matrix]
#' @export
setup_K_matrix.as_matrix= function(Kname, s, xds_obj, options = list()){
  change_K_matrix(Kname, xds_obj, s)
}

#' @title Setup a Here-There Dispersal Matrix
#' 
#' @description Implements [setup_K_matrix] for the herethere model:
#' dispersal to every other patch, with equal probability
#' 
#' @inheritParams setup_K_matrix
#' 
#' @return a [matrix]
#' @export
setup_K_matrix.herethere = function(Kname, s, xds_obj, options = list()){
  K_matrix <- make_K_matrix_herethere(xds_obj$nPatches)
  change_K_matrix(K_matrix, xds_obj, s)
}

#' @title Make a Here-There Dispersal Matrix
#' 
#' @param nPatches is the number of patches in the model
#' 
#' @export
make_K_matrix_herethere = function(nPatches) {
  K_matrix <- matrix(1/(nPatches-1), nPatches, nPatches)
  diag(K_matrix) <- 0
  return(K_matrix)
}

#' @title Setup a Kernel-Based Mosquito Dispersal Matrix
#'  
#' @description Set up a mosquito dispersal matrix from
#' a set of \eqn{x,y} coordinates and a *kernel,* a function 
#' that assigns weights by distance. The fraction leaving from 
#' each patch that arrive at other patch is the vector of normalized
#' weights.  
#' 
#' @inheritParams setup_K_matrix
#' 
#' @return a [matrix]
#' @export
setup_K_matrix.xy = function(Kname = "xy", s, xds_obj, options=list()) {
  K_matrix <- with(options, make_K_matrix_xy(xy, ker))
  change_K_matrix(K_matrix, xds_obj, s)
}

#' @title make a Kernel-Based Mosquito Dispersal Matrix
#' 
#' @param xy is a vector of the xy-coordinates of patch locations
#' @param ker is a function that weights putative locations by distance
#' @export
make_K_matrix_xy = function(xy, ker = F_exp) {
  dmat <- as.matrix(stats::dist(xy), upper=T)
  K_matrix <- ker(dmat)
  diag(K_matrix) <- 0
  K_matrix = K_matrix %*% diag(1/rowSums(K_matrix))
  return(K_matrix)
}
