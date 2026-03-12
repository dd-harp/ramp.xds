

#' @title Mosquito Dispersal
#'   
#' @section Mosquito Dispersal Matrix, \eqn{K}: 
#' 
#' In adult mosquito modules, mosquito dispersal is described by 
#' a square matrix \eqn{K} with `nPatches` \eqn{(=n)} rows and columns. By convention,
#' all \eqn{K} matrices have the form:
#' \deqn{
#'  K = \left[ \begin{array}{ccccc}
#'  -1 & k_{1,2} & k_{1,3} & \cdots & k_{1, n} \\
#'  k_{2,1} & -1 & k_{2,3} & \cdots & k_{2, n} \\
#'  k_{3,1} & k_{3,2} & -1 & \cdots & k_{3, n} \\
#'  \vdots & \vdots & \vdots & \ddots  & \vdots \\
#'  k_{n,1} & k_{n,2} & k_{n,3} & \cdots & -1 \\
#'  \end{array} \right]
#' }
#' We interpret \eqn{K} as the destinations of 
#' emigrating mosquitoes that survive and stay in the system.
#' The values of the elements \eqn{k_{i,j}} are thus 
#' constrained such that there is no net dispersal loss from the system: \eqn{\forall i,}
#' \deqn{\sum_j k_{i,j} = 1.} 
#' The columns of \eqn{K} sum up to zero.
#' 
#' Emigration-related loss (including mortality and emigration from the spatial domain)
#' is modeled with another parameter (see [mosquito_demography]).  
#' 
#' @section Basic Setup: 
#' \describe{
#'   \item{`K_matrix`}{a matrix or a named list: options that set up \eqn{K}}
#' }
#' 
#' There are several ways to configure  \eqn{K} during basic setup. 
#' 
#' By **default:** `K_matrix` is an empty list. When the `MY_obj` is set up, the \eqn{K} 
#' matrix is set to `diag(g)`, a model with no dispersal. That is not changed unless one of the
#' following holds: 
#' 
#' + `is.matrix(K_matrix)`: if the user to passes a proper \eqn{K} matrix, then it is used as-is
#' 
#' + `with(Kmatrix, exists("Kname"))`: If a string in the named list passed as `K_matrix` is called `Kname`, then basic setup calls [setup_K_matrix] with `options=K_matrix` 
#'
#' @seealso [setup_K_matrix]
#'  
#' @name mosquito_dispersal 
NULL


#' @title Get the Mosquito Dispersal Matrix
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
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
#' @return an **`xds`** object
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
#' @keywords internal
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
#' @param t current simulation time
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
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
#' @return an **`xds`** object
#' @keywords internal
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
#' @return an **`xds`** object
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
#' @keywords internal
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
#' @keywords internal
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
  diag(K_matrix) <- -1 
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
#' @keywords internal
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
  diag(K_matrix) <- -1 
  return(K_matrix)
}
