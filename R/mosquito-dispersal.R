
#' @title Mosquito Dispersal
#' 
#' @description 
#' Mosquito dispersal is handled 
#' in a standard way across all **MY** modules.
#' + A mosquito dispersal matrix, called \eqn{K} or `K_matrix`, is set up using [setup_K_matrix]; 
#' + \eqn{K} is used to make a mosquito demographic matrix, called \eqn{\Omega} or `Omega` (see [xds_info_mosquito_demography]).
#'  
#' @section Setup: 
#' \describe{
#'   \item{`K_matrix`}{either a \eqn{K} matrix: or options for [setup_K_matrix]}
#' }
#' 
#' There are several ways to configure  \eqn{K} during basic setup. 
#' 
#' By **default:** `K_matrix` is an empty list. When the `MY_obj` is set up, the \eqn{K} 
#' matrix is set to `diag(g)`, a model with no dispersal. That is not changed unless one of the
#' following holds: 
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
#' The values of the elements \eqn{k_{i,j}} are thus 
#' constrained such that there is no net dispersal loss from the system: \eqn{\forall i,}
#' \deqn{\sum_j k_{i,j} = 1.} 
#' The columns of \eqn{K} sum up to zero.
#' \eqn{K} thus describes the destinations of 
#' emigrating mosquitoes that survive and stay in the system.
#'  
#' Emigration rates and emigration-related loss -- emigration from the spatial domain and mortality that is conditioned on emigration -- 
#' are handled separately (see [xds_info_mosquito_demography]).  
#' 
#' @seealso [setup_K_matrix], [change_K_matrix] & [xds_info_mosquito_demography] 
#'  
#' @name xds_info_mosquito_dispersal 
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
#' @description
#' Check that `K_matrix` 
#' + is an `nPatches` \eqn{\times} `nPatches` matrix;
#' + diagonal elements are -1;
#' + and columns sum to 0. 
#' 
#' After passing checks, `xds_obj` is updated.  
#' 
#' In models with multiple species, use `s` to 
#' specify the species to update. 
#' 
#' @param K_matrix a mosquito dispersal [matrix]
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' @seealso [xds_info_mosquito_dispersal]; [setup_K_matrix]
#' @export
change_K_matrix = function(K_matrix, xds_obj, s=1){
  stopifnot(is.matrix(K_matrix))
  stopifnot(diag(K_matrix) == -1)
  stopifnot(dim(K_matrix) == rep(xds_obj$nPatches,2))
  stopifnot(colSums(K_matrix) < 1e-7) 
  xds_obj$MY_obj[[s]]$K_matrix <- K_matrix
  xds_obj$MY_obj[[s]]$Omega_obj <- trigger_setup(xds_obj$MY_obj[[s]]$Omega_obj)
  xds_obj <- update_Omega_xde(xds_obj, s)
  xds_obj$MY_obj[[s]]$Upsilon_obj <- trigger_setup(xds_obj$MY_obj[[s]]$Upsilon_obj)
  xds_obj <- update_Upsilon_xde(xds_obj, s)
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
  MY_obj$K_matrix = diag(0, nPatches)
  MY_obj$K_obj <- list()
  class(MY_obj$K_obj) <- "static"
  return(MY_obj)
}

#' @title Compute the blood feeding rate, f
#'
#' @description  It should
#' set the values of the bionomic parameters to baseline values
#'
#' @note This method dispatches on the type of `K_obj` attached to the `MY_obj`.
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
#' @note This method dispatches on the type of `K_obj` attached to the `MY_obj`.
#'
#' @inheritParams F_f
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
F_K_matrix.static = function(t, xds_obj, s){
  return(xds_obj)
}

#' @title Setup Mosquito Dispersal Matrix
#' @description
#' A flexible setup function for mosquito dispersal. The first
#' argument `Kname` dispatches a method: `Kname` is used to set the `class` of `options,` and 
#' the method  dispatches on `class(options)`
#' 
#' Options for `Kname` are:
#'  
#' + `is.matrix(Kname)`: if the user passes a matrix, then `class(Kname) <- "as_matrix"`
#' + `Kname = "no_setup"` -- the **`xds`** object is returned unmodified 
#' + `Kname = "as_matrix"` -- calls [change_K_matrix] and passes `K_matrix`
#' + `Kname = "herethere"` -- calls [make_K_matrix_herethere] 
#' + `Kname = "xy"` -- calls [make_K_matrix_xy] 
#' 
#' @param Kname a matrix or a string
#' @param xds_obj an **`xds`** model object
#' @param options a list of options to configure K_matrix
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' @export
setup_K_matrix = function(Kname, xds_obj, options = list(), s=1){
  if(is.matrix(Kname)) class(options) <- "as_matrix"
  if(is.character(Kname)) class(options) <- Kname 
  UseMethod("setup_K_matrix", options)
}

#' @title Setup no dispersal matrix
#'
#' @description Implements [setup_K_matrix] for the "no_setup" case
#'
#' @inheritParams setup_K_matrix
#'
#' @return a [matrix]
#' @keywords internal
#' @export
setup_K_matrix.no_setup = function(Kname, xds_obj, options = list(), s=1){
  return(xds_obj) 
}

#' @title Setup a Here-There Dispersal Matrix
#'
#' @description Implements [setup_K_matrix] for the here and there model:
#' dispersal to every other patch, with equal probability
#'
#' @inheritParams setup_K_matrix
#'
#' @return a [matrix]
#' @keywords internal
#' @export
setup_K_matrix.as_matrix = function(Kname, xds_obj, options = list(), s=1){
  K_matrix = Kname
  change_K_matrix(K_matrix, xds_obj, s)
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
setup_K_matrix.herethere = function(Kname, xds_obj, options = list(), s=1){
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
setup_K_matrix.xy = function(Kname = "xy", xds_obj, options=list(), s=1) {
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
