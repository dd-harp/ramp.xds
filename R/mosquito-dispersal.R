
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

#' @title Check K Matrix
#' 
#' @description
#' Check that 
#' + \eqn{K} is a \eqn{N_p \times N_p} matrix
#' + if not zero, the diagonal elements are all \eqn{-1}
#' + the columns sum to 0: tolerance is set by `tol`
#' 
#' @param K a mosquito dispersal matrix
#' @param Np the number of patches
#' @param tol tolerance
#' 
#' @seealso [xds_info_mosquito_dispersal]
#' @export
check_K_matrix = function(K, Np, tol=1e-12){
  stopifnot(is.matrix(K))
  stopifnot(dim(K)==c(Np, Np))
  diagK = diag(K)
  stopifnot(diagK == -1 | diagK == 0)
  stopifnot(abs(colSums(K)) < tol)
}


#' @title Change a Mosquito Dispersal Matrix
#' 
#' @description
#' Run [check_K_matrix] 
#' 
#' After passing checks, `xds_obj` is updated.  
#' 
#' In models with multiple species, use `s` to 
#' specify the species to update. 
#' 
#' @note
#' The argument `which_K` is used to retrieve state-dependent
#' mosquito dispersal arguments for behavioral state models.
#' The default "K" changes `K_matrix`
#' 
#' @param K_matrix a mosquito dispersal [matrix]
#' @param xds_obj an **`xds`** model object
#' @param which_K which K_matrix
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' @seealso [xds_info_mosquito_dispersal]; [setup_K_matrix]
#' @export
change_K_matrix = function(K_matrix, xds_obj, which_K="K", s=1){
  class(which_K) = which_K
  UseMethod("change_K_matrix", which_K)
}

#' @title Change Mosquito Dispersal Matrix
#' @description
#' Run [check_K_matrix] then 
#' 
#' After passing checks, `xds_obj` is updated.  
#' 
#' In models with multiple species, use `s` to 
#' specify the species to update. 
#' 
#' @inheritParams change_K_matrix
#'
#' @return an **`xds`** object
#' @keywords internal
#' @seealso [xds_info_mosquito_dispersal]; [setup_K_matrix]
#' @export
change_K_matrix.K = function(K_matrix, xds_obj, which_K="K", s=1){
  check_K_matrix(K_matrix, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$K_matrix <- K_matrix
  xds_obj$MY_obj[[s]]$Omega_obj <- trigger_setup(xds_obj$MY_obj[[s]]$Omega_obj)
  xds_obj <- update_Omega_xde(xds_obj, s)
  xds_obj$MY_obj[[s]]$Upsilon_obj <- trigger_setup(xds_obj$MY_obj[[s]]$Upsilon_obj)
  xds_obj <- update_Upsilon_xde(xds_obj, s)
  return(xds_obj)
}

#' @title Get the Mosquito Dispersal Matrix
#' 
#' @description
#' Get (inspect) a mosquito dispersal matrix.
#' 
#' @note
#' The argument `K` is used to retrieve state-dependent
#' mosquito dispersal arguments for behavioral state models.
#' The default "K" returns `K_matrix`
#' 
#' @param xds_obj an **`xds`** model object
#' @param which_K which K_matrix
#' @param s the vector species index
#' 
#' @return an **`xds`** object
#'
#' @export
get_K_matrix = function(xds_obj, which_K="K", s=1){
  class(which_K) = which_K
  UseMethod("get_K_matrix", which_K)
}

#' @title Get the Mosquito Dispersal Matrix
#'
#' @inheritParams get_K_matrix
#'
#' @return an **`xds`** object
#' @keywords internal
#' 
#' @export
get_K_matrix.K = function(xds_obj, which_K="K", s=1){
  return(xds_obj$MY_obj[[s]]$K_matrix)
}


#' @title Setup K_obj
#'
#' @description Set up a port object
#' for mosquito dispersal
#'
#' @param MY_obj an **`MY`** model object
#'
#' @return an **`MY`** model object
#' @keywords internal
#' @export
setup_K_obj = function(MY_obj){
  K_obj <- list() 
  class(K_obj) = "static"
  MY_obj$K_obj = K_obj 
  return(MY_obj)
}



#' @title Setup Mosquito Dispersal Matrix
#' @description
#' A flexible function to set up or change the mosquito dispersal matrix (see [xds_info_mosquito_dispersal]).
#' 
#' The function was designed to dispatch on the first argument, `name`:
#'  
#' + `name` is a method name 
#' 
#' + `options` is a named list that sets the parameters in a function `make_K_matrix_name`
#' 
#' + Before dispatching, the function sets `class(option) = "name"`
#' 
#' Pre-dispatch cases were developed to make the function call more flexible: any
#' matrix can be passed as the first argument: or the user could set up an options list
#' and pass it
#' (*e.g.* `Koptions` is passed to `setup_K_matrix` in `xds_setup`). The pre-dispatch
#' parsing: 
#' + if `name` is a method name, set `class(options) = "name"`
#' + if `name` is a matrix, set `class(options) = "as_matrix"`
#' + if `name` is a list of options, 
#' 
#' Available methods are: 
#'
#' + "as_matrix" --- sets up the matrix
#' + "herethere" --- calls [make_K_matrix_herethere] 
#' + "xy" -- calls [make_K_matrix_xy]
#' + "list" --- for options lists
#' + "zero" --- sets up the zero matrix
#' + "no_setup" --- returns the **`xds`** object without modification
#' 
#' @param name a method name: or a matrix, or a list
#' @param xds_obj an **`xds`** model object
#' @param options a list of options to configure K_matrix
#' @param s the vector species index
#'
#' @return an **`xds`** object 
#' @export
setup_K_matrix = function(name, xds_obj, options=list(), s=1){
  if(is.matrix(name)) class(options) = "as_matrix"
  if(is.character(name)) class(options) = name
  UseMethod("setup_K_matrix", options)
}

#' @title Setup K_matrix 
#'
#' @description If the options list is passed
#' as the first argument, the set 
#' + `Kname = name$name` 
#' + `options = name`
#' and call `setup_K_matrix(Kname, xds_obj, options, s)` 
#'
#' @inheritParams setup_K_matrix
#'
#' @return a [matrix]
#' @keywords internal
#' @export
setup_K_matrix.list = function(name, xds_obj, options=list(), s=1){
  options = name
  Kname = name$name
  if(is.null(Kname)) Kname = "no_setup"
  xds_obj <- setup_K_matrix(Kname, xds_obj, options, s)
  return(xds_obj)
}

#' @title Setup K_matrix 
#'
#' @description Call [setup_K_matrix] \eqn{N} times. 
#' The options for the \eqn{i^{th}} call are `options[[i]]` 
#' 
#' 
#' of options 
#'
#' @inheritParams setup_K_matrix
#'
#' @return a [matrix]
#' @keywords internal
#' @export
#' 
setup_K_matrix.N = function(name, xds_obj, options=list(), s=1){
  for(i in 1:options$N){
    opts <- options$opts[[i]]
    xds_obj <- setup_K_matrix(opts, xds_obj, list(), s)  
  }
  return(xds_obj)
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
setup_K_matrix.no_setup = function(name, xds_obj, options = list(), s=1){
  return(xds_obj)
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
setup_K_matrix.zero = function(name, xds_obj, options = list(), s=1){
  which_K = with(options, ifelse(exists("which_K"), which_K, "K"))
  K_matrix = with(xds_obj, matrix(0, nPatches, nPatches))
  change_K_matrix(K_matrix, xds_obj, which_K, s)
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
setup_K_matrix.as_matrix = function(name, xds_obj, options=list(), s=1){

  if(is.list(options)) 
    K_matrix = options$K_matrix
  if(is.matrix(name))
    K_matrix = name

  which_K = with(options, ifelse(exists("which_K"), which_K, "K"))
  change_K_matrix(K_matrix, xds_obj, which_K, s)
}

#' @title Setup a Here-There Dispersal Matrix
#'
#' @description Implements [setup_K_matrix] for the herethere model:
#' dispersal to every other patch, with equal probability
#'
#' @inheritParams setup_K_matrix
#'
#' @return an **`xds`** object 
#' @keywords internal
#' @export
setup_K_matrix.herethere = function(name, xds_obj, options=list(), s=1){
  which_K = with(options, ifelse(exists("which_K"), which_K, "K"))
  K_matrix <- make_K_matrix_herethere(xds_obj$nPatches)
  change_K_matrix(K_matrix, xds_obj, which_K, s)
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
setup_K_matrix.xy = function(name, xds_obj, options=list(), s=1) {
  which_K = with(options, ifelse(exists("which_K"), which_K, "K"))
  if(with(options, !exists("V"))) options$V = list()
  K_matrix <- with(options, make_K_matrix_xy(xy, F_K, V))
  change_K_matrix(K_matrix, xds_obj, which_K, s)
}

#' @title make a Kernel-Based Mosquito Dispersal Matrix
#'
#' @param xy is a vector of the xy-coordinates of patch locations
#' @param F_K is a function that weights putative locations by distance
#' @param V variables
#' 
#' @export
make_K_matrix_xy = function(xy, F_K = F_exp, V=list()) {
  dmat <- as.matrix(stats::dist(xy), upper=T)
  K_matrix <- F_K(dmat, V)
  diag(K_matrix) <- 0
  K_matrix = K_matrix %*% diag(1/rowSums(K_matrix))
  diag(K_matrix) <- -1 
  return(K_matrix)
}

#' @title Dynamically update a K Matrix
#'
#' @description A port function to 
#' updates the mosquito dispersal 
#' matrix dynamically. 
#' 
#' @note The port object is called `K_obj`
#' 
#' @param t current simulation time
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#' 
#' @return a [numeric] vector of length `nPatches`

#' @keywords internal
#' @export
F_K_matrix = function(t, xds_obj, s) {
  UseMethod("F_K_matrix", xds_obj$MY_obj[[s]]$K_obj)
}

#' @title Dynamically update a K Matrix
#'
#' @description Implements [F_K_matrix] for a static model
#'
#' @inheritParams F_K_matrix
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
F_K_matrix.static = function(t, xds_obj, s){
  return(xds_obj)
}