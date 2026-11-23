# X

#' @title Mosquito Dispersal
#' 
#' @description 
#' Mosquito dispersal is handled 
#' in a standard way across all **MY** modules.
#' + A mosquito dispersal matrix, called \eqn{K} or `K_matrix`, is set up using [setup_K_matrix]; 
#' + \eqn{K} is used to make a mosquito demographic matrix, called \eqn{\Omega} or `Omega` (see [xds_info_mosquito_demography]).
#'  
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
#' @seealso [setup_K_matrix] | [check_K_matrix] | [xds_info_mosquito_demography] 
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
#' @return an **`xds`** model object
#' @seealso [xds_info_mosquito_dispersal]; [setup_K_matrix]
#' @export
change_K_matrix = function(K_matrix, xds_obj, which_K="K", s=1){
  class(which_K) = which_K
  UseMethod("change_K_matrix", which_K)
}

#' @title Change Mosquito Dispersal Matrix
#' @description
#' Run [check_K_matrix].
#' 
#' After passing checks, `xds_obj` is updated.  
#' 
#' For models with two or more dispersal matrices, 
#' use `which_K`.
#' 
#' In models with multiple species, use `s` to 
#' specify the species to update. 
#' 
#' @inheritParams change_K_matrix
#' 
#' @note
#' The argument `which_K` supports behavioral state
#' models for mosquito ecology with two or more 
#' mosquito dispersal matrices.
#' The default "K" returns `K_matrix`
#' 
#' @return an **`xds`** model object
#' @keywords internal
#' @seealso [xds_info_mosquito_dispersal]; [setup_K_matrix]
#' @export
change_K_matrix.K = function(K_matrix, xds_obj, which_K="K", s=1){
  check_K_matrix(K_matrix, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$K_matrix <- K_matrix
  xds_obj$MY_obj[[s]]$K_matrix_t <- K_matrix
  xds_obj$MY_obj[[s]]$es_K_matrix <- matrix(1, xds_obj$nPatches, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$K_obj <- make_static_obj()
  xds_obj$MY_obj[[s]]$K_obj$K_matrix <- K_matrix
  xds_obj <- change_Omega(xds_obj, s)
  return(xds_obj)
}

#' @title Get the Mosquito Dispersal Matrix
#' 
#' @description
#' Get (inspect) a mosquito dispersal matrix.
#' 
#' @note
#' The argument `which_K` is used to retrieve state-dependent
#' mosquito dispersal matrices for behavioral state models.
#' The default "K" returns `K_matrix`
#' 
#' @param xds_obj an **`xds`** model object
#' @param which_K which K_matrix
#' @param s the vector species index
#' 
#' @return a mosquito dispersal [matrix] of dimensions `nPatches` by `nPatches`
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
#' @return a mosquito dispersal [matrix] of dimensions `nPatches` by `nPatches`
#' @keywords internal
#' 
#' @export
get_K_matrix.K = function(xds_obj, which_K="K", s=1){
  return(xds_obj$MY_obj[[s]]$K_matrix)
}


#' @title Setup a mosquito dispersal matrix
#' 
#' @description
#' The advanced set up function for the mosquito dispersal matrix (see [xds_info_mosquito_dispersal]).
#' 
#' Available methods are:
#' + "as_matrix" --- sets up a passed matrix
#' + "herethere" --- calls [make_K_matrix_herethere] 
#' + "xy" -- calls [make_K_matrix_xy]
#' 
#' @seealso [xds_info_mosquito_dispersal]
#' 
#' @note 
#' The function was designed to work using arguments passed to [xds_setup]:
#' 
#' + if `name` is a string, then it is handled as a method name
#'  
#' + if `name` is a matrix, then `as_matrix` is dispatched
#' 
#' + `options` is a named list that sets method-specific parameters
#'
#'
#' Pre-dispatch cases were developed to make the function call more flexible: any
#' matrix can be passed as the first argument, or the user could set up an options list
#' and pass it
#' (*e.g.* `Koptions` is passed to `setup_K_matrix` in `xds_setup`). The pre-dispatch
#' parsing: 
#' + if `name` is a method name, set `class(options) = name`
#' + if `name` is a matrix, set `class(options) = "as_matrix"`
#' + if `name` is a list of options, then `list` is dispatched
#' 
#' Several methods are used during basic setup:
#' + "list" --- for options lists
#' + "zero" --- sets up the zero matrix
#' + "no_setup" --- returns the **`xds`** object without modification
#' 
#' @param name a method name: or a matrix, or a list
#' @param xds_obj an **`xds`** model object
#' @param options a list of options to configure K_matrix
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#' @export
setup_K_matrix = function(name, xds_obj, options=list(), s=1){
  if(is.matrix(name)) class(options) = "as_matrix"
  if(is.character(name)) class(options) = name
  UseMethod("setup_K_matrix", options)
}

#' @title Setup K_matrix 
#'
#' @description Set up the mosquito dispersal matrix using an options list.
#'
#' @note If an options list is passed as the first argument, use its
#' `name` field to select the setup method. If `name` is NULL, use the
#' `no_setup` method. Pass the list as `options` to [setup_K_matrix].
#'
#' @inheritParams setup_K_matrix
#'
#' @return an **`xds`** model object
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
#' The options for the \eqn{i^{th}} call are `options$opts[[i]]`
#'
#' @inheritParams setup_K_matrix
#'
#' @return an **`xds`** model object
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

#' @title Set up K_matrix (no setup) (**MY**)
#'
#' @description Leave the existing mosquito dispersal configuration unchanged.
#'
#' @inheritParams setup_K_matrix
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_K_matrix.no_setup = function(name, xds_obj, options = list(), s=1){
  return(xds_obj)
}

#' @title Set up a zero mosquito dispersal matrix (**MY**)
#'
#' @description Set up a zero mosquito dispersal matrix using [change_K_matrix].
#'
#' @inheritParams setup_K_matrix
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_K_matrix.zero = function(name, xds_obj, options = list(), s=1){
  which_K = with(options, ifelse(exists("which_K"), which_K, "K"))
  K_matrix = with(xds_obj, matrix(0, nPatches, nPatches))
  change_K_matrix(K_matrix, xds_obj, which_K, s)
}

#' @title Set up a supplied mosquito dispersal matrix
#'
#' @description Set up a supplied mosquito dispersal matrix using [change_K_matrix].
#'
#' @inheritParams setup_K_matrix
#'
#' @return an **`xds`** model object
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

#' @title Setup a here-and-there dispersal matrix
#'
#' @description Implements [setup_K_matrix] for the `herethere` method.
#' See [make_K_matrix_herethere]. 
#'
#' @inheritParams setup_K_matrix
#' @seealso [make_K_matrix_herethere] and [setup_K_matrix]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_K_matrix.herethere = function(name, xds_obj, options=list(), s=1){
  which_K = with(options, ifelse(exists("which_K"), which_K, "K"))
  K_matrix <- make_K_matrix_herethere(xds_obj$nPatches)
  change_K_matrix(K_matrix, xds_obj, which_K, s)
}

#' @title Make a here-and-there dispersal matrix
#' 
#' @description Mosquitoes disperse to every other patch
#' with equal probability. 
#' 
#' @param nPatches is the number of patches
#' 
#' @return a mosquito dispersal [matrix] of dimensions `nPatches` by `nPatches`
#'
#' @seealso [setup_K_matrix] 
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
#' that assigns weights by distance. The fractions leaving from
#' each patch that arrive at other patches form the vector of normalized
#' weights.
#'
#' @inheritParams setup_K_matrix
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_K_matrix.xy = function(name, xds_obj, options=list(), s=1) {
  which_K = with(options, ifelse(exists("which_K"), which_K, "K"))
  if(with(options, !exists("V"))) options$V = list()
  K_matrix <- with(options, make_K_matrix_xy(xy, F_K, V))
  change_K_matrix(K_matrix, xds_obj, which_K, s)
}

#' @title Make a mosquito dispersal matrix using patch locations
#' 
#' @description
#' Build a mosquito dispersal matrix using the \eqn{x} and \eqn{y} locations and
#' a function \eqn{F_K(d,V)} that weights by distance and perhaps some other variables.
#' 
#' 
#' @param xy a numeric matrix or data frame with one row per patch and columns
#' for the x and y coordinates
#' @param F_K is a function that weights putative locations by distance
#' @param V variables
#' 
#' @return a mosquito dispersal [matrix] with one row and column per patch location
#'
#' @seealso [setup_K_matrix], [xds_info_mosquito_dispersal]
#' @export
make_K_matrix_xy = function(xy, F_K = F_exp, V=list()) {
  dmat <- as.matrix(stats::dist(xy), upper=T)
  K_matrix <- F_K(dmat, V)
  diag(K_matrix) <- 0
  K_matrix = K_matrix %*% diag(1/rowSums(K_matrix))
  diag(K_matrix) <- -1 
  return(K_matrix)
}

#' @title Compute the mosquito dispersal matrix (**MY**)
#'
#' @description Compute the mosquito dispersal matrix.
#' 
#' @note The port object is called `K_obj`
#' and dispatch is on its class.
#' 
#' @param t time
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#' 
#' @return a mosquito dispersal [matrix] of dimensions `nPatches` by `nPatches`

#' @keywords internal
#' @export
F_K_matrix = function(t, xds_obj, s) {
  UseMethod("F_K_matrix", xds_obj$MY_obj[[s]]$K_obj)
}

#' @title Return the static mosquito dispersal matrix (**MY**)
#'
#' @description Return the mosquito dispersal matrix.
#'
#' @note The mosquito dispersal matrix is stored in the port object `K_obj`.
#'
#' @inheritParams F_K_matrix
#'
#' @return a mosquito dispersal [matrix] of dimensions `nPatches` by `nPatches`
#' 
#' @keywords internal
#' @export
F_K_matrix.static = function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$K_obj$K_matrix)
}