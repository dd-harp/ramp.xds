
#' @title Make indices for all the model variables
#' 
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** model object
#' @export
make_indices <- function(xds_obj) {
  xds_obj$max_ix <- 0

  s = length(xds_obj$L_obj)
  if(s>0)
    for(ix in 1:s)
      xds_obj = setup_L_ix(xds_obj, ix)

  s = length(xds_obj$MY_obj)
  if(s>0)
    for(ix in 1:s)
      xds_obj = setup_MY_ix(xds_obj, ix)

  i = length(xds_obj$XH_obj)
  if(i>0)
    for(ix in 1:i)
      xds_obj = setup_XH_ix(xds_obj, ix)

  return(xds_obj)
}

#' @title Get the stored initial values, \eqn{y_0}
#' @param xds_obj an **`xds`** model object
#' @param flatten a [logical]: if true, results are returned as an unnamed vector
#' @return a named [list] or `if(flatten==TRUE)` a [vector]
#' @export
get_inits <- function(xds_obj, flatten=FALSE){

  Li = list()
  s = length(xds_obj$L_obj)
  if(s>0)
    for(ix in 1:s)
      Li = c(Li, get_L_inits(xds_obj, ix))

  MYi = c()
  s = length(xds_obj$MY_obj)
  if(s>0)
    for(ix in 1:s)
      MYi = c(MYi, get_MY_inits(xds_obj, ix))

  Xi = c()
  i = length(xds_obj$XH_obj)
  if(i>0)
    for(ix in 1:i)
      Xi = c(Xi, get_XH_inits(xds_obj, ix))
  y = list(L=Li, MY=MYi, X=Xi)
  if(flatten) y <- xds_flatten(y)

  return(y)
}

#' @title Invert a diagonal matrix
#' @description Invert a diagonal matrix which is passed as a vector. If any
#' elements are zero, set them to one.
#' @param x a [numeric] vector
#' @return a diagonal [matrix]
#' @export
diag_inverse <- function(x) {
  x <- as.vector(x)
  ix <- which(x == 0)
  if (length(ix) > 0) {
    x[ix] <- 1
  }
  return(diag(x = 1/x, nrow = length(x), ncol = length(x), names = FALSE))
}

#' @title Check if two numeric values are approximately equal
#' @param a a [numeric] model object
#' @param b a [numeric] model object
#' @param tol the numeric tolerance
#' @return a [logical] value
#' @export
approx_equal <- function(a, b, tol = sqrt(.Machine$double.eps)) {
  abs(a - b) < tol
}

#' @title Check the length of an input value
#' @param x a [numeric] model object
#' @param lng a [numeric] model object
#' @param type a [character] string specifying required typeof
#' @param fixit a [logical] value, if TRUE force length to lng
#' @return a [numeric] model object
#' @export
checkIt = function(x, lng, type = "numeric", fixit=TRUE){
  stopifnot(is.numeric(x))
  if(type == "integer") x = as.integer(x)
  if(length(x)==1 & fixit) x=rep(x, lng)
  stopifnot(length(x)==lng)
  x
}

#' @title Check the shape and dimensions of an model object
#' 
#' @param obj a [numeric] model object
#' @param d1 an [integer]
#' @param d2 an [integer]
#' 
#' @return [matrix]
#' 
#' @export
shapeIt = function(obj, d1, d2){
  Obj = as.matrix(obj)
  dd = dim(Obj)
  stopifnot(d1 %in% dd)
  stopifnot(d2 %in% dd)
  if(dd[1]!=d1) obj = t(obj)
  return(obj)
}


#' @title Set the initial values to the last values of the last simulation
#' @param y0 a [vector] of initial values
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param i the host species index
#' @export
list_vars <- function(xds_obj, y0=NULL, s=1, i=1){
  if(is.null(y0)) y0=get_inits(xds_obj)
  y0 = unname(y0)
  c(
    get_L_vars(y0, xds_obj, s),
    get_MY_vars(y0, xds_obj, s),
    get_XH_vars(y0, xds_obj, s))
}

#' @title Set the initial values to the last values of the last simulation
#' @param xds_obj an **`xds`** model object
#' @param y0 a [vector] of initial values
#' @return y a [numeric] vector
#' @export
update_inits <- function(xds_obj, y0=NULL){
  if(is.null(y0)) y0 = get_last(xds_obj)

  s = length(xds_obj$L_obj)
  if(s>0) for(ix in 1:s){
    vars = get_L_vars(y0, xds_obj, ix)
    xds_obj = change_L_inits(xds_obj, ix, vars)
  }

  s = length(xds_obj$MY_obj)
  if(s>0) for(ix in 1:s){
    vars = get_MY_vars(y0, xds_obj, ix)
    xds_obj = change_MY_inits(xds_obj, ix, vars)
  }

  ii = length(xds_obj$XH_obj)
  if(ii>0) for(ix in 1:ii){
    vars = get_XH_vars(y0, xds_obj, ix)
    xds_obj = change_XH_inits(xds_obj, ix, vars)
  } 
  
  return(xds_obj)
}

#' @title Get the last state
#' 
#' @param xds_obj an **`xds`** model object
#' @param parse if TRUE return a named list
#'  
#' @return a [numeric] vector
#' @export
get_last <- function(xds_obj, parse=FALSE){
  y <- xds_obj$outputs$last_y
  if(parse == TRUE) y <- parse_y(y, xds_obj)
  return(y)
}

#' @title Set the initial values to the last values of the last simulation
#' @param xds_obj a [list]
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** model object
#' @export
last_to_inits <- function(xds_obj){
  xds_obj <- update_inits(xds_obj, xds_obj$outputs$last_y)
  return(xds_obj)
}

#' @title Set the initial values to the last values of the last simulation
#' @param vars a [list]
#' @return `vars` as an unnamed [vector]
#' @export
xds_flatten <- function(vars){
  return(unname(as.vector(unlist(vars))))
}

#' @title Run Checks 
#' 
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** model object
#' @export
check_models <- function(xds_obj) {
  for(i in 1:xds_obj$nHostSpecies)
    xds_obj <- check_XH(xds_obj, i)
  
  for(s in 1:xds_obj$nVectorSpecies){
    xds_obj <- check_MY(xds_obj, s)
    xds_obj <- check_L(xds_obj, s)
  }
  return(xds_obj)
}
  
#' Print the **`xds`** model object
#'
#' @inheritParams base::print
#' @rdname print 
#' @returns a description of the model
#' @export print.xds_obj
#' @export 
print.xds_obj = function(x, ...){
  print("HUMAN / HOST",  quote=FALSE)
  print(c("# Species:   ", x$nHostSpecies), quote=FALSE)
  print("",  quote=FALSE)
  print(c("X Module:    ", x$Xname), quote=FALSE)
  print(c("# Strata:    ", x$nStrata), quote=FALSE)
  print("",  quote=FALSE)
  print("VECTORS",  quote=FALSE)
  print(c("# Species:    ", x$nVectorSpecies), quote=FALSE)
  print("",  quote=FALSE)
  print(c("MY Module:  ", x$MYname), quote=FALSE)
  print(c("# Patches:   ", x$nPatches), quote=FALSE)
  print("",  quote=FALSE)
  print(c("L Module:    ", x$Lname), quote=FALSE)
  print(c("# Habitats:  ", x$nHabitats), quote=FALSE)
}
