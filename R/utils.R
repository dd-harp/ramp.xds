
#' @title Set indices for generalized spatial model
#' @param pars a [list]
#' @return none
#' @export
make_indices <- function(pars) {
  pars$max_ix <- 0

  s = length(pars$Linits)
  if(s>0)
    for(ix in 1:s)
      pars = make_indices_L(pars, ix)

  s = length(pars$MYZinits)
  if(s>0)
    for(ix in 1:s)
      pars = make_indices_MYZ(pars, ix)

  i = length(pars$Xinits)
  if(i>0)
    for(ix in 1:i)
      pars = make_indices_X(pars, ix)

  return(pars)
}

#' @title Get the initial values as a vector
#' @param pars a [list]
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_inits <- function(pars){

  Li = c()
  s = length(pars$Lpar)
  if(s>0)
    for(ix in 1:s)
      Li = c(Li, get_inits_L(pars, ix))

  MYZi = c()
  s = length(pars$MYZpar)
  if(s>0)
    for(ix in 1:s)
      MYZi = c(MYZi, get_inits_MYZ(pars, ix))

  Xi = c()
  i = length(pars$Xpar)
  if(i>0)
    for(ix in 1:i)
      Xi = c(Xi, get_inits_X(pars, ix))
  y = c(L=Li, MYZ=MYZi, X=Xi)

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
#' @param a a [numeric] object
#' @param b a [numeric] object
#' @param tol the numeric tolerance
#' @return a [logical] value
#' @export
approx_equal <- function(a, b, tol = sqrt(.Machine$double.eps)) {
  abs(a - b) < tol
}

#' @title Check the length of an input value
#' @param x a [numeric] object
#' @param lng a [numeric] object
#' @param type a [character] string specifying required typeof
#' @param fixit a [logical] value, if TRUE force length to lng
#' @return a [numeric] object
#' @export
checkIt = function(x, lng, type = "numeric", fixit=TRUE){
  stopifnot(is.numeric(x))
  if(type == "integer") x = as.integer(x)
  if(length(x)==1 & fixit) x=rep(x, lng)
  stopifnot(length(x)==lng)
  x
}

#' @title Check the shape and dimensions of an object
#' @param obj a [numeric] object
#' @param d1 an [integer]
#' @param d2 an [integer]
#' @return [matrix]
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
#' @param pars a [list]
#' @param s the vector species index
#' @param i the host species index
#' @export
list_vars <- function(pars, y0=NULL, s=1, i=1){
  if(is.null(y0)) y0=get_inits(pars)
  y0 = unname(y0)
  c(
  list_Lvars(y0, pars, s),
  list_MYZvars(y0, pars, s),
  list_Xvars(y0, pars, s))
}

#' @title Set the initial values to the last values of the last simulation
#' @param y0 a [vector] of initial values
#' @param pars a [list]
#' @return y a [numeric] vector
#' @export
update_inits <- function(y0, pars){
  s = length(pars$Lpar)
  if(s>0)
    for(ix in 1:s)
      pars = update_inits_L(pars, y0, ix)

  s = length(pars$MYZpar)
  if(s>0)
    for(ix in 1:s)
      pars = update_inits_MYZ(pars, y0, ix)

  ii = length(pars$Xpar)
  if(ii>0)
    for(ix in 1:ii)
      pars = update_inits_X(pars, y0, ix)

  return(pars)
}

#' @title Set the initial values to the last values of the last simulation
#' @param pars a [list]
#' @return y a [numeric] vector
#' @export
last_to_inits <- function(pars){
  pars <- update_inits(pars$outputs$orbits$y_last, pars)
  return(pars)
}
