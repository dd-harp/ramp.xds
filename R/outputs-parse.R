
#' @title Parse the output of an object returned by deSolve
#' @param y a [vector] with the variables
#' @param pars an `xds` object
#' @return varslist a [list]
#' @export
parse_y <- function(y, pars){

  vals = list()

  s = length(pars$Lpar)
  if(s>0){
    vals$L = list()
    for(ix in 1:s)
      vals$L[[ix]] = list_Lvars(y, pars, ix)
  }

  s = length(pars$MYZpar)
  if(s>0){
    vals$MYZ = list()
    for(ix in 1:s)
      vals$MYZ[[ix]] = list_MYZvars(y, pars, ix)
  }

  s = length(pars$Xpar)
  if(s>0){
    vals$X  = list()
    for(ix in 1:ix)
      vals$XH[[ix]] = list_Xvars(y, pars, ix)
  }
  return(vals)
}

#' @title Parse the outputs of an object created by xde_solve or dts_solve
#' @param pars a [list]
#' @param outputs a [matrix] of orbits returned by deSolve
#' @return varslist a [list]
#' @export
parse_orbits <- function(outputs, pars){
  varslist = list()

  s = length(pars$Lpar)
  if(s>0){
    varslist$L = list()
    for(ix in 1:s)
      varslist$L[[ix]] = parse_Lorbits(outputs, pars, ix)
  }

  s = length(pars$MYZpar)
  if(s>0){
    varslist$MYZ = list()
    for(ix in 1:s)
      varslist$MYZ[[ix]]= parse_MYZorbits(outputs, pars, ix)
  }

  s = length(pars$Xpar)
  if(s>0){
    varslist$XH = list()
    for(ix in 1:s)
      varslist$XH[[ix]] = parse_Xorbits(outputs, pars, ix)
  }

  return(varslist)
}

