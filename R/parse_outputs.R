
#' @title Parse the outputs of an object created by xde_solve or dts_solve
#' @param pars a [list]
#' @param deout a [matrix] of orbits returned by deSolve
#' @return varslist a [list]
#' @export
parse_outputs <- function(deout, pars){
  varslist = list()

  s = length(pars$Lpar)
  if(s>0)
    for(ix in 1:s)
      varslist$L[[ix]]= parse_outputs_L(deout, pars, ix)

  s = length(pars$MYZpar)
  if(s>0)
    for(ix in 1:s)
      varslist$MYZ[[ix]]= parse_outputs_MYZ(deout, pars, ix)

  s = length(pars$Xpar)
  if(s>0)
    for(ix in 1:s)
      varslist$XH[[ix]]= parse_outputs_X(deout, pars, ix)
  varslist$terms = compute_terms(varslist, deout, pars, 1, 1)
  varslist$deout = deout
  varslist$y_last = tail(deout, 1)[-1]
  return(varslist)
}

#' @title Parse the output of an object returned by deSolve
#' @param vec a [vector] with the variables, as returned by rootsolve
#' @param pars a [list]
#' @return varslist a [list]
#' @export
parse_outputs_vec <- function(vec, pars){
  deout = rbind(c(0,vec), c(0,vec))
  varslist = parse_outputs(deout, pars)

  for(i in 1:length(varslist$XH))
    varslist$XH[[i]] = tail(varslist$XH[[i]],1)
  for(i in 1:length(varslist$MYZ))
    varslist$MYZ[[i]] = tail(varslist$MYZ[[i]],1)
  for(i in 1:length(varslist$L))
    varslist$L[[i]] = tail(varslist$L[[i]],1)

  varslist$terms = compute_terms_steady(varslist, vec, pars)
  return(varslist)
}


