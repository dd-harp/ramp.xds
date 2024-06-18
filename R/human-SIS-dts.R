

#' @title Derivatives for human population
#' @description Implements [dXdt] for the SIS_dts model, no demography.
#' @inheritParams dXdt
#' @return a [numeric] vector
#' @export
dXdt.SIS_dts <- function(t, y, pars, i) {

  attack <- pars$AR[[i]]

  with(list_Xvars(y, pars, i),{
    H <- F_H(t, y, pars, i)
    with(pars$Xpar[[i]], {

      St <- (1-attack)*S + r*I + dHdt(t, S, pars, i) + Births(t, H, pars, i)
      It <- (1-r)*I + ar*S + dHdt(t, I, pars, i)


      return(c(St, It))
    })
  })
}

#' @title Make parameters for SIS_dts human model, with defaults
#' @param nStrata is the number of population strata
#' @param Xopts a [list] that could overwrite defaults
#' @param b transmission probability (efficiency) from mosquito to human
#' @param c transmission probability (efficiency) from human to mosquito
#' @param r recovery rate
#' @return a [list]
#' @export
make_Xpar_SIS_dts = function(nStrata, Xopts=list(),
                         b=0.55, r=1/180, c=0.15){
  with(Xopts,{
    Xpar = list()
    class(Xpar) <- "SIS_dts"

    Xpar$b = checkIt(b, nStrata)
    Xpar$c = checkIt(c, nStrata)
    Xpar$r = checkIt(r, nStrata)

    return(Xpar)
})}

#' @title Setup Xpar.SIS_dts
#' @description Implements [setup_Xpar] for the SIS_dts model
#' @inheritParams setup_Xpar
#' @return a [list] vector
#' @export
setup_Xpar.SIS_dts = function(Xname, pars, i, Xopts=list()){
  pars$Xpar[[i]] = make_Xpar_SIS_dts(pars$Hpar[[i]]$nStrata, Xopts)
  return(pars)
}

# specialized methods for the human SIS_dts model

#' @title Size of effective infectious human population
#' @description Implements [F_X] for the SIS_dts model.
#' @inheritParams F_X
#' @return a [numeric] vector of length `nStrata`
#' @export
F_X.SIS_dts <- function(t, y, pars, i) {
  I = y[pars$ix$X[[i]]$I_ix]
  X = with(pars$Xpar[[i]], c*I)
  return(X)
}

#' @title Size of effective infectious human population
#' @description Implements [F_H] for the SIS_dts model.
#' @inheritParams F_H
#' @return a [numeric] vector of length `nStrata`
#' @export
F_H.SIS_dts <- function(t, y, pars, i){
  with(list_Xvars(y, pars, i), return(S+I))
}


#' @title Infection blocking pre-erythrocytic immunity
#' @description Implements [F_b] for the SIS_dts model.
#' @inheritParams F_b
#' @return a [numeric] vector of length `nStrata`
#' @export
F_b.SIS_dts <- function(y, pars, i) {
  with(pars$Xpar[[i]], b)
}

#' @title Make initial values for the SIS_dts human model, with defaults
#' @param nStrata the number of strata in the model
#' @param Xopts a [list] to overwrite defaults
#' @param H0 the initial human population density
#' @param S0 the initial values of the parameter S
#' @param I0 the initial values of the parameter I
#' @return a [list]
#' @export
make_Xinits_SIS_dts = function(nStrata, Xopts = list(), H0=NULL, S0=NULL, I0=1){with(Xopts,{
  if(is.null(S0)) S0 = H0 - I0
  stopifnot(is.numeric(S0))
  S = checkIt(S0, nStrata)
  I = checkIt(I0, nStrata)
  return(list(S=S, I=I))
})}


#' @title Return the variables as a list
#' @description This method dispatches on the type of `pars$Xpar`
#' @inheritParams list_Xvars
#' @return a [list]
#' @export
list_Xvars.SIS_dts <- function(y, pars, i) {
  with(pars$ix$X[[i]],
      return(list(
        S = y[S_ix],
        I = y[I_ix]
      )
  ))
}

#' @title Setup Xinits.SIS_dts
#' @description Implements [setup_Xinits] for the SIS_dts model
#' @inheritParams setup_Xinits
#' @return a [list] vector
#' @export
setup_Xinits.SIS_dts = function(pars, i, Xopts=list()){
  pars$Xinits[[i]] = with(pars,make_Xinits_SIS_dts(pars$Hpar[[i]]$nStrata, Xopts, H0=Hpar[[i]]$H))
  return(pars)
}

#' @title Add indices for human population to parameter list
#' @description Implements [make_indices_X] for the SIS_dts model.
#' @inheritParams make_indices_X
#' @return none
#' @importFrom utils tail
#' @export
make_indices_X.SIS_dts <- function(pars, i) {with(pars,{

  S_ix <- seq(from = max_ix+1, length.out=Hpar[[i]]$nStrata)
  max_ix <- tail(S_ix, 1)

  I_ix <- seq(from = max_ix+1, length.out=Hpar[[i]]$nStrata)
  max_ix <- tail(I_ix, 1)

  pars$max_ix = max_ix
  pars$ix$X[[i]] = list(S_ix=S_ix, I_ix=I_ix)
  return(pars)
})}

#' @title Return initial values as a vector
#' @description This method dispatches on the type of `pars$Xpar`.
#' @inheritParams get_inits_X
#' @return a [numeric] vector
#' @export
get_inits_X.SIS_dts <- function(pars, i){
  with(pars$Xinits[[i]], return(c(S,I)))
}

#' @title Update inits for the SIS_dts human model from a vector of states
#' @inheritParams update_inits_X
#' @return none
#' @export
update_inits_X.SIS_dts <- function(pars, y0, i) {
  with(list_Xvars(y0, pars, i),{
    pars$Xinits[[i]] = make_Xinits_SIS_dts(pars, list(), S0=S, I0=I)
    return(pars)
})}


#' @title Parse the output of deSolve and return variables for the SIS_dts model
#' @description Implements [parse_outputs_X] for the SIS_dts model
#' @inheritParams parse_outputs_X
#' @return none
#' @export
parse_outputs_X.SIS_dts <- function(outputs, pars, i) {
  time = outputs[,1]
  with(pars$ix$X[[i]],{
    S = outputs[,S_ix+1]
    I = outputs[,I_ix+1]
    H = S+I
    return(list(time=time, S=S, I=I, H=H))
})}

#' @title Compute the HTC for the SIS_dts model
#' @description Implements [HTC] for the SIS_dts model with demography.
#' @inheritParams HTC
#' @return a [numeric] vector
#' @export
HTC.SIS_dts <- function(pars, i) {
  with(pars$Xpar[[i]],
    return(c/r)
  )
}

#' @title Compute the "true" prevalence of infection / parasite rate
#' @description Implements [F_pr] for the SIS_dts model.
#' @inheritParams F_pr
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pr.SIS_dts <- function(varslist, pars, i) {
  pr = with(varslist$XH[[i]], I/H)
  return(pr)
}

#' Plot the density of infected individuals for the SIS_dts model
#'
#' @inheritParams xds_plot_X
#' @export
xds_plot_X.SIS_dts = function(pars, i=1, clrs=c("darkblue","darkred"), llty=1, stable=FALSE, add_axes=TRUE){
  vars=with(pars$outputs,if(stable==TRUE){stable_orbits}else{orbits})

  if(add_axes==TRUE)
    with(vars$XH[[i]],
         plot(time, 0*time, type = "n", ylim = c(0, max(H)),
              ylab = "# Infected", xlab = "Time"))

  add_lines_X_SIS_dts(vars$XH[[i]], pars$Hpar[[i]]$nStrata, clrs, llty)
}


#' Add lines for the density of infected individuals for the SIS_dts model
#'
#' @param XH a list with the outputs of parse_outputs_X_SIS_dts
#' @param nStrata the number of population strata
#' @param clrs a vector of colors
#' @param llty an integer (or integers) to set the `lty` for plotting
#'
#' @export
add_lines_X_SIS_dts = function(XH, nStrata, clrs=c("darkblue","darkred"), llty=1){
  with(XH,{
    if(nStrata==1) {
      lines(time, S, col=clrs[1], lty = llty[1])
      lines(time, I, col=clrs[2], lty = llty[1])
    }
    if(nStrata>1){
      if (length(clrs)==2) clrs=matrix(clrs, 2, nStrata)
      if (length(llty)==1) llty=rep(llty, nStrata)

      for(i in 1:nStrata){
        lines(time, S[,i], col=clrs[1,i], lty = llty[i])
        lines(time, I[,i], col=clrs[2,i], lty = llty[i])
      }
    }
  })}


#' @title Make parameters for SIS_dts human model
#' @param pars a [list]
#' @param b transmission probability (efficiency) from mosquito to human
#' @param c transmission probability (efficiency) from human to mosquito
#' @param r recovery rate
#' @return a [list]
#' @export
make_parameters_X_SIS_dts <- function(pars, b, c, r) {
  stopifnot(is.numeric(b), is.numeric(c), is.numeric(r))
  Xpar <- list()
  class(Xpar) <- c('SIS_dts', 'SIS_dtsdX')
  Xpar$b <- b
  Xpar$c <- c
  Xpar$r <- r
  pars$Xpar[[1]] <- Xpar
  return(pars)
}


#' @title Make inits for SIS_dts human model
#' @param pars a [list]
#' @param S0 size of infected population in each strata
#' @param I0 size of infected population in each strata
#' @return none
#' @export
make_inits_X_SIS_dts <- function(pars, S0, I0) {
  stopifnot(is.numeric(S0))
  stopifnot(is.numeric(I0))
  pars$Xinits[[1]] <- list(S=S0, I=I0)
  return(pars)
}
