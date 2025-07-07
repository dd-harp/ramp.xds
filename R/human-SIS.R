# specialized methods for the human SIS model

#' @title **X** Component Derivatives for the `SIS` Model
#' @description
#' Compute the derivatives for SIS compartmental model. Here, the model includes human demographic changes,
#' and it is computed in an equivalent form:
#' \deqn{
#' \begin{array}{rl}
#' dH/dt = & B(t,H)  + {\cal D} \cdot H \\
#' dI/dt = & h (H-I) - r I - \xi(t) + {\cal D} \cdot I
#' \end{array}
#' }
#' where \eqn{S=H-I} ; 
#' \eqn{\xi(t)} is a function to simulate mass treatment;
#' \eqn{B(t, H)} is the time-dependent birth rate; and the \eqn{\cal D} is a matrix describing demographic changes,
#' including mortality, migration, and aging; 
#' @inheritParams dXdt
#' @return a [numeric] vector
#' @export
dXdt.SIS <- function(t, y, pars, i) {

  foi <- pars$FoI[[i]]
  Hpar <- pars$Hpar[[i]]
  with(list_Xvars(y, pars, i),{
    with(pars$Xpar[[i]], {
      dH <- Births(t, H, Hpar) + dHdt(t, H, Hpar)
      dI <- foi*(H-I) - r*I - F_treat(t)/H*I + dHdt(t, I, Hpar)
      return(c(dH, dI))
    })
  })
}

#' @title Compute the steady states for the SIS model as a function of the daily EIR
#' @description Compute the steady state of the SIS model as a function of the daily eir.
#' @inheritParams xde_steady_state_X
#' @return the steady states as a named vector
#' @export
xde_steady_state_X.SIS = function(foi, H, Xpar){with(Xpar,{
  Ieq = foi/(foi+r)*H
  Seq = H-Ieq
  return(list(S=Seq, I=Ieq))
})}

#' @title DTS updating for the SIS model for human / vertebrate host infections
#' @description Implements [Update_Xt] for the SIS model
#' @inheritParams Update_Xt
#' @return a [numeric] vector
#' @export
Update_Xt.SIS <- function(t, y, pars, i) {

  ar <- pars$AR[[i]]
  Hpar <- pars$Hpar[[i]]
  with(list_Xvars(y, pars, i),{
    with(pars$Xpar[[i]], {

      St <- (1-ar)*S + (1-nr)*(1-ar)*I + dHdt(t, S, Hpar) + Births(t, H, Hpar)
      It <- nr*I + (1-nr)*ar*I + ar*S + dHdt(t, I, Hpar)

      return(c(S=unname(St), I=unname(It)))
    })
  })
}

#' @title Compute the steady states for the  dts SIS model as a function of the daily EIR
#' @description Compute the steady state of the  dts SIS model as a function of the daily eir.
#' @inheritParams dts_steady_state_X
#' @return the steady states as a named vector
#' @export
dts_steady_state_X.SIS = function(ar, H, Xpar){with(Xpar,{
  dIeq = ar*H/(1-(1-ar)*nr)
  dSeq = ((1-ar)*(1-nr)*H)/(1-(1-ar)*nr)
  return(c(S=dSeq, I=dIeq))
})}

#' @title Setup Xpar.SIS
#' @description Implements [setup_Xpar] for the SIS model
#' @inheritParams setup_Xpar
#' @return a [list] vector
#' @export
setup_Xpar.SIS = function(Xname, pars, i, Xopts=list()){
  pars$Xpar[[i]] = make_Xpar_SIS(pars$nStrata[1], Xopts)
  return(pars)
}

#' @title Make parameters for SIS xde human model, with defaults
#' @param nStrata is the number of population strata
#' @param Xopts a [list] that could overwrite defaults
#' @param b transmission probability (efficiency) from mosquito to human
#' @param c transmission probability (efficiency) from human to mosquito
#' @param r recovery rate
#' @return a [list]
#' @export
make_Xpar_SIS = function(nStrata, Xopts=list(),
                           b=0.55, r=1/180, c=0.15){
  with(Xopts,{
    Xpar = list()
    class(Xpar) <- "SIS"

    Xpar$b = checkIt(b, nStrata)
    Xpar$c = checkIt(c, nStrata)
    Xpar$r = checkIt(r, nStrata)
    Xpar$F_treat = F_zero 

    return(Xpar)
  })}


#' @title Size of effective infectious human population
#' @description Implements [F_X] for the SIS model.
#' @inheritParams F_X
#' @return a [numeric] vector of length `nStrata`
#' @export
F_X.SIS <- function(t, y, pars, i) {
  I = y[pars$ix$X[[i]]$I_ix]
  X = with(pars$Xpar[[i]], c*I)
  return(X)
}

#' @title Size of effective infectious human population
#' @description Implements [F_H] for the SIS model.
#' @inheritParams F_H
#' @return a [numeric] vector of length `nStrata`
#' @export
F_H.SIS <- function(t, y, pars, i){
  with(list_Xvars(y, pars, i), return(H))
}

#' @title Infection blocking pre-erythrocytic immunity
#' @description Implements [F_b] for the SIS model.
#' @inheritParams F_b
#' @return a [numeric] vector of length `nStrata`
#' @export
F_b.SIS <- function(y, pars, i) {
  with(pars$Xpar[[i]], b)
}

#' @title Return the SIS model variables as a list, returned from Update_Xt.SIS
#' @description This method dispatches on the type of `pars$Xpar`
#' @inheritParams put_Xvars
#' @return a [list]
#' @export
put_Xvars.SIS <- function(Xvars, y, pars, i) {
  with(pars$ix$X[[i]],
       with(as.list(Xvars),{
         y[H_ix] = H
         y[I_ix] = I
         return(y)
       }))}

#' @title Return the variables as a list
#' @description This method dispatches on the type of `pars$Xpar`
#' @inheritParams list_Xvars
#' @return a [list]
#' @export
list_Xvars.SIS <- function(y, pars, i) {
  with(pars$ix$X[[i]],{
    H = y[H_ix]
    I = y[I_ix]
    S = H-I
    return(list(S=S,I=I,H=H))})
}

#' @title Return the parameters as a list
#' @description Parameter values for the \eqn{i^{th}} host are
#' stored as `pars$Xpar[[i]]`. This returns the stored parameter
#' values as a list.
#' @inheritParams get_Xpars
#' @return a [list]
#' @export
get_Xpars.SIS <- function(pars, i=1) {
  with(pars$Xpar[[i]],list(b=b, c=c, r=r))
}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `pars$Xpar[[i]]`.
#' @inheritParams set_Xpars
#' @return an **`xds`** object
#' @export
set_Xpars.SIS <- function(pars, i=1, Xopts=list()) {
  nHabitats <- pars$nHabitats
  with(pars$Xpar[[i]], with(Xopts,{
    pars$Xpar[[i]]$b <- b
    pars$Xpar[[i]]$c <- c
    pars$Xpar[[i]]$r <- r
    return(pars)
  }))}



#' @title Make initial values for the SIS xde human model, with defaults
#' @param nStrata the number of strata in the model
#' @param H the initial human population density
#' @param Xopts a [list] to overwrite defaults
#' @param I the initial values of the parameter I
#' @return a [list]
#' @export
make_Xinits_SIS = function(nStrata, H, Xopts = list(), I=1){with(Xopts,{
  I = unname(as.vector(checkIt(I, nStrata)))
  return(list(H=H, I=I))
})}




#' @title Setup Xinits.SIS
#' @description Implements [setup_Xinits] for the SIS model
#' @inheritParams setup_Xinits
#' @return a [list] vector
#' @export
setup_Xinits.SIS = function(pars, H, i, Xopts=list()){
  pars$Xinits[[i]] = with(pars, make_Xinits_SIS(pars$nStrata[i], H, Xopts))
  return(pars)
}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `pars$Xpar[[i]]`.
#' @inheritParams set_Xinits
#' @return an **`xds`** object
#' @export
set_Xinits.SIS <- function(pars, i=1, Xopts=list()) {
  with(get_Xinits(pars, i), with(Xopts,{
    pars$Xinits[[i]]$I = I
    return(pars)
  }))}

#' @title Return initial values as a vector
#' @description This method dispatches on the type of `pars$Xpar`
#' @inheritParams get_Xinits
#' @return a [list]
#' @export
get_Xinits.SIS <- function(pars, i=1){pars$Xinits[[i]]}


#' @title Add indices for human population to parameter list
#' @description Implements [setup_Xix] for the SIS model.
#' @inheritParams setup_Xix
#' @return none
#' @importFrom utils tail
#' @export
setup_Xix.SIS <- function(pars, i) {with(pars,{

  H_ix <- seq(from = max_ix+1, length.out=nStrata[i])
  max_ix <- tail(H_ix, 1)

  I_ix <- seq(from = max_ix+1, length.out=nStrata[i])
  max_ix <- tail(I_ix, 1)

  pars$max_ix = max_ix
  pars$ix$X[[i]] = list(H_ix=H_ix, I_ix=I_ix)
  return(pars)
})}

#' @title Update inits for the SIS xde human model from a vector of states
#' @inheritParams update_Xinits
#' @return an `xds` object
#' @export
update_Xinits.SIS <- function(pars, y, i=1) {
  with(list_Xvars(y, pars, i),{
    pars$Xinits[[i]]$H = get_H(pars, i)
    pars$Xinits[[i]]$I = I
    return(pars)
  })}


#' @title Parse the output of deSolve and return variables for the SIS model
#' @description Implements [parse_Xorbits] for the SIS model
#' @inheritParams parse_Xorbits
#' @return none
#' @export
parse_Xorbits.SIS <- function(outputs, pars, i) {with(pars$ix$X[[i]],{
  H <- outputs[,H_ix]
  I <- outputs[,I_ix]
  S <- H-I
  ni <- pars$Xpar[[i]]$c*I/H
  true_pr <- I/H
  vars <- list(S=S, I=I, H=H, ni=ni, true_pr=true_pr)
  return(vars)
})}

#' @title Compute the net infectiousness
#' @description Implements [F_ni] for the SIS model.
#' @inheritParams F_ni
#' @return a [numeric] vector of length `nStrata`
#' @export
F_ni.SIS <- function(vars, Xpar) {
  pr = with(vars, Xpar$c*I/H)
  return(pr)
}

#' @title Compute the prevalence of infection 
#' @description Implements [F_prevalence] for the SIS model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @export
F_prevalence.SIS <- function(vars, Xpar) {
  pr = with(vars, I/H)
  return(pr)
}

#' @title Compute *Pf*PR by light microscopy
#' @description Implements [F_prevalence] for the SIS model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pfpr_by_lm.SIS <- function(vars, Xpar) {
  pr = with(vars, I/H)
  return(pr)
}

#' @title Compute *Pf*PR by RDT 
#' @description Implements [F_prevalence] for the SIS model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pfpr_by_rdt.SIS <- function(vars, Xpar) {
  pr = with(vars, I/H)
  return(pr)
}

#' @title Compute *Pf*PR by PCR 
#' @description Implements [F_prevalence] for the SIS model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pfpr_by_pcr.SIS <- function(vars, Xpar) {
  pr = with(vars, I/H)
  return(pr)
}

#' Plot the density of infected individuals for the SIS model
#'
#' @inheritParams xds_plot_X
#' @export
xds_plot_X.SIS = function(pars, i=1, clrs=c("darkblue","darkred"), llty=1, add=FALSE){
  XH = pars$outputs$orbits$XH[[i]]
  time = pars$outputs$time

  if(add==FALSE)
    plot(time, 0*time, type = "n", ylim = c(0, max(XH$H)),
         ylab = "# Infected", xlab = "Time")

  add_lines_X_SIS(time, XH, pars$nStrata[i], clrs, llty)
}

#' Add lines for the density of infected individuals for the SIS model
#'
#' @param time time points for the observations
#' @param XH parsed outputs, from parse_Xorbits_SIS
#' @param nStrata the number of population strata
#' @param clrs a vector of colors
#' @param llty an integer (or integers) to set the `lty` for plotting
#'
#' @export
add_lines_X_SIS = function(time, XH, nStrata, clrs=c("darkblue","darkred"), llty=1){
  if (length(llty)< nStrata) llty = rep(llty, nStrata)
  with(XH,{
    if(nStrata == 1){
        lines(time, S, col=clrs[1], lty = llty)
        lines(time, I, col=clrs[2], lty = llty)
    } else {
    for(i in 1:nStrata)
      lines(time, S[,i], col=clrs[1], lty = llty[i])
      lines(time, I[,i], col=clrs[2], lty = llty[i])
    }})
}

#' @title Compute the HTC for the SIS model
#' @description Implements [HTC] for the SIS model with demography.
#' @inheritParams HTC
#' @return a [numeric] vector
#' @export
HTC.SIS <- function(pars, i) {
  with(pars$Xpar[[i]],
       return(c/r)
  )
}
