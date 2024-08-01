# The SEIS

#' @title Derivatives for the SEIS model for human / vertebrate host infections
#' @description Implements [dXdt] for the SEIS model
#' @inheritParams dXdt
#' @return a [numeric] vector
#' @export
dXdt.SEIS <- function(t, y, pars, i) {

  foi <- pars$FoI[[i]]
  Hpar <- pars$Hpar[[i]]
  with(list_Xvars(y, pars, i),{
    with(pars$Xpar[[i]], {
      dS <- Births(t, H, Hpar) - foi*S + r*I + dHdt(t, S, Hpar)
      dE <- foi*S - nu*E + dHdt(t, E, Hpar)
      dI <- nu*E - r*I + dHdt(t, I, Hpar)
      return(c(dS, dE, dI))
    })
  })
}

#' @title DTS updating for the SEIS model for human / vertebrate host infections
#' @description Implements [Update_Xt] for the SEIS model
#' @inheritParams Update_Xt
#' @return a [numeric] vector
#' @export
Update_Xt.SEIS <- function(t, y, pars, i) {

  ar <- pars$AR[[i]]
  Hpar <- pars$Hpar[[i]]
  with(list_Xvars(y, pars, i),{
    with(pars$Xpar[[i]], {

      St <- (1-ar)*S + (1-nr)*(1-ar)*I + dHdt(t, S, Hpar) + Births(t, H, Hpar)
      Et <- ar*S + (1-nr)*ar*I + nu*E + dHdt(t, E, Hpar)
      It <- nr*I + (1-nu)*E + dHdt(t, I, Hpar)

      return(c(S=unname(St), I=unname(It)))
    })
  })
}

#' @title Setup Xpar.SEIS
#' @description Implements [make_Xpar] for the SEIS model
#' @inheritParams make_Xpar
#' @return a [list] vector
#' @export
make_Xpar.SEIS = function(Xname, pars, i, Xopts=list()){
  pars$Xpar[[i]] = create_Xpar_SEIS(pars$nStrata[i], Xopts)
  return(pars)
}

#' @title Make parameters for SEIS xde human model, with defaults
#' @param nStrata is the number of population strata
#' @param Xopts a [list] that could overwrite defaults
#' @param b transmission probability (efficiency) from mosquito to human
#' @param r recovery rate
#' @param nu 1/latent period
#' @param c transmission probability (efficiency) from human to mosquito
#' @return a [list]
#' @export
create_Xpar_SEIS = function(nStrata, Xopts=list(),
                             b=0.55, r=1/180, nu=1/14, c=0.15){
  with(Xopts,{
    Xpar = list()
    class(Xpar) <- "SEIS"

    Xpar$b = checkIt(b, nStrata)
    Xpar$c = checkIt(c, nStrata)
    Xpar$r = checkIt(r, nStrata)
    Xpar$nu = checkIt(nu, nStrata)

    return(Xpar)
  })}




# specialized methods for the human SEIS model

#' @title Size of effective infectious human population
#' @description Implements [F_X] for the SEIS model.
#' @inheritParams F_X
#' @return a [numeric] vector of length `nStrata`
#' @export
F_X.SEIS <- function(y, pars, i) {
  I = y[pars$ix$X[[i]]$I_ix]
  X = with(pars$Xpar[[i]], c*I)
  return(X)
}

#' @title Size of effective infectious human population
#' @description Implements [F_H] for the SEIS model.
#' @inheritParams F_H
#' @return a [numeric] vector of length `nStrata`
#' @export
F_H.SEIS <- function(y, pars, i){
  with(list_Xvars(y, pars, i), return(H))
}


#' @title Infection blocking pre-erythrocytic immunity
#' @description Implements [F_b] for the SEIS model.
#' @inheritParams F_b
#' @return a [numeric] vector of length `nStrata`
#' @export
F_b.SEIS <- function(y, pars, i) {
  with(pars$Xpar[[i]], b)
}

#' @title Make initial values for the SEIS xde human model, with defaults
#' @param nStrata the number of strata in the model
#' @param H0 the initial human population density
#' @param Xopts a [list] to overwrite defaults
#' @param E the initial values of the parameter E
#' @param I the initial values of the parameter I
#' @return a [list]
#' @export
create_Xinits_SEIS = function(nStrata, H0, Xopts = list(), E=0, I=1){with(Xopts,{
  S = checkIt(H0-E-I, nStrata)
  E = checkIt(E, nStrata)
  I = checkIt(I, nStrata)
  return(list(S=S, E=E, I=I))
})}




#' @title Return the SEIS model variables as a list, returned from Update_Xt.SISd
#' @description This method dispatches on the type of `pars$Xpar`
#' @inheritParams put_Xvars
#' @return a [list]
#' @export
put_Xvars.SEIS <- function(Xvars, y, pars, i) {
  with(pars$ix$X[[i]],
       with(as.list(Xvars),{
         y[S_ix] = S
         y[E_ix] = E
         y[I_ix] = I
         return(y)
       }))}

#' @title Return the variables as a list
#' @description This method dispatches on the type of `pars$Xpar`
#' @inheritParams list_Xvars
#' @return a [list]
#' @export
list_Xvars.SEIS <- function(y, pars, i) {
  with(pars$ix$X[[i]],{
    S = y[S_ix]
    E = y[E_ix]
    I = y[I_ix]
    H = S+E+I
    return(list(S=S,E=E,I=I,H=H))})
}


#' @title Setup Xinits.SEIS
#' @description Implements [make_Xinits] for the SEIS model
#' @inheritParams make_Xinits
#' @return a [list] vector
#' @export
make_Xinits.SEIS = function(pars, H, i, Xopts=list()){
  pars$Xinits[[i]] = with(pars, create_Xinits_SEIS(pars$nStrata[i], H, Xopts))
  return(pars)
}

#' @title Add indices for human population to parameter list
#' @description Implements [make_X_indices] for the SEIS model.
#' @inheritParams make_X_indices
#' @return none
#' @importFrom utils tail
#' @export
make_X_indices.SEIS <- function(pars, i) {with(pars,{

  S_ix <- seq(from = max_ix+1, length.out=nStrata[i])
  max_ix <- tail(S_ix, 1)

  E_ix <- seq(from = max_ix+1, length.out=nStrata[i])
  max_ix <- tail(E_ix, 1)

  I_ix <- seq(from = max_ix+1, length.out=nStrata[i])
  max_ix <- tail(I_ix, 1)

  pars$max_ix = max_ix
  pars$ix$X[[i]] = list(S_ix=S_ix, E_ix=E_ix, I_ix=I_ix)
  return(pars)
})}

#' @title Return initial values as a vector
#' @description This method dispatches on the type of `pars$Xpar`.
#' @inheritParams get_Xinits
#' @return a [numeric] vector
#' @export
get_Xinits.SEIS <- function(pars, i){
  with(pars$Xinits[[i]], return(c(S=S,E=E,I=I)))
}

#' @title Update inits for the SEIS xde human model from a vector of states
#' @inheritParams update_Xinits
#' @return none
#' @export
update_Xinits.SEIS <- function(pars, y0, i) {
  with(list_Xvars(y0, pars, i),{
    pars$Xinits[[i]] = create_Xinits_SEIS(pars$nStrata[i], H, list(), E=E, I=I)
    return(pars)
  })}


#' @title Parse the output of deSolve and return variables for the SEIS model
#' @description Implements [parse_outputs_X] for the SEIS model
#' @inheritParams parse_outputs_X
#' @return none
#' @export
parse_outputs_X.SEIS <- function(outputs, pars, i) {
  time = outputs[,1]
  with(pars$ix$X[[i]],{
    S = outputs[,S_ix+1]
    E = outputs[,E_ix+1]
    I = outputs[,I_ix+1]
    H = S+I
    return(list(time=time, S=S, E=E, I=I, H=H))
  })}

#' @title Compute the HTC for the SEIS model
#' @description Implements [HTC] for the SEIS model with demography.
#' @inheritParams HTC
#' @return a [numeric] vector
#' @export
HTC.SEIS <- function(pars, i) {
  with(pars$Xpar[[i]],
       return(c/r)
  )
}

#' @title Compute the "true" prevalence of infection / parasite rate
#' @description Implements [F_pr] for the SEIS model.
#' @inheritParams F_pr
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pr.SEIS <- function(vars, Xpar) {
  pr = with(vars, I/H)
  return(pr)
}

#' @title Compute the prevalence of infection by light microscopy
#' @description Implements [F_pr] for the SEIS model.
#' @inheritParams F_pr
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pr_by_lm.SEIS <- function(vars, Xpar) {
  pr = with(vars, I/H)
  return(pr)
}

#' @title Compute the prevalence of infection by RDT
#' @description Implements [F_pr] for the SEIS model.
#' @inheritParams F_pr
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pr_by_rdt.SEIS <- function(vars, Xpar) {
  pr = with(vars, I/H)
  return(pr)
}

#' @title Compute the prevalence of infection by pcr
#' @description Implements [F_pr] for the SEIS model.
#' @inheritParams F_pr
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pr_by_pcr.SEIS <- function(vars, Xpar) {
  pr = with(vars, I/H)
  return(pr)
}

#' Plot the density of infected individuals for the SEIS model
#'
#' @inheritParams xds_plot_X
#' @export
xds_plot_X.SEIS = function(pars, i=1, clrs=c("darkblue","darkred"), llty=1, stable=FALSE, add_axes=TRUE){
  vars=with(pars$outputs,if(stable==TRUE){stable_orbits}else{orbits})

  if(add_axes==TRUE)
    with(vars$XH[[i]],
         plot(time, 0*time, type = "n", ylim = c(0, max(H)),
              ylab = "# Infected", xlab = "Time"))


  add_lines_X_SEIS(vars$XH[[i]], pars$nStrata[i], clrs, llty)
}

#' Add lines for the density of infected individuals for the SEIS model
#'
#' @param XH a list with the outputs of parse_outputs_X_SEIS
#' @param nStrata the number of population strata
#' @param clrs a vector of colors
#' @param llty an integer (or integers) to set the `lty` for plotting
#'
#' @export
add_lines_X_SEIS = function(XH, nStrata, clrs=c("darkblue","darkred"), llty=1){
  with(XH,{
    if(nStrata==1) {
      lines(time, S+E, col=clrs[1], lty = llty[1])
      lines(time, I, col=clrs[2], lty = llty[1])
    }
    if(nStrata>1){
      if (length(clrs)==2) clrs=matrix(clrs, 2, nStrata)
      if (length(llty)==1) llty=rep(llty, nStrata)

      for(i in 1:nStrata){
        lines(time, S[,i]+E[,i], col=clrs[1,i], lty = llty[i])
        lines(time, I[,i], col=clrs[2,i], lty = llty[i])
      }
    }
  })}

#' @title Compute the steady states for the SEIS model as a function of the daily EIR
#' @description Compute the steady state of the SEIS model as a function of the daily eir.
#' @inheritParams xde_steady_state_X
#' @return the steady states as a named vector
#' @export
xde_steady_state_X.SEIS = function(foi, H, Xpar){with(Xpar,{
  Ieq = (foi*H*nu)/(foi*(r+nu) +r*nu)
  Eeq = (foi*H*r)/(foi*(r+nu) +r*nu)
  Seq = H- Ieq-Eeq
  return(c(S=Seq, E=Eeq, I=Ieq, H=H))
})}

