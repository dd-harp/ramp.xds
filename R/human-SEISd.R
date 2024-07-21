# The delay SEIS model as a delay differential equation and delay difference equation

#' @title Derivatives for the SEISd model for human / vertebrate host infections
#' @description Implements [dXdt] for the SEISd model
#' @inheritParams dXdt
#' @return a [numeric] vector
#' @export
dXdt.SEISd <- function(t, y, pars, i) {

  foi <- pars$FoI[[i]]
  Hpar <- pars$Hpar[[i]]
  with(list_Xvars(y, pars, i),{
    with(pars$Xpar[[i]], {

      if (t <= nu) {
        S_nu = 0
        foi_nu = 0
      } else {
        S_nu   = lagvalue(t=t-nu, nr=pars$ix$X[[i]]$S_ix)
        foi_nu = lagderiv(t=t-nu, nr=pars$ix$X[[i]]$cfoi_ix)
      }

      dS <- Births(t, H, Hpar) - foi*S + r*I + dHdt(t, S, Hpar)
      dE <- foi*S - foi_nu*S_nu + dHdt(t, E, Hpar)
      dI <- foi_nu*S_nu - r*I + dHdt(t, I, Hpar)
      dcfoi <- foi

#      if (t > 20) browser()
      return(c(dS, dE, dI, dcfoi))
    })
  })
}

#' @title DTS updating for the SEISd model for human / vertebrate host infections
#' @description Implements [DT_Xt] for the SEISd model
#' @inheritParams DT_Xt
#' @return a [numeric] vector
#' @export
DT_Xt.SEISd <- function(t, y, pars, i) {

  ar <- pars$AR[[i]]
  Hpar <- pars$Hpar[[i]]
  with(list_Xvars(y, pars, i),{
    H <- F_H(y, pars, i)
    with(pars$Xpar[[i]], {

      St <- (1-ar)*S + (1-nr)*(1-ar)*I + dHdt(t, S, Hpar) + Births(t, H, Hpar)
      Et <- ar*S + (1-nr)*ar*I + nu*E + dHdt(t, E, Hpar)
      It <- nr*I + (1-nu)*E + dHdt(t, I, Hpar)

      return(c(S=unname(St), I=unname(It)))
    })
  })
}

#' @title Setup Xpar.SEISd
#' @description Implements [xde_setup_Xpar] for the SEISd model
#' @inheritParams xde_setup_Xpar
#' @return a [list] vector
#' @export
xde_setup_Xpar.SEISd = function(Xname, pars, i, Xopts=list()){
  pars$Xpar[[i]] = xde_make_Xpar_SEISd(pars$Hpar[[i]]$nStrata, Xopts)
  return(pars)
}

#' @title Make parameters for SEISd xde human model, with defaults
#' @param nStrata is the number of population strata
#' @param Xopts a [list] that could overwrite defaults
#' @param b transmission probability (efficiency) from mosquito to human
#' @param r recovery rate
#' @param nu 1/latent period
#' @param c transmission probability (efficiency) from human to mosquito
#' @return a [list]
#' @export
xde_make_Xpar_SEISd = function(nStrata, Xopts=list(),
                             b=0.55, r=1/180, nu=14, c=0.15){
  with(Xopts,{
    Xpar = list()
    class(Xpar) <- "SEISd"

    Xpar$b = checkIt(b, nStrata)
    Xpar$c = checkIt(c, nStrata)
    Xpar$r = checkIt(r, nStrata)
    Xpar$nu = checkIt(nu, nStrata)

    return(Xpar)
  })}


#' @title Setup Xpar for the discrete time SEISd model
#' @description Implements [dts_setup_Xpar] for the SEISd model
#' @inheritParams dts_setup_Xpar
#' @return a [list] vector
#' @export
dts_setup_Xpar.SEISd = function(Xname, pars, i, Xopts=list()){
  pars$Xpar[[i]] = dts_make_Xpar_SEISd(pars$Hpar[[i]]$nStrata, pars$Xday, Xopts)
  return(pars)
}

#' @title Make parameters for SEISd human model, with defaults
#' @param nStrata is the number of population strata
#' @param Xday the X component runtime time step
#' @param Xopts a [list] that could overwrite defaults
#' @param b transmission probability (efficiency) from mosquito to human
#' @param r recovery rate
#' @param nu 1/latent period
#' @param c transmission probability (efficiency) from human to mosquito
#' @return a [list]
#' @export
dts_make_Xpar_SEISd = function(nStrata, Xday, Xopts=list(),
                             b=0.55, r=1/180, nu=14, c=0.15){
  with(Xopts,{
    Xpar = list()
    class(Xpar) <- "SEISd"
    Xpar$b = checkIt(b, nStrata)
    Xpar$c = checkIt(c, nStrata)
    Xpar$nr = exp(-checkIt(r, nStrata)*Xday)
    Xpar$nu = exp(-checkIt(nu, nStrata)*Xday)

    return(Xpar)
  })}




# specialized methods for the human SEISd model

#' @title Size of effective infectious human population
#' @description Implements [F_X] for the SEISd model.
#' @inheritParams F_X
#' @return a [numeric] vector of length `nStrata`
#' @export
F_X.SEISd <- function(y, pars, i) {
  I = y[pars$ix$X[[i]]$I_ix]
  X = with(pars$Xpar[[i]], c*I)
  return(X)
}

#' @title Size of effective infectious human population
#' @description Implements [F_H] for the SEISd model.
#' @inheritParams F_H
#' @return a [numeric] vector of length `nStrata`
#' @export
F_H.SEISd <- function(y, pars, i){
  with(list_Xvars(y, pars, i), return(H))
}


#' @title Infection blocking pre-erythrocytic immunity
#' @description Implements [F_b] for the SEISd model.
#' @inheritParams F_b
#' @return a [numeric] vector of length `nStrata`
#' @export
F_b.SEISd <- function(y, pars, i) {
  with(pars$Xpar[[i]], b)
}

#' @title Make initial values for the SEISd xde human model, with defaults
#' @param nStrata the number of strata in the model
#' @param Xopts a [list] to overwrite defaults
#' @param H0 the initial human population density
#' @param S0 the initial values of the parameter S
#' @param E0 the initial values of the parameter E
#' @param I0 the initial values of the parameter I
#' @return a [list]
#' @export
#' @importFrom deSolve lagvalue
#' @importFrom deSolve lagderiv
make_Xinits_SEISd = function(nStrata, Xopts = list(), H0=NULL, S0=NULL, E0=0, I0=1){with(Xopts,{
  if(is.null(S0)) S0 = H0 - I0
  stopifnot(is.numeric(S0))
  S = checkIt(S0, nStrata)
  E = checkIt(E0, nStrata)
  I = checkIt(I0, nStrata)
  cfoi = rep(0, nStrata)
  return(list(S=S, E=E, I=I, cfoi=cfoi))
})}




#' @title Return the SEISd model variables as a list, returned from DT_Xt.SISd
#' @description This method dispatches on the type of `pars$Xpar`
#' @inheritParams put_Xvars
#' @return a [list]
#' @export
put_Xvars.SEISd <- function(Xvars, y, pars, i) {
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
list_Xvars.SEISd <- function(y, pars, i) {
  with(pars$ix$X[[i]],{
    S = y[S_ix]
    E = y[E_ix]
    I = y[I_ix]
    cfoi = y[cfoi_ix]
    H = S+E+I
    return(list(S=S,E=E,I=I,H=H,cfoi=cfoi))})
}

#' @title Setup Xinits.SEISd
#' @description Implements [setup_Xinits] for the SEISd model
#' @inheritParams setup_Xinits
#' @return a [list] vector
#' @export
setup_Xinits.SEISd = function(pars, i, Xopts=list()){
  pars$Xinits[[i]] = with(pars,make_Xinits_SEISd(pars$Hpar[[i]]$nStrata, Xopts, H0=Hpar[[i]]$H))
  return(pars)
}

#' @title Add indices for human population to parameter list
#' @description Implements [make_indices_X] for the SEISd model.
#' @inheritParams make_indices_X
#' @return none
#' @importFrom utils tail
#' @export
make_indices_X.SEISd <- function(pars, i) {with(pars,{

  S_ix <- seq(from = max_ix+1, length.out=Hpar[[i]]$nStrata)
  max_ix <- tail(S_ix, 1)

  E_ix <- seq(from = max_ix+1, length.out=Hpar[[i]]$nStrata)
  max_ix <- tail(E_ix, 1)

  I_ix <- seq(from = max_ix+1, length.out=Hpar[[i]]$nStrata)
  max_ix <- tail(I_ix, 1)

  cfoi_ix <- seq(from = max_ix+1, length.out=Hpar[[i]]$nStrata)
  max_ix <- tail(cfoi_ix, 1)

  pars$max_ix = max_ix
  pars$ix$X[[i]] = list(S_ix=S_ix, E_ix=E_ix, I_ix=I_ix, cfoi_ix=cfoi_ix)
  return(pars)
})}

#' @title Return initial values as a vector
#' @description This method dispatches on the type of `pars$Xpar`.
#' @inheritParams get_inits_X
#' @return a [numeric] vector
#' @export
get_inits_X.SEISd <- function(pars, i){
  with(pars$Xinits[[i]], return(c(S,E,I,cfoi)))
}

#' @title Update inits for the SEISd xde human model from a vector of states
#' @inheritParams update_inits_X
#' @return none
#' @export
update_inits_X.SEISd <- function(pars, y0, i) {
  with(list_Xvars(y0, pars, i),{
    pars$Xinits[[i]] = make_Xinits_SEISd(pars$Hpar[[i]]$nStrata, list(), S0=S, E0=E, I0=I)
    return(pars)
  })}


#' @title Parse the output of deSolve and return variables for the SEISd model
#' @description Implements [parse_outputs_X] for the SEISd model
#' @inheritParams parse_outputs_X
#' @return none
#' @export
parse_outputs_X.SEISd <- function(outputs, pars, i) {
  time = outputs[,1]
  with(pars$ix$X[[i]],{
    S = outputs[,S_ix+1]
    E = outputs[,E_ix+1]
    I = outputs[,I_ix+1]
    H = S+I
    return(list(time=time, S=S, E=E, I=I, H=H))
  })}

#' @title Compute the HTC for the SEISd model
#' @description Implements [HTC] for the SEISd model with demography.
#' @inheritParams HTC
#' @return a [numeric] vector
#' @export
HTC.SEISd <- function(pars, i) {
  with(pars$Xpar[[i]],
       return(c/r)
  )
}

#' @title Compute the "true" prevalence of infection / parasite rate
#' @description Implements [F_pr] for the SEISd model.
#' @inheritParams F_pr
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pr.SEISd <- function(vars, Xpar) {
  pr = with(vars, I/H)
  return(pr)
}

#' @title Compute the prevalence of infection by light microscopy
#' @description Implements [F_pr] for the SEISd model.
#' @inheritParams F_pr
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pr_by_lm.SEISd <- function(vars, Xpar) {
  pr = with(vars, I/H)
  return(pr)
}

#' @title Compute the prevalence of infection by RDT
#' @description Implements [F_pr] for the SEISd model.
#' @inheritParams F_pr
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pr_by_rdt.SEISd <- function(vars, Xpar) {
  pr = with(vars, I/H)
  return(pr)
}

#' @title Compute the prevalence of infection by pcr
#' @description Implements [F_pr] for the SEISd model.
#' @inheritParams F_pr
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pr_by_pcr.SEISd <- function(vars, Xpar) {
  pr = with(vars, I/H)
  return(pr)
}

#' Plot the density of infected individuals for the SEISd model
#'
#' @inheritParams xds_plot_X
#' @export
xds_plot_X.SEISd = function(pars, i=1, clrs=c("darkblue","darkred"), llty=1, stable=FALSE, add_axes=TRUE){
  vars=with(pars$outputs,if(stable==TRUE){stable_orbits}else{orbits})

  if(add_axes==TRUE)
    with(vars$XH[[i]],
         plot(time, 0*time, type = "n", ylim = c(0, max(H)),
              ylab = "# Infected", xlab = "Time"))


  add_lines_X_SEISd(vars$XH[[i]], pars$Hpar[[i]]$nStrata, clrs, llty)
}

#' Add lines for the density of infected individuals for the SEISd model
#'
#' @param XH a list with the outputs of parse_outputs_X_SEISd
#' @param nStrata the number of population strata
#' @param clrs a vector of colors
#' @param llty an integer (or integers) to set the `lty` for plotting
#'
#' @export
add_lines_X_SEISd = function(XH, nStrata, clrs=c("darkblue","darkred"), llty=1){
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


#' @title Compute the steady states for the SEISd model as a function of the daily EIR
#' @description Compute the steady state of the SEISd model as a function of the daily eir.
#' @inheritParams xde_steady_state_X
#' @return the steady states as a named vector
#' @export
xde_steady_state_X.SEISd = function(foi, H, Xpar){with(Xpar,{
  Seq = H/(1+foi*nu + foi/r)
  Eeq = foi*Seq*nu
  Ieq = foi*Seq/r
  return(c(S=Seq, E = Eeq, I=Ieq, H=H))
})}
