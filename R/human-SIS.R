# specialized methods for the human SIS model

#' @title Derivatives for the SIS \eqn{\cal X} Compartment Model
#' @description Implements [dXdt] for the SIS model for human / vertebrate host infections
#' @details
#' The core dynamics of infection
#' \deqn{\frac{dS}{dt} = -h S + r I}
#' @inheritParams dXdt
#' @return a [numeric] vector
#' @export
dXdt.SIS <- function(t, y, pars, i) {

  foi <- pars$FoI[[i]]
  Hpar <- pars$Hpar[[i]]
  with(list_Xvars(y, pars, i),{
    with(pars$Xpar[[i]], {
      dS <- Births(t, H, Hpar) - foi*S + r*I + dHdt(t, S, Hpar)
      dI <- foi*S - r*I + dHdt(t, I, Hpar)
      return(c(dS, dI))
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
#' @description Implements [make_Xpar] for the SIS model
#' @inheritParams make_Xpar
#' @return a [list] vector
#' @export
make_Xpar.SIS = function(Xname, pars, i, Xopts=list()){
  pars$Xpar[[i]] = create_Xpar_SIS(pars$nStrata[1], Xopts)
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
create_Xpar_SIS = function(nStrata, Xopts=list(),
                             b=0.55, r=1/180, c=0.15){
  with(Xopts,{
    Xpar = list()
    class(Xpar) <- "SIS"

    Xpar$b = checkIt(b, nStrata)
    Xpar$c = checkIt(c, nStrata)
    Xpar$r = checkIt(r, nStrata)

    return(Xpar)
  })}




#' @title Size of effective infectious human population
#' @description Implements [F_X] for the SIS model.
#' @inheritParams F_X
#' @return a [numeric] vector of length `nStrata`
#' @export
F_X.SIS <- function(y, pars, i) {
  I = y[pars$ix$X[[i]]$I_ix]
  X = with(pars$Xpar[[i]], c*I)
  return(X)
}

#' @title Size of effective infectious human population
#' @description Implements [F_H] for the SIS model.
#' @inheritParams F_H
#' @return a [numeric] vector of length `nStrata`
#' @export
F_H.SIS <- function(y, pars, i){
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
         y[S_ix] = S
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
       S = y[S_ix]
       I = y[I_ix]
       H = S+I
       return(list(S=S,I=I,H=H))})
}

#' @title Make initial values for the SIS xde human model, with defaults
#' @param nStrata the number of strata in the model
#' @param H the initial human population density
#' @param Xopts a [list] to overwrite defaults
#' @param I the initial values of the parameter I
#' @return a [list]
#' @export
create_Xinits_SIS = function(nStrata, H, Xopts = list(), I=1){with(Xopts,{
  S = unname(as.vector(checkIt(H-I, nStrata)))
  I = unname(as.vector(checkIt(I, nStrata)))
  return(list(S=S, I=I))
})}

#' @title Setup Xinits.SIS
#' @description Implements [make_Xinits] for the SIS model
#' @inheritParams make_Xinits
#' @return a [list] vector
#' @export
make_Xinits.SIS = function(pars, H, i, Xopts=list()){
  pars$Xinits[[i]] = with(pars, create_Xinits_SIS(pars$nStrata[i], H, Xopts))
  return(pars)
}

#' @title Return initial values as a vector
#' @description This method dispatches on the type of `pars$Xpar`
#' @inheritParams get_Xinits
#' @return a [list]
#' @export
get_Xinits.SIS <- function(pars, i=1){pars$Xinits[[i]]}


#' @title Add indices for human population to parameter list
#' @description Implements [make_X_indices] for the SIS model.
#' @inheritParams make_X_indices
#' @return none
#' @importFrom utils tail
#' @export
make_X_indices.SIS <- function(pars, i) {with(pars,{

  S_ix <- seq(from = max_ix+1, length.out=nStrata[i])
  max_ix <- tail(S_ix, 1)

  I_ix <- seq(from = max_ix+1, length.out=nStrata[i])
  max_ix <- tail(I_ix, 1)

  pars$max_ix = max_ix
  pars$ix$X[[i]] = list(S_ix=S_ix, I_ix=I_ix)
  return(pars)
})}

#' @title Update inits for the SIS xde human model from a vector of states
#' @inheritParams update_Xinits
#' @return an `xds` object
#' @export
update_Xinits.SIS <- function(pars, y0, i) {
  with(list_Xvars(y0, pars, i),{
    pars$Xinits[[i]]$S = S
    pars$Xinits[[i]]$I = I
    return(pars)
  })}


#' @title Parse the output of deSolve and return variables for the SIS model
#' @description Implements [parse_Xorbits] for the SIS model
#' @inheritParams parse_Xorbits
#' @return none
#' @export
parse_Xorbits.SIS <- function(outputs, pars, i) {with(pars$ix$X[[i]],{
  S <- outputs[,S_ix]
  I <- outputs[,I_ix]
  H <- S+I
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

#' @title Compute the "true" prevalence of infection / parasite rate
#' @description Implements [F_pr] for the SIS model.
#' @inheritParams F_pr
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pr.SIS <- function(vars, Xpar) {
  pr = with(vars, I/H)
  return(pr)
}

#' @title Compute the prevalence of infection by light microscopy
#' @description Implements [F_pr] for the SIS model.
#' @inheritParams F_pr
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pr_by_lm.SIS <- function(vars, Xpar) {
  pr = with(vars, I/H)
  return(pr)
}

#' @title Compute the prevalence of infection by RDT
#' @description Implements [F_pr] for the SIS model.
#' @inheritParams F_pr
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pr_by_rdt.SIS <- function(vars, Xpar) {
  pr = with(vars, I/H)
  return(pr)
}

#' @title Compute the prevalence of infection by PCR
#' @description Implements [F_pr] for the SIS model.
#' @inheritParams F_pr
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pr_by_pcr.SIS <- function(vars, Xpar) {
  pr = with(vars, I/H)
  return(pr)
}

#' Plot the density of infected individuals for the SIS model
#'
#' @inheritParams xds_plot_X
#' @export
xds_plot_X.SIS = function(pars, i=1, clrs=c("darkblue","darkred"), llty=1, stable=FALSE, add_axes=TRUE){
  vars=with(pars$outputs,if(stable==TRUE){stable_orbits}else{orbits})

  if(add_axes==TRUE)
    with(vars$XH[[i]],
         plot(time, 0*time, type = "n", ylim = c(0, max(H)),
              ylab = "# Infected", xlab = "Time"))


  add_lines_X_SIS(vars$XH[[i]], pars$nStrata[i], clrs, llty)
}

#' Add lines for the density of infected individuals for the SIS model
#'
#' @param XH a list with the outputs of parse_Xorbits_SIS
#' @param nStrata the number of population strata
#' @param clrs a vector of colors
#' @param llty an integer (or integers) to set the `lty` for plotting
#'
#' @export
add_lines_X_SIS = function(XH, nStrata, clrs=c("darkblue","darkred"), llty=1){
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
