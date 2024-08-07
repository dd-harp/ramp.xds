# specialized methods for the aquatic mosquito trace model

#' @title Reset aquatic parameters to baseline
#' @description Implements [LBionomics] for the RM model
#' @inheritParams LBionomics
#' @return an **`xds`** object
#' @export
LBionomics.trace <- function(t, y, pars, s) {
  return(pars)
}

#' @title Compute the steady state as a function of the egg deposition rate eta
#' @description This method dispatches on the type of `Lpar`.
#' @inheritParams xde_steady_state_L
#' @return none
#' @export
xde_steady_state_L.trace = function(eta, Lpar){
  return(numeric(0))
}

#' @title Number of newly emerging adults from each larval habitat
#' @description Implements [F_emerge] for the trace (forced emergence) model.
#' @inheritParams F_emerge
#' @return a [numeric] vector of length `nHabitats`
#' @export
F_emerge.trace <- function(t, y, pars, s) {
  with(pars$Lpar[[s]], scale*Lt(t))
}

#' @title Derivatives for aquatic stage mosquitoes
#' @description Implements [dLdt] for the trace (forced emergence) model.
#' @inheritParams dLdt
#' @return an empty [list]
#' @export
dLdt.trace <- function(t, y, pars, s) {
  return(numeric(0))
}

#' @title Return the variables as a list
#' @description This method dispatches on the type of `pars$Lpar[[s]]`
#' @inheritParams list_Lvars
#' @return an empty [list]
#' @export
list_Lvars.trace <- function(y, pars, s){
  return(list())
}

#' @title Return the variables as a list
#' @description This method dispatches on the type of `pars$Lpar[[s]]`
#' @inheritParams put_Lvars
#' @return a [list]
#' @export
put_Lvars.trace <- function(Lvars, y, pars, s){
  return(y)
}

#' @title Derivatives for aquatic stage mosquitoes
#' @description Implements [Update_Lt] for the trace (forced emergence) model.
#' @inheritParams Update_Lt
#' @return a [numeric] vector
#' @export
Update_Lt.trace <- function(t, y, pars, s) {
  return(list())
}

#' @title xde_setup Lpar for the trace model
#' @description Implements [make_Lpar] for the trace model
#' @inheritParams make_Lpar
#' @return a [list] vector
#' @export
make_Lpar.trace = function(Lname, pars, s, Lopts=list()){
  pars$Lpar[[s]] = create_Lpar_trace(pars$nHabitats, Lopts)
  return(pars)
}

#' @title Make parameters for trace aquatic mosquito model
#' @param nHabitats the number of habitats in the model
#' @param Lopts a [list] that overwrites default values
#' @param Lambda vector of mean emergence rates from each aquatic habitat
#' @param Lt is a [function] of the form Lt(t,pars) that computes temporal fluctuations
#' @return none
#' @export
create_Lpar_trace = function(nHabitats, Lopts=list(), Lambda=1000, Lt = NULL){
  with(Lopts,{
    Lpar = list()
    class(Lpar) <- "trace"
    Lpar$scale = checkIt(Lambda, nHabitats)
    if(is.null(Lt)) Lt = function(t){1}
    Lpar$Lt = Lt
    Lpar$baseline = "trace"
    class(Lpar$baseline) = "trace"
    return(Lpar)
  })}

#' @title Setup \eqn{\cal L}-trace
#' @description Implements [make_Linits] for the trace model
#' @inheritParams make_Linits
#' @return a [list]
#' @export
make_Linits.trace = function(pars, s, Lopts=list()){
  pars$Linits[[s]] = list()
  return(pars)
}

#' @title Add indices for aquatic stage mosquitoes to parameter list
#' @description Implements [make_indices_L] for trace (forced emergence) model.
#' @inheritParams make_indices_L
#' @return none
#' @export
make_indices_L.trace <- function(pars, s) {
  return(pars)
}

#' @title Update inits for the basic aquatic mosquito competition model
#' @inheritParams update_Linits
#' @return none
#' @export
update_Linits.trace<- function(pars, y0, s) {
  return(pars)
}

#' @title Parse the variable names for the trace model
#' @description Implements [parse_outputs_L] for the trace model.
#' @inheritParams parse_outputs_L
#' @return [list]
#' @export
parse_outputs_L.trace <- function(outputs, pars, s) {
  return(numeric(0))
}
