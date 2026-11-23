# Y

# a hybrid model tracking mean MoI for all and apparent infections

#' @title The `hMoI` module for the **XH** component
#' 
#' @description
#' `hMoI` implements a model with two hybrid variables that track the
#' mean Multiplicity of Infection (MoI): the first models the true MoI, 
#' and the second is the MoI for "younger" infections that are detected
#' with higher frequency than older ones. 
#' 
#' This model is derived under a restrictive set of assumptions, 
#' so it cannot be extended to model dynamic changes in population size or
#' treatment with anti-malarial drugs.
#'
#' @section State Variables:
#' \describe{
#'   \item{`m1` or \eqn{m_1}}{mean MoI for all infections}
#'   \item{`m2` or \eqn{m_2}}{mean MoI for apparent (detectable) infections}
#' }
#' 
#' @section Demography:
#' 
#' Demographic change is inconsistent with the model formulation. Human population density \eqn{(H)}
#' is a parameter, but it should be changed using [change_XH_inits]. 
#' 
#' @section Dynamical Term:
#' 
#' \describe{
#'   \item{\eqn{h} or `foi`}{The force of infection}
#' }
#' 
#' In the `hMoI` module, there is no effect of pre-erythrocytic immunity, so
#' a constant fraction of infectious bites causes an infection: the parameter \eqn{b.}
#'
#' @note The force of infection is computed and stored by [Exposure] as `FoI`.
#' 
#'
#' @section Parameters:
#' \describe{
#'   \item{`r1` or \eqn{r_1}}{clearance rate for all infections}
#'   \item{`r2` or \eqn{r_2}}{clearance rate for apparent infections, \eqn{r_2 > r_1}}
#'   \item{`b`}{probability of infection per infectious bite}
#'   \item{`c1` or \eqn{c_1}}{probability of transmitting from m1}
#'   \item{`c2` or \eqn{c_2}}{probability of transmitting from m2}
#'   \item{H}{human or host population density}
#' }
#'
#' @section Infection Dynamics:
#' \deqn{
#' \begin{array}{rl}
#' dm_1/dt &= h - r_1 m_1 \\
#' dm_2/dt &= h - r_2 m_2 \\
#' \end{array}}
#' 
#' @section Prevalence:
#' 
#' \describe{
#'   \item{\eqn{x_1}}{true prevalence: \eqn{x_1 = 1-e^{-m_1}}}
#'   \item{\eqn{x_2}}{apparent prevalence: \eqn{x_2 = 1-e^{-m_2}}}
#' }
#' [F_pr_obs.hMoI] returns apparent prevalence \eqn{x_2}.
#' [F_pfpr_by_lm], [F_pfpr_by_rdt], and [F_pfpr_by_pcr] are compatibility
#' aliases for [F_pr_obs] in this module.
#' 
#' @section Infectiousness:
#' 
#' The model assumes that younger infections are more infectious, but that
#' some transmission does occur from the older infections: 
#' \deqn{c_2 x_2 + c_1 (x_1 - x_2)}
#' so the infectious density is \deqn{\left(c_2 x_2 + c_1 (x_1 - x_2)\right) H}
#' 
#' @name hMoI
#' @rdname hMoI
NULL

#' @title Compute the derivatives for `hMoI` (**XH**)
#'
#' @description The derivatives for the hybrid MoI (`hMoI`) module are:
#' \deqn{\begin{array}{rl}
#' dm_1/dt &= h-r_1m_1 \\
#' dm_2/dt &= h-r_2m_2
#' \end{array}}
#' Here \eqn{h} is the force of infection (`foi`).
#' @inheritParams dXHdt
#' @return a [numeric] vector
#' @keywords internal
#' @seealso [hMoI]
#' @export
dXHdt.hMoI <- function(t, y, xds_obj, i) {

  foi = xds_obj$terms$FoI[[i]]

  with(get_XH_vars(y, xds_obj, i),{
    with(xds_obj$XH_obj[[i]], {
      dm1dt <- foi - r1*m1
      dm2dt <- foi - r2*m2
      return(c(dm1dt, dm2dt))
    })
  })
}

#' @title Get the state variables for `hMoI` (**XH**)
#'
#' @description Extract the state variables for the `hMoI` module
#' from `y` and return them by name in a list for use during model solving.
#' Also return `H` from the module object rather than from `y`.
#' @inheritParams get_XH_vars
#' @return a named [list] of state variables and module-specific derived quantities
#' @keywords internal
#' @seealso [hMoI]
#' @export
#' @note Variable indices are stored at `xds_obj$XH_obj[[i]]$ix`.
get_XH_vars.hMoI <- function(y, xds_obj, i) {
  with(xds_obj$XH_obj[[i]]$ix,{
    m1 = y[m1_ix]
    m2 = y[m2_ix]
    H = xds_obj$XH_obj[[i]]$H
    return(list(H=H,m1=m1,m2=m2))})
}





#' @title Size of effective infectious human population
#' @description Implements [F_I] for the hybrid MoI model.
#' @inheritParams F_I
#' @return a [numeric] vector of length `nStrata`
#' @importFrom stats pexp
#' @keywords internal
#' @seealso [hMoI]
#' @export
F_I.hMoI <- function(t, y, xds_obj, i) {
  with(get_XH_vars(y, xds_obj, i),{
    with(xds_obj$XH_obj[[i]],{
      x1 <- pexp(q = m1)
      x2 <- pexp(q = m2)
      X <- ((c2 * x2) + (c1 * (x1 - x2)))*H
      return(X)
    })
})}

#' @title Size of the human population
#' @description Implements [F_H] for the hybrid MoI model.
#' @inheritParams F_H
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @seealso [hMoI]
#' @export
F_H.hMoI <- function(t, y, xds_obj, i) {
  xds_obj$XH_obj[[i]]$H
}


#' @title Infection blocking pre-erythrocytic immunity
#' @description Implements [F_infectivity] for the `hMoI` model.
#' @inheritParams F_infectivity
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @seealso [hMoI]
#' @export
F_infectivity.hMoI <- function(y, xds_obj,i) {
  with(xds_obj$XH_obj[[i]], b)
}

#' @title Make a module object for `hMoI` (**XH**)
#' @description Create an `hMoI` module object and initialize its parameters.
#' Inspect parameters with [get_XH_pars.hMoI] and change supported values
#' with [change_XH_pars.hMoI]. Values in `options` override parameter arguments.
#'
#' MoI stands for Multiplicity of Infection, and refers to malarial superinfection.
#' @seealso [get_XH_pars.hMoI], [change_XH_pars.hMoI]
#' @param nStrata is the number of human population strata
#' @param options a [list] that overrides default values
#'
#' @param b transmission probability (efficiency) from mosquito to human
#' @param c1 transmission probability (efficiency) from inapparent human infections to mosquito
#' @param c2 transmission probability (efficiency) from patent human infections to mosquito
#' @param r1 recovery rate from inapparent infections
#' @param r2 recovery rate from patent infections
#' @param H human population density
#'
#' @return a [list]
#' @keywords internal
#' @seealso [hMoI]
#' @export
make_XH_obj_hMoI = function(nStrata, options=list(),
                            b=0.55, r1=1/180, r2 = 1/70,
                            c1=0.015, c2=0.15, H=1){
  with(options,{
    XH_obj = list()
    class(XH_obj) <- "hMoI"

    XH_obj$b = checkIt(b, nStrata)
    XH_obj$c1 = checkIt(c1, nStrata)
    XH_obj$c2 = checkIt(c2, nStrata)
    XH_obj$r1 = checkIt(r1, nStrata)
    XH_obj$r2 = checkIt(r2, nStrata)
    XH_obj$H = checkIt(H, nStrata)

    return(XH_obj)
})}

#' @title Set up the module object for `hMoI` (**XH**)
#' @description
#'
#' Set up an `hMoI` module object for the **XH** component and the \eqn{i^{th}} host species.
#'
#' + Set the module name to `hMoI`.
#' + Create the module object with [make_XH_obj_hMoI], passing `nStrata[i]` and `options`.
#' + Set initial values using [setup_XH_inits], passing `options` to override defaults.
#' + Initialize blood search weights using [setup_blood_search_weights.setup].
#' + Initialize the time-spent matrix using [setup_timespent.setup].
#' + Initialize time away using [setup_time_away.setup].
#' + Initialize travel EIR using [setup_travel_eir.no_travel].
#' + Return the updated **`xds`** model object.
#'
#' @seealso [hMoI], [make_XH_obj_hMoI]
#'
#' @note The module object is stored at `xds_obj$XH_obj[[i]]`
#' @inheritSection hMoI State Variables
#' @inheritSection hMoI Parameters
#' @inheritParams setup_XH_obj
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_XH_obj.hMoI = function(XHname, residence, xds_obj, options=list(), i=1){
  xds_obj$XHname = "hMoI"
  xds_obj$XH_obj[[i]] = make_XH_obj_hMoI(xds_obj$nStrata[i], options)
  xds_obj <- setup_XH_inits(xds_obj, options=options, i=i)
  xds_obj <- setup_blood_search_weights("setup", xds_obj, i=i)
  xds_obj <- setup_timespent("setup", xds_obj, list(residence=residence), i)
  xds_obj <- setup_time_away("setup", xds_obj, i=i)
  xds_obj <- setup_travel_eir("no_travel", xds_obj, i=i)
  return(xds_obj)
}

#' @title Get parameters for `hMoI` (**XH**)
#' @seealso [make_XH_obj_hMoI], [change_XH_pars.hMoI]
#'
#' @description Return the parameters for the `hMoI` module as a named list.
#' @note Parameter values for the \eqn{i^{th}} host are
#' stored as `xds_obj$XH_obj[[i]]`.
#' 
#' @inheritSection hMoI Parameters 
#' 
#' @inheritParams get_XH_pars
#'
#' @return a named [list] of parameter values
#'
#' @seealso [hMoI], [make_XH_obj_hMoI]
#' @keywords internal
#' @export
get_XH_pars.hMoI<- function(xds_obj, i=1) {
  with(xds_obj$XH_obj[[i]],list(b=b, c1=c1, c2=c2, r1=r1, r2=r2))
}

#' @title Change parameters for `hMoI` (**XH**)
#' @seealso [make_XH_obj_hMoI], [get_XH_pars.hMoI]
#'
#' @description Change parameter values named in `options` for the `hMoI`
#' module, leaving other values unchanged.
#'
#' @inheritSection hMoI Parameters 
#' 
#' @inheritParams change_XH_pars
#'
#' @return an **`xds`** model object
#'
#' @keywords internal
#' @seealso [hMoI]
#' @export
change_XH_pars.hMoI <- function(xds_obj, options=list(), i=1) {
  nHabitats <- xds_obj$nHabitats
  with(xds_obj$XH_obj[[i]], with(options,{
    xds_obj$XH_obj[[i]]$b <- b
    xds_obj$XH_obj[[i]]$c1 <- c1
    xds_obj$XH_obj[[i]]$c2 <- c2
    xds_obj$XH_obj[[i]]$r1 <- r1
    xds_obj$XH_obj[[i]]$r2 <- r2
    return(xds_obj)
  }))}

#' @title Make initial values for `hMoI` (**XH**)
#' @description Create a named list of initial values for the `hMoI` module.
#' Values in `options` override the defaults.
#' MoI stands for Multiplicity of Infection, and refers to malarial superinfection.
#' @details Initial values must be numeric scalars or have length `nStrata`.
#' Scalars are expanded to that length.
#'
#' @inheritSection hMoI State Variables
#' 
#' @param nStrata the number of population strata
#' @param options a [list] that overrides default values
#' @param m1 mean MoI for all infections
#' @param m2 mean MoI for apparent infections
#' @return a named [list] of initial values
#' @keywords internal
#' @seealso [hMoI]
#' @export
#' @seealso [setup_XH_inits.hMoI], [change_XH_inits.hMoI]
make_XH_inits_hMoI = function(nStrata, options = list(), m1=2, m2=1){
  with(options,{
    m1 = checkIt(m1, nStrata)
    m2 = checkIt(m2, nStrata)
    return(list(m1=m1, m2=m2))
})}

#' @title Set up initial values for `hMoI` (**XH**)
#' @description Set up initial values for the `hMoI` module using
#' [make_XH_inits_hMoI]. Values in `options` override the defaults.
#' If `H` is supplied in `options`, also update the module's population density.
#' @inheritParams setup_XH_inits
#' @return an **`xds`** model object
#' @keywords internal
#' @seealso [hMoI]
#' @export
#' @seealso [change_XH_inits.hMoI]
#' @note Initial values are stored at `xds_obj$XH_obj[[i]]$inits`.
#' @seealso [get_XH_inits]
setup_XH_inits.hMoI = function(xds_obj, options=list(), i=1){
  xds_obj$XH_obj[[i]]$inits = make_XH_inits_hMoI(xds_obj$nStrata[i], options)
  if(!is.null(options$H))
    xds_obj$XH_obj[[i]]$H = checkIt(options$H, xds_obj$nStrata[i])
  return(xds_obj)
}

#' @title Change initial values for `hMoI` (**XH**)
#' @description Change initial values named in `options` for the `hMoI`
#' module, leaving other values unchanged.
#' 
#' It must be true that \eqn{m_1 > m_2}, so this condition is checked before
#' changing the initial conditions, but some pairs of values would not arise
#' from any history of exposure. 
#' 
#' @inheritParams change_XH_inits
#' @inheritSection hMoI State Variables
#' @return an **`xds`** model object
#' @keywords internal
#' @seealso [hMoI]
#' @export
#' @seealso [make_XH_inits_hMoI], [setup_XH_inits.hMoI]
#' @note Stored initial values for this module are at
#' `xds_obj$XH_obj[[i]]$inits`.
#' If `H` is supplied in `options`, the module's population density is also updated.
change_XH_inits.hMoI <- function(xds_obj, options=list(), i=1) {
  with(xds_obj$XH_obj[[i]]$inits, with(options,{
    m1 = checkIt(m1, xds_obj$nStrata[i])
    m2 = checkIt(m2, xds_obj$nStrata[i])
    stopifnot(m2 < m1)
    if(!is.null(options$H))
      xds_obj$XH_obj[[i]]$H = checkIt(options$H, xds_obj$nStrata[i])
    xds_obj$XH_obj[[i]]$inits$m1 = m1
    xds_obj$XH_obj[[i]]$inits$m2 = m2
    return(xds_obj)
}))}

#' @title Set up variable indices for `hMoI` (**XH**)
#' @description Assign consecutive variable indices for the `hMoI` module,
#' starting after `xds_obj$max_ix`, and update the maximum index.
#' Allocate blocks in the order `m1_ix`, `m2_ix`.
#' Each block has length `nStrata[i]`.
#' @inheritParams setup_XH_ix
#' @return an **`xds`** model object
#' @importFrom utils tail
#' @keywords internal
#' @seealso [hMoI]
#' @export
#' @note Indices are stored at `xds_obj$XH_obj[[i]]$ix`.
#' The maximum index is stored at `xds_obj$max_ix`.
setup_XH_ix.hMoI <- function(xds_obj, i) {with(xds_obj,{

  m1_ix <- seq(from=max_ix+1, length.out=nStrata[i])
  max_ix <- tail(m1_ix, 1)

  m2_ix <- seq(from=max_ix+1, length.out=nStrata[i])
  max_ix <- tail(m2_ix, 1)

  xds_obj$XH_obj[[i]]$ix = list(m1_ix=m1_ix, m2_ix=m2_ix)

  xds_obj$max_ix = max_ix
  return(xds_obj)
})}

#' @title Parse outputs for `hMoI` (**XH**)
#' @description Extract the model state outputs for the `hMoI` module
#' from `outputs` and return them by name in a list.
#' Return `m1`, `m2`, and a matrix `H` with one row per output time and
#' `nStrata[i]` columns.
#' The fixed population parameter `H` is repeated across output times.
#' @note Variable indices are stored at `xds_obj$XH_obj[[i]]$ix`.
#' @inheritParams parse_XH_orbits
#' @return a [list]
#' @keywords internal
#' @seealso [hMoI]
#' @export
parse_XH_orbits.hMoI <- function(outputs, xds_obj, i){
  with(xds_obj$XH_obj[[i]]$ix, {
    m1 = outputs[,m1_ix]
    m2 = outputs[,m2_ix]
    H = matrix(xds_obj$XH_obj[[i]]$H, nrow=nrow(outputs),
               ncol=xds_obj$nStrata[i], byrow=TRUE)
    return(list(H=H, m1=m1, m2=m2))
})}

#' @title Compute the "true" prevalence of infection / parasite rate
#' @description Implements [F_prevalence] for the `hMoI` model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @seealso [hMoI]
#' @export
F_prevalence.hMoI<- function(vars, XH_obj) {
  pr = with(vars, 1-exp(-m1))
  return(pr)
}


#' @title Compute net infectiousness
#' @description Implements [F_ni] for the `hMoI` model.
#' @inheritParams F_ni
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @seealso [hMoI]
#' @export
F_ni.hMoI<- function(vars, XH_obj) {with(XH_obj,{
  x1 <- 1 - exp(-vars$m1)
  x2 <- 1 - exp(-vars$m2)
  ni <- (c2 * x2) + (c1 * (x1 - x2))
  return(ni)
})}


#' @title Compute the get_HTC for the `hMoI` model
#' @description Implements \code{get_HTC} for the `hMoI` model without demographic change.
#' @inheritParams get_HTC
#' @return a [numeric] vector
#' @keywords internal
#' @seealso [hMoI]
#' @export
get_HTC.hMoI <- function(xds_obj, i) {
  with(xds_obj$XH_obj[[i]],
       return(c2/r2 + c1*(1/r1 - 1/r2))
  )
}

#' @title Compute observed prevalence
#' @description Return apparent prevalence for the `hMoI` module as
#' `1 - exp(-m2)`, where `m2` is the mean MoI for apparent infections.
#' @inheritParams F_pr_obs
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @seealso [hMoI]
#' @export
F_pr_obs.hMoI <- function(vars, XH_obj) {
  pr = with(vars, 1-exp(-m2))
  return(pr)
}

#' @title Compute observed prevalence by light microscopy
#' @description Compatibility alias for [F_pr_obs.hMoI]. All diagnostic
#' aliases return apparent prevalence based on `m2`.
#' @inheritParams F_pfpr_by_lm
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @seealso [hMoI]
#' @export
F_pfpr_by_lm.hMoI <- function(vars, XH_obj) {
  F_pr_obs(vars, XH_obj)
}

#' @title Compute observed prevalence by RDT
#' @description Compatibility alias for [F_pr_obs.hMoI]. All diagnostic
#' aliases return apparent prevalence based on `m2`.
#' @inheritParams F_pfpr_by_rdt
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @seealso [hMoI]
#' @export
F_pfpr_by_rdt.hMoI<- function(vars, XH_obj) {
  F_pr_obs(vars, XH_obj)
}

#' @title Compute observed prevalence by PCR
#' @description Compatibility alias for [F_pr_obs.hMoI]. All diagnostic
#' aliases return apparent prevalence based on `m2`.
#' @inheritParams F_pfpr_by_pcr
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @seealso [hMoI]
#' @export
F_pfpr_by_pcr.hMoI<- function(vars, XH_obj) {
  F_pr_obs(vars, XH_obj)
}

#' @title Check `hMoI` (**XH**)
#'
#' @inheritParams check_XH
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @seealso [hMoI]
#' @export
check_XH.hMoI = function(xds_obj, i){
  return(xds_obj)
}


#' @title Get the skill set for `hMoI` (**XH**)
#'
#' @description Return the capabilities and compatibilities of the `hMoI` module as a list.
#'
#' @details The `hMoI` module is an example
#' of a model that is not extensible. There is no
#' way to add either dynamic human population density
#' or mass treatment without violating the model
#' assumptions.
#'
#' @inheritParams get_skillset_XH
#'
#' @return the **XH** module skill set, as a list
#'
#' @keywords internal
#' @seealso [hMoI]
#' @export
get_skillset_XH.hMoI = function(xds_obj, i){
  skills = list(
    dts        = FALSE,
    xde        = TRUE,
    delay      = FALSE,
    demography = FALSE,
    malaria    = TRUE,
    pr_obs     = TRUE,
    pf_lm      = FALSE,
    pf_rdt     = FALSE,
    pf_pcr     = FALSE,
    mda        = FALSE,
    msat       = FALSE
  )
  return(skills)
}

#' @title Compute steady states for `hMoI` (**XH**)
#'
#' @description Compute steady-state infection values for the `hMoI` module
#' given daily FoI `foi` for the differential-equation system.
#' The fixed population input `H` is not used in this calculation.
#' @inheritParams steady_state_X
#' @return a named [numeric] vector containing `m1` followed by `m2`
#' @keywords internal
#' @seealso [hMoI]
#' @export
steady_state_X.hMoI = function(foi, H, xds_obj, i=1){
  with(xds_obj$XH_obj[[i]],{
    m1 = foi/r1
    m2 = foi/r2
    return(c(m1=m1, m2=m2))
})}
