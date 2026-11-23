# Y
# specialized methods for the adult mosquito RMdts model
# TODO: Review parse_MY_orbits.RMdts repair: sum cohorts within each patch
# and preserve singleton dimensions during extraction.
# TODO: Reconcile Update_MYt.RMdts's list return with the numeric-vector interface,
# including cohort shape/order.
# TODO: Reconcile the RMdts State Variables anchor with Update_MYt.RMdts,
# which also updates P and U and treats Y as a cohort matrix.
# TODO: Review Update_MYt.RMdts cohort indexing: Gix uses t-1:max_eip,
# and the cohort columns are merged before the current-day column is replaced.
# TODO: Review change_MY_inits.RMdts repairs: use stored inits instead of y0,
# and rebuild with the selected species' max_eip. Confirm cohort dimensions,
# scalar expansion, partial updates, and species isolation.

#' @title The `RMdts` module for the MY component
#' @description
#' Implements the **MY** component using a discrete-time Macdonald
#' model of adult mosquito ecology and infection dynamics.
#'
#' @section State Variables:
#' \describe{
#'   \item{`M`}{density of adult mosquitoes}
#'   \item{`Y`}{density of infected adult mosquitoes}
#'   \item{`Z`}{density of infectious adult mosquitoes}
#' }
#'
#' @section Dynamical Terms:
#' \describe{
#'   \item{`Lambda` or \eqn{\Lambda}}{adult mosquito emergence, supplied by [Emergence]}
#'   \item{`kappa` or \eqn{\kappa}}{net infectiousness, supplied by [Transmission]}
#' }
#'
#' @section Parameters:
#' \describe{
#'   \item{`f`}{blood feeding rate}
#'   \item{`q`}{human blood fraction}
#'   \item{`eip`}{extrinsic incubation period (time steps)}
#'   \item{`g`}{mosquito mortality rate}
#'   \item{`sigma`}{emigration rate}
#'   \item{`mu`}{emigration loss rate}
#'   \item{`K`}{mosquito dispersal matrix}
#'   \item{`Omega`}{adult mosquito demographic matrix (mortality + migration)}
#'   \item{`Upsilon`}{survival and dispersal through the eip}
#' }
#'
#' @section Dynamics:
#' State is updated each time step via [Update_MYt.RMdts].
#'
#' @section DTS:
#' [Update_MYt.RMdts] updates `M`, `P`, `U`, the cohort matrix `Y`, and `Z`.
#' It uses the stored `Omega` as a one-step matrix and `G` to weight progression
#' out of the infected cohorts. Cohort columns are merged and replaced after
#' computing survival and progression.
#'
#' @section Egg Laying:
#' The daily number of eggs laid per patch is `M*nu*eggsPerBatch`, where
#' `M` is total adult mosquito density, `nu` is the oviposition rate,
#' and `eggsPerBatch` is the number of eggs per batch.
#' [F_eggs.RMdts] returns this quantity before distribution among aquatic
#' habitats by [EggLaying].
#'
#' @name RMdts
#' @rdname RMdts
NULL


#' @title Update states for `RMdts` (**MY**)
#' @description Update the state variables for the `RMdts` module:
#' \deqn{\begin{array}{rl}
#' M_t &= \Lambda+\Omega\cdot M_{t-1} \\\\
#' P_t &= f(M_{t-1}-P_{t-1})+\Omega\cdot P_{t-1} \\\\
#' U_t &= \Lambda+\Omega\cdot(e^{-fq\kappa}U_{t-1}) \\\\
#' \widetilde{Y}_t &= \Omega\cdot\left(Y_{t-1}\cdot\operatorname{diag}(1-G_t)\right) \\\\
#' Z_t &= \Omega\cdot(Y_{t-1}\cdot G_t)+\Omega\cdot Z_{t-1}
#' \end{array}}
#' Here `G_t` is `G[Gix]`, where `Gix = c(t-1:max_eip) %% max_eip + 1`.
#' Let \eqn{a} be `eip_day_ix = (t %% max_eip) + 1` and \eqn{b} be
#' `eip_yday_ix = ((t-1) %% max_eip) + 1`. Starting from
#' \eqn{Y_t=\widetilde{Y}_t}, apply these column assignments in order:
#' \deqn{\begin{array}{rl}
#' Y_t[,b] &\leftarrow Y_t[,b]+Y_t[,a] \\\\
#' Y_t[,a] &\leftarrow \Omega\cdot\left((1-e^{-fq\kappa})U_{t-1}\right)
#' \end{array}}
#' @inheritSection RMdts State Variables
#' @inheritSection RMdts Dynamical Terms
#' @inheritSection RMdts Parameters
#' @inheritSection RMdts DTS
#' @inheritParams Update_MYt
#' @seealso [RMdts]
#' @return a [list]
#' @keywords internal
#' @export
Update_MYt.RMdts <- function(t, y, xds_obj, s) {
  Lambda = xds_obj$terms$Lambda[[s]]
  kappa = xds_obj$terms$kappa[[s]]


  with(get_MY_vars(y, xds_obj, s),{
    with(xds_obj$MY_obj[[s]],{

      eip_day_ix = (t %% max_eip) + 1
      eip_yday_ix = ((t-1) %% max_eip) + 1
      Gix = c(t-1:max_eip) %% max_eip + 1
      Gt <- G[Gix]

      Mt <- Lambda + Omega %*% M
      Pt <- f*(M-P) + Omega %*% P
      Ut <- Lambda + Omega %*% (exp(-f*q*kappa)*U)
      Yt <- Omega %*% (Y %*% diag(1-Gt))
      Zt <- Omega %*% (Y%*%Gt)  + (Omega %*% Z)

      Yt[,eip_yday_ix]  <- Yt[,eip_yday_ix] + Yt[,eip_day_ix]
      Yt[,eip_day_ix] <- Omega %*% ((1-exp(-f*q*kappa))*U)

      return(list(M=unname(Mt), P=unname(Pt), U=unname(Ut), Y=unname(as.vector(Yt)), Z=unname(Zt)))
    })
  })
}


#' @title Get the state variables for `RMdts` (**MY**)
#' @description Extract the state variables for the `RMdts` module
#' from `y` and return them by name in a list for use during model solving.
#' Return `Y` as an `nPatches` by `max_eip` cohort matrix.
#' @inheritParams get_MY_vars
#' @return a named [list] of state variables and module-specific derived quantities
#' @keywords internal
#' @export
#' @note Variable indices are stored at `xds_obj$MY_obj[[s]]$ix`.
get_MY_vars.RMdts <- function(y, xds_obj, s){
  with(xds_obj$MY_obj[[s]]$ix,
       return(list(
         M = y[M_ix],
         P = y[P_ix],
         U = y[U_ix],
         Y = matrix(y[Y_ix], xds_obj$nPatches, xds_obj$MY_obj[[s]]$max_eip),
         Z = y[Z_ix]
       )))
}

#' @title Mosquito bionomics for `RMdts` (**MY**)
#' @description Leave the parameters unchanged; this module does not
#' implement dynamic bionomic ports.
#' @inheritParams MBionomics
#' @return an **`xds`** model object
#' @seealso [RMdts]
#' @keywords internal
#' @export
MBionomics.RMdts <- function(t, y, xds_obj, s) {
  return(xds_obj)
}

#' @title Apply effect sizes for `RMdts` (**MY**)
#' @description Leave the parameters unchanged; this module does not
#' implement effect-size modifications.
#' @inheritParams MEffectSizes
#' @return an **`xds`** model object
#' @seealso [RMdts]
#' @keywords internal
#' @export
MEffectSizes.RMdts <- function(t, y, xds_obj, s) {
  return(xds_obj)
}

#' @title Compute infective mosquito biting density for `RMdts` (**MY**)
#' @description Return the daily infective mosquito biting density on humans,
#' `fqZ`, for each patch in the `RMdts` module.
#' Compute `fqZ = f*q*Z`.
#' @inheritParams F_fqZ
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
#' @seealso [F_fqM.RMdts]
F_fqZ.RMdts <- function(t, y, xds_obj, s) {
  with(xds_obj$MY_obj[[s]], f*q)*y[xds_obj$MY_obj[[s]]$ix$Z_ix]
}

#' @title Compute mosquito biting density for `RMdts` (**MY**)
#' @description Return the daily mosquito biting density on humans,
#' `fqM`, for each patch in the `RMdts` module.
#' Compute `fqM = f*q*M`.
#' @inheritParams F_fqM
#' @seealso [F_hbr], [F_fqZ.RMdts]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqM.RMdts <- function(t, y, xds_obj, s) {
  with(xds_obj$MY_obj[[s]], f*q)*y[xds_obj$MY_obj[[s]]$ix$M_ix]
}

#' @title Compute egg production for `RMdts` (**MY**)
#' @description Return the number of eggs laid by the adult mosquito population, per patch
#' @inheritSection RMdts Egg Laying
#' @inheritParams F_eggs
#' @seealso [RMdts], [EggLaying]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_eggs.RMdts <- function(t, y, xds_obj, s) {
  M <- y[xds_obj$MY_obj[[s]]$ix$M_ix]
  with(xds_obj$MY_obj[[s]],{
    return(M*nu*eggsPerBatch)
  })
}

#' @title Get the feeding rate for `RMdts` (**MY**)
#' @description Return the stored realized feeding rate per patch.
#' @inheritSection RMdts Parameters
#' @inheritParams get_f
#' @seealso [RMdts]
#' @details Read `f` without evaluating ports or applying effect sizes.
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_f.RMdts = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], f)
}

#' @title Get the human blood fraction for `RMdts` (**MY**)
#' @description Return the stored realized human blood fraction per patch.
#' @inheritSection RMdts Parameters
#' @inheritParams get_q
#' @seealso [RMdts]
#' @details Read `q` without evaluating ports or applying effect sizes.
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_q.RMdts = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], q)
}

#' @title Get the adult mosquito mortality rate for `RMdts` (**MY**)
#' @description Return the stored realized adult mosquito mortality rate per patch.
#' @inheritSection RMdts Parameters
#' @inheritParams get_g
#' @seealso [RMdts]
#' @details Read `g` without evaluating ports or applying effect sizes.
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_g.RMdts = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], g)
}

#' @title Get the patch emigration rate for `RMdts` (**MY**)
#' @description Return the stored realized patch emigration rate per patch.
#' @inheritSection RMdts Parameters
#' @inheritParams get_sigma
#' @seealso [RMdts]
#' @details Read `sigma` without evaluating ports or applying effect sizes.
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_sigma.RMdts = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], sigma)
}

#' @title Get parameters for `RMdts` (**MY**)
#' @description Return the parameters for the `RMdts` module as a named list.
#' @inheritParams get_MY_pars
#' @return a named [list] of parameter values
#' @seealso [RMdts]
#' @keywords internal
#' @export
get_MY_pars.RMdts <- function(xds_obj, s=1) {
  with(xds_obj$MY_obj[[s]], list(
    f=f, q=q, g=g, sigma=sigma, eip=eip, mu=mu,
    nu=nu, eggsPerBatch=eggsPerBatch, K_matrix=K_matrix
  ))
}

#' @title Change parameters for `RMdts` (**MY**)
#' @description Change parameter values named in `options` for the `RMdts`
#' module, leaving other values unchanged.
#' Call [MBionomics] at time zero after updating parameters, without calling [MEffectSizes].
#' This method changes static parameter values.
#' @inheritParams change_MY_pars
#' @return an **`xds`** model object
#' @seealso [RMdts], [get_MY_pars.RMdts]
#' @keywords internal
#' @export
change_MY_pars.RMdts <- function(xds_obj, options=list(), s=1) {
  with(xds_obj$MY_obj[[s]], with(options,{
    xds_obj$MY_obj[[s]]$f = f
    xds_obj$MY_obj[[s]]$q = q
    xds_obj$MY_obj[[s]]$g = g
    xds_obj$MY_obj[[s]]$sigma = sigma
    xds_obj$MY_obj[[s]]$eip = eip
    xds_obj$MY_obj[[s]]$mu = mu
    xds_obj$MY_obj[[s]]$nu = nu
    xds_obj$MY_obj[[s]]$eggsPerBatch = eggsPerBatch
    y <- get_inits(xds_obj, flatten=TRUE)
    xds_obj <- MBionomics(0, y, xds_obj, s)
    return(xds_obj)
  }))
}

#' @title Make initial values for `RMdts` (**MY**)
#' @description Create a named list of initial values for the `RMdts` module.
#' Values in `options` override the defaults.
#' @details Initial values must be numeric scalars or have the required length:
#' `nPatches` for `M`, `P`, `U`, and `Z`, and `nPatches * max_eip` for `Y`.
#' Scalars are expanded to the required length.
#' @param nPatches the number of patches
#' @param max_eip the maximum number of EIP cohorts, an [integer]
#' @param options a [list] of values that overrides the defaults
#' @param M total mosquito density at each patch
#' @param P total parous mosquito density at each patch
#' @param U total uninfected mosquito density at each patch
#' @param Y initial infected mosquito densities across EIP cohorts, a numeric
#' scalar or input of length `nPatches * max_eip`
#' @param Z infectious mosquito density at each patch
#' @return a named [list] of initial values
#' @keywords internal
#' @export
#' @seealso [setup_MY_inits.RMdts], [change_MY_inits.RMdts]
make_MY_inits_RMdts = function(nPatches, max_eip, options = list(),
                                  M=5, P=1, U=0, Y=1, Z=1){
  with(options,{
    M = checkIt(M, nPatches)
    P = checkIt(P, nPatches)
    U = checkIt(U, nPatches)
    Y = checkIt(Y, nPatches*max_eip)
    Z = checkIt(Z, nPatches)
    return(list(M=M, P=P, U=U, Y=Y, Z=Z))
  })
}


#' @title Set up initial values for `RMdts` (**MY**)
#' @description Set up initial values for the `RMdts` module using
#' [make_MY_inits_RMdts]. Values in `options` override the defaults.
#' @inheritParams setup_MY_inits
#' @return an **`xds`** model object
#' @keywords internal
#' @export
#' @seealso [change_MY_inits.RMdts]
#' @note Initial values are stored at `xds_obj$MY_obj[[s]]$inits`.
#' @seealso [get_MY_inits]
setup_MY_inits.RMdts = function(xds_obj, options=list(), s=1){
  xds_obj$MY_obj[[s]]$inits = with(xds_obj$MY_obj[[s]], make_MY_inits_RMdts(nPatches, max_eip, options))
  return(xds_obj)
}

#' @title Change initial values for `RMdts` (**MY**)
#' @description Change initial values named in `options` for the `RMdts`
#' module, leaving other values unchanged.
#' Rebuild initial values using [make_MY_inits_RMdts] and the selected species'
#' `max_eip`; scalar values are expanded to the required lengths.
#' @inheritParams change_MY_inits
#' @return an **`xds`** model object
#' @keywords internal
#' @export
#' @seealso [make_MY_inits_RMdts], [setup_MY_inits.RMdts]
#' @note Stored initial values for this module are at
#' `xds_obj$MY_obj[[s]]$inits`.
change_MY_inits.RMdts <- function(xds_obj, options=list(), s=1) {
  with(xds_obj$MY_obj[[s]]$inits,{
    with(options,{
    xds_obj$MY_obj[[s]]$inits = make_MY_inits_RMdts(xds_obj$nPatches,
      xds_obj$MY_obj[[s]]$max_eip, list(), M=M, P=P, U=U, Y=Y, Z=Z)
    return(xds_obj)
})})}

#' @title Get the initial adult mosquito density for `RMdts` (**MY**)
#' @description Return the initial adult mosquito density per patch.
#' @inheritSection RMdts State Variables
#' @inheritParams get_M
#' @seealso [RMdts], [get_MY_inits]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_M.RMdts = function(xds_obj, s=1){
  get_MY_inits(xds_obj, s)$M
}

#' @title Get the initial infective mosquito density for `RMdts` (**MY**)
#' @description Return the initial infective mosquito density per patch.
#' @inheritSection RMdts State Variables
#' @inheritParams get_Z
#' @seealso [RMdts], [get_MY_inits]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_Z.RMdts = function(xds_obj, s=1){
  get_MY_inits(xds_obj, s)$Z
}


#' @title Set up variable indices for `RMdts` (**MY**)
#' @description Assign consecutive variable indices for the `RMdts` module,
#' starting after `xds_obj$max_ix`, and update the maximum index.
#' Allocate blocks in the order `M_ix`, `P_ix`, `U_ix`, `Y_ix`, `Z_ix`.
#' Each block has length `nPatches`, except `Y_ix`, which has length
#' `nPatches * xds_obj$MY_obj[[s]]$max_eip`.
#' @inheritParams setup_MY_ix
#' @return an **`xds`** model object
#' @importFrom utils tail
#' @keywords internal
#' @export
#' @note Indices are stored at `xds_obj$MY_obj[[s]]$ix`.
#' The maximum index is stored at `xds_obj$max_ix`.
setup_MY_ix.RMdts <- function(xds_obj, s) {with(xds_obj,{

  M_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(M_ix, 1)

  P_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(P_ix, 1)

  U_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(U_ix, 1)

  Y_ix <- seq(from = max_ix+1, length.out=nPatches*MY_obj[[s]]$max_eip)
  max_ix <- tail(Y_ix, 1)

  Z_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(Z_ix, 1)

  xds_obj$max_ix = max_ix
  xds_obj$MY_obj[[s]]$ix = list(M_ix=M_ix, P_ix=P_ix, U_ix=U_ix, Y_ix=Y_ix, Z_ix=Z_ix)
  return(xds_obj)
})}


#' @title Parse outputs for `RMdts` (**MY**)
#' @description Extract the model state outputs for the `RMdts` module
#' from `outputs` and return them by name in a list.
#' Sum the `Y` cohorts within each patch for each output time, and compute
#' `y = Y/M`, `z = Z/M`, and `parous = P/M`.
#' @note Variable indices are stored at `xds_obj$MY_obj[[s]]$ix`.
#' The output-processing pipeline stores the returned orbits at
#' `xds_obj$outputs$orbits$MY[[s]]`.
#' @inheritParams parse_MY_orbits
#' @return a [list]
#' @keywords internal
#' @export
parse_MY_orbits.RMdts <- function(outputs, xds_obj, s) {with(xds_obj$MY_obj[[s]]$ix,{
  M = outputs[,M_ix]
  P = outputs[,P_ix]
  U = outputs[,U_ix]
  Y = matrix(0, nrow=nrow(outputs), ncol=length(M_ix))
  for (cohort in seq_len(xds_obj$MY_obj[[s]]$max_eip)) {
    cohort_ix = Y_ix[(cohort-1L)*length(M_ix) + seq_along(M_ix)]
    Y = Y + outputs[,cohort_ix,drop=FALSE]
  }
  Y = drop(Y)
  Z = outputs[,Z_ix]
  y = Y/M
  z = Z/M
  parous = P/M
  return(list(M=M, P=P, U=U, Y=Y, Z=Z, y=y, z=z, parous=parous))
})}

#' @title Check `RMdts` (**MY**)
#'
#' @inheritParams check_MY
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
check_MY.RMdts = function(xds_obj, s){
  return(xds_obj)
}


#' @title Get the skill set for `RMdts` (**MY**)
#'
#' @description Return the capabilities and compatibilities of the `RMdts` module as a list.
#'
#' @inheritParams get_skillset_MY
#'
#' @return the **MY** module skill set, as a list
#'
#' @keywords internal
#' @export
get_skillset_MY.RMdts = function(xds_obj, s){
  skills = list(
    f_is_port = FALSE,
    q_is_port = FALSE,
    g_is_port = FALSE,
    sigma_is_port = FALSE,
    sigma_b_is_port = FALSE,
    sigma_q_is_port = FALSE,
    mu_is_port = FALSE,
    nu_is_port = FALSE,
    eip_is_port = FALSE,
    K_matrix_is_port = FALSE,
    dts = FALSE,
    xde = TRUE,
    delay = TRUE
  )
  return(skills)
}
