# specialized methods for the adult mosquito RMdts model

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
#' @name RMdts
#' @rdname RMdts
NULL


#' @title The skill set
#'
#' @inheritParams get_skillset_MY
#'
#' @return the **MY** module skill set, as a list
#'
#' @keywords internal
#' @export
get_skillset_MY.RMdts = function(xds_obj, s){
  skills = list(
    dts = FALSE,
    xde = TRUE,
    delay = TRUE
  )
  return(skills)
}

#' Run a check before solving
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
check_MY.RMdts = function(xds_obj, s){
  return(xds_obj)
}


#' @title Update state variables for `RMdts` (**MY**)
#' @description Implements [Update_MYt] for the RMdts model.
#' @inheritParams Update_MYt
#' @return a [numeric] vector
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

#' @title Infective mosquito biting rate for `RMdts`
#' @description Implements [F_fqZ] for the RMdts model.
#' @inheritParams F_fqZ
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqZ.RMdts <- function(t, y, xds_obj, s) {
  with(xds_obj$MY_obj[[s]], f*q)*y[xds_obj$ix$MY[[s]]$Z_ix]
}

#' @title Mosquito biting rate for `RMdts`
#' @description Returns `fqM`, the daily mosquito biting density on humans
#' for each patch in the RMdts model.
#' @inheritParams F_fqM
#' @seealso [F_hbr]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqM.RMdts <- function(t, y, xds_obj, s) {
  with(xds_obj$MY_obj[[s]], f*q)*y[xds_obj$ix$MY[[s]]$M_ix]
}

#' @title Number of eggs laid by adult mosquitoes
#' @description Implements [F_eggs] for the RMdts model.
#' @inheritParams F_eggs
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_eggs.RMdts <- function(t, y, xds_obj, s) {
  M <- y[xds_obj$ix$MY[[s]]$M_ix]
  with(xds_obj$MY_obj[[s]],{
    return(M*nu*eggsPerBatch)
  })
}


#' @title Setup initial values for `RMdts` (**MY**)
#' @description Set initial values for all the variables in the `RMdts` module
#' (**MY**) for the \eqn{s^{th}} species.
#' + [make_MY_inits_RMdts] supplies default values
#' + The values in `options` override the defaults
#' + To inspect variable names, use [get_MY_inits] or
#' see the inline module documentation.
#' @inheritParams setup_MY_inits
#' @return a [list]
#' @keywords internal
#' @export
setup_MY_inits.RMdts = function(xds_obj, options=list(), s=1){
  xds_obj$MY_obj[[s]]$inits = with(xds_obj$MY_obj[[s]], make_MY_inits_RMdts(nPatches, max_eip, options))
  return(xds_obj)
}

#' @title Make inits for RMdts adult mosquito model
#' @param nPatches the number of patches
#' @param max_eip the maximum number of EIP cohorts, an [integer]
#' @param options a [list] of values that overrides the defaults
#' @param M total mosquito density at each patch
#' @param P total parous mosquito density at each patch
#' @param U total uninfected mosquito density at each patch
#' @param Y infected mosquito density at each patch
#' @param Z infectious mosquito density at each patch
#' @return a [list]
#' @keywords internal
#' @export
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


#' @title Add indices for adult mosquitoes to parameter list
#' @description Implements [setup_MY_ix] for the RMdts model.
#' @inheritParams setup_MY_ix
#' @return a [list]
#' @importFrom utils tail
#' @keywords internal
#' @export
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
  xds_obj$ix$MY[[s]] = list(M_ix=M_ix, P_ix=P_ix, U_ix=U_ix, Y_ix=Y_ix, Z_ix=Z_ix)
  return(xds_obj)
})}


#' @title List variables for `RMdts` (**MY**)
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`
#' @inheritParams get_MY_vars
#' @return a [list]
#' @keywords internal
#' @export
get_MY_vars.RMdts <- function(y, xds_obj, s){
  with(xds_obj$ix$MY[[s]],
       return(list(
         M = y[M_ix],
         P = y[P_ix],
         U = y[U_ix],
         Y = matrix(y[Y_ix], xds_obj$nPatches, xds_obj$MY_obj[[s]]$max_eip),
         Z = y[Z_ix]
       )))
}


#' @title Parse outputs for `RMdts` (**MY**)
#' @description Implements [parse_MY_orbits] for the RMdts model
#' @inheritParams parse_MY_orbits
#' @return a [list]
#' @keywords internal
#' @export
parse_MY_orbits.RMdts <- function(outputs, xds_obj, s) {with(xds_obj$ix$MY[[s]],{
  M = outputs[,M_ix]
  P = outputs[,P_ix]
  U = outputs[,U_ix]
  Y = rowSums(outputs[,Y_ix])
  Z = outputs[,Z_ix]
  y = Y/M
  z = Z/M
  parous = P/M
  return(list(M=M, P=P, U=U, Y=Y, Z=Z, y=y, z=z, parous=parous))
})}

#' @title Change initial values for `RMdts` (**MY**)
#' @description For the `RMdts` module (**MY**) for the \eqn{s^{th}} species,
#' change initial values named in `options` leaving other values unchanged.
#' @inheritParams change_MY_inits
#' @return a [list]
#' @keywords internal
#' @export
change_MY_inits.RMdts <- function(xds_obj, options=list(), s=1) {
  with(xds_obj$MY_obj[[s]]$ix,{
    with(options,{
    M = y0[M_ix]
    P = y0[P_ix]
    U = y0[U_ix]
    Y = y0[Y_ix]
    Z = y0[Z_ix]
    xds_obj$MY_obj[[s]]$inits = make_MY_inits_RMdts(xds_obj$nPatches, max_eip, list(), M=M, P=P, U=U, Y=Y, Z=Z)
    return(xds_obj)
})})}

#' @title Get parameters for `RMdts` (**MY**)
#' @description Get the stored parameter values for the `RMdts` model.
#' @inheritParams get_MY_pars
#' @return a [list]
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
#' @description Change static parameter values named in `options`,
#' leaving other values unchanged.
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
    return(xds_obj)
  }))
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

#' @title Get the feeding rates
#' @inheritParams get_f
#' @return the feeding rate, as a vector
#' @seealso [RMdts]
#' @keywords internal
#' @export
get_f.RMdts = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], f)
}

#' @title Get the human fractions
#' @inheritParams get_q
#' @return the human fraction, as a vector
#' @seealso [RMdts]
#' @keywords internal
#' @export
get_q.RMdts = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], q)
}

#' @title Get the mortality rates
#' @inheritParams get_g
#' @return the mortality rate, as a vector
#' @seealso [RMdts]
#' @keywords internal
#' @export
get_g.RMdts = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], g)
}

#' @title Get the patch emigration rates
#' @inheritParams get_sigma
#' @return the patch emigration rates, as a vector
#' @seealso [RMdts]
#' @keywords internal
#' @export
get_sigma.RMdts = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], sigma)
}

#' @title Get the initial mosquito population density
#' @inheritParams get_M
#' @return the initial mosquito population density
#' @seealso [RMdts], [get_MY_inits]
#' @keywords internal
#' @export
get_M.RMdts = function(xds_obj, s=1){
  get_MY_inits(xds_obj, s)$M
}

#' @title Get the initial infective mosquito density
#' @inheritParams get_Z
#' @return the initial infective mosquito population density
#' @seealso [RMdts], [get_MY_inits]
#' @keywords internal
#' @export
get_Z.RMdts = function(xds_obj, s=1){
  get_MY_inits(xds_obj, s)$Z
}
