
#' @title Static model for the blood feeding rate
#' @description Implements [F_f] for a static model
#' @inheritParams F_f
#' @return a [numeric] vector of length `nPatches`
#' @export
F_f.static <- function(t, MYZpar){
  MYZpar$f0
}

#' @title Static model for human blood fraction
#' @description Implements [F_q] for a static model
#' @inheritParams F_q
#' @return a [numeric] vector of length `nPatches`
#' @export
F_q.static <- function(t, MYZpar){
  MYZpar$q0
}

#' @title Static model for mosquito survival
#' @description Implements [F_p] for a static model
#' @inheritParams F_p
#' @return a [numeric] vector of length `nPatches`
#' @export
F_p.static <- function(t, MYZpar){
  MYZpar$p0
}

#' @title Static model for mosquito survival
#' @description Implements [F_g] for a static model
#' @inheritParams F_g
#' @return a [numeric] vector of length `nPatches`
#' @export
F_g.static <- function(t, MYZpar){
  MYZpar$g0
}

#' @title Static model for mosquito emigration
#' @description Implements [F_sigma] for a static model
#' @inheritParams F_sigma
#' @return a [numeric] vector of length `nPatches`
#' @export
F_sigma.static <- function(t, MYZpar){
  MYZpar$sigma0
}

#' @title Static model for the egg laying rate
#' @description Implements [F_nu] for a static model
#' @inheritParams F_nu
#' @return a [numeric] vector of length `nPatches`
#' @export
F_nu.static <- function(t, MYZpar){
  MYZpar$nu0
}
