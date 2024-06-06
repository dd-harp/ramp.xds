
#' @title Type 2 functional response for the blood feeding rate
#' @description Implements [F_f] for a static model
#' @inheritParams F_f
#' @return a [numeric] vector of length `nPatches`
#' @export
F_f.type2 <- function(t, MYZpar){
  B = with(MYZpar, W + Visitors + O)
  with(MYZpar$f_par,{
    return(fx*sf*B/(1+sf*B))
  })
}

#' @title Static model for human blood fraction
#' @description Implements [F_q] for a static model
#' @inheritParams F_q
#' @return a [numeric] vector of length `nPatches`
#' @export
F_q.dynamic <- function(t, MYZpar){
  with(MYZpar,{
    return((W+Visitors)/(W + Visitors + O))
  })
}


#' @title Model for mosquito emigration based on resource availability
#' @description Implements [F_sigma] for a static model
#' @inheritParams F_sigma
#' @return a [numeric] vector of length `nPatches`
#' @export
F_sigma.BQS <- function(t, MYZpar){
  with(MYZpar, with(MYZpar$sigma_par,{
    return(sigma_x*(sigma_B/(1+sB*B) + sigma_Q/(1+sQ*Q) + sigma_S/(1+sS*S)))
  }))
}


#' @title Type 2 functional response for the blood feeding rate
#' @description Implements [F_nu] for a static model
#' @inheritParams F_nu
#' @return a [numeric] vector of length `nPatches`
#' @export
F_nu.type2 <- function(t, MYZpar){
  with(MYZpar$nu_par,{
    return(nux*snu*Q/(1+snu*Q))
  })
}
