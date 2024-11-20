
#' @title Compute the average True PR
#'
#' @description This is a generic way of computing the average PR
#' for some subset(s) of the population. The object `members` is a
#' membership matrix, like the residence matrix (see [create_residence_matrix]).
#' If the residence matrix were passed to members, average_PR_true returns
#' the average PR for each patch by residency.
#'
#' The default, `members=NULL,` creates a \eqn{1 \times} `nStrata` matrix,
#' which takes the matrix over the whole population. If `members` is a
#' matrix with 1 column and `nStrata` rows, with only some elements set to one,
#' then it returns the average over those strata.
#'
#' @param pars an **`xds`** object
#' @param i the human species index
#' @param members a membership matrix
#' @return a PfPR
#' @export
average_PR_true = function(pars, i=1, members=NULL){
  if(is.null(members)) members = matrix(1, nrow=pars$nStrata[i], ncol=1)
  XH <- get_XH(pars,i)
  Ht <- XH$H %*% members
  npos <- with(XH, true_pr*H) %*% members
  PR <- npos/Ht
  return(PR)
}

#' @title Compute the average EIR
#'
#' @description This is a generic way of computing the average EIR
#' for some subset(s) of the population strata. The object members is a
#' membership matrix, like the residence matrix (see [create_residence_matrix]).
#' If the residence matrix were passed to members, average_EIR returns
#' the average EIR for the residents of each patch, which is could be different
#' from what would be computed.
#'
#' The default, `members=NULL,` creates a \eqn{1 \times} `nStrata` matrix,
#' which takes the matrix over the whole population. If `members` is a
#' matrix with 1 column and `nStrata` rows, with only some elements set to one,
#' then it returns the average over those strata.
#'
#' @param pars an **`xds`** object
#' @param i the human species index
#' @param members a membership matrix
#' @return a PfPR
#' @export
average_EIR = function(pars, i=1, members=NULL){
  if(is.null(members)) members = matrix(1, nrow=pars$nStrata[i], ncol=1)
  XH <- get_XH(pars,i)
  terms <- pars$outputs$terms
  Ht <- XH$H %*% members
  fqZ <- (XH$H*terms$EIR) %*% members
  eir <- fqZ/Ht
  return(eir)
}
