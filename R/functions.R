# X

#' F_one
#'
#' @param t time
#' @param V an optional list of variables
#'
#' @returns a vector of ones matching `t`
#' @keywords internal
#' @export
F_one = function(t, V=list()){0*t+1}

#' F_zero
#'
#' @param t time
#' @param V an optional list of variables
#'
#' @returns a vector of zeros matching `t`
#' @keywords internal
#' @export
F_zero = function(t, V=list()){0*t}

#' F_exp
#'
#' @param d distance
#' @param V an optional list of variables
#'
#' @returns `exp(-d)`
#' @keywords internal
#' @export
F_exp = function(d, V=list()){exp(-d)}
