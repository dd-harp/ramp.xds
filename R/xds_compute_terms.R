# X 

#' @title Compute exogenous variables and dynamical terms
#'
#' @seealso [xds_solve]
#'
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#'
#' @return an **`xds`** model object
#' @keywords internal
#'
#' @export
xds_compute_terms <- function(t, y, xds_obj) {
  UseMethod("xds_compute_terms", xds_obj$frame)
}

#' @title Compute exogenous variables and dynamical terms
#' @inheritParams xds_compute_terms
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
xds_compute_terms.full <- function(t, y, xds_obj) {
  # set the values of Exogenous forcing variables
  xds_obj <- Forcing(t, xds_obj)
  # implement malaria control
  xds_obj <- Health(t, y, xds_obj)
  # implement vector control
  xds_obj <- VectorControl1(t, y, xds_obj)
  # set the values of resource variables --
  xds_obj <- Resources(t, y, xds_obj)
  # importation: travel and visitors
  xds_obj <- Importation(t, xds_obj)
  # blood feeding: available blood hosts, TaR, relative biting rates
  xds_obj <- BloodFeeding(t, y, xds_obj)
  # egg laying: available habitat, egg distribution matrix
  xds_obj <- AquaticHabitats(t, y, xds_obj)
  # Compute mosquito bionomic parameters
  # (including available sugar)
  xds_obj <- MosquitoBionomics(t, y, xds_obj)
  # Compute  "independent effect sizes"
  xds_obj <- VectorControl2(t, y, xds_obj)
  # Apply vector control effect sizes
  xds_obj <- VectorControlEffectSizes(t, y, xds_obj)
  # ML_interface: egg laying and emergence
  xds_obj <- ML_interface(t, y, xds_obj)
  # transmission: beta, EIR, and kappa
  xds_obj <- Transmission(t, y, xds_obj)
  # compute the FoI
  xds_obj <- Exposure(t, y, xds_obj)
  return(xds_obj)
}

#' @title Compute exogenous variables and dynamical terms
#' @inheritParams xds_compute_terms
#' @return an **`xds`** model object
#' @keywords internal
#' @export
xds_compute_terms.human <- function(t, y, xds_obj) {
  # weather port -- see ramp.forcing
  xds_obj <- Forcing(t, xds_obj)
  # malaria control port -- see ramp.control
  xds_obj <- Health(t, y, xds_obj)
  # vector control port -- see ramp.control
  xds_obj <- VectorControl1(t, y, xds_obj)
  # set the values of resource variables
  xds_obj <- Resources(t, y, xds_obj)
  # importation: travel and visitors
  xds_obj <- Importation(t, xds_obj)
  # blood feeding: available blood hosts, TaR, relative biting rates
  xds_obj <- BloodFeeding(t, y, xds_obj)
  # Compute mosquito bionomic parameters
  # (including available sugar)
  xds_obj <- MosquitoBionomics(t, y, xds_obj)
  # Compute total effect size assuming
  # "independent effect sizes"
  xds_obj <- VectorControl2(t, y, xds_obj)
  # Apply vector control effect sizes
  xds_obj <- VectorControlEffectSizes(t, y, xds_obj)
  # compute beta, EIR, and kappa
  xds_obj <- Transmission(t, y, xds_obj)
  # compute the FoI
  xds_obj <- Exposure(t, y, xds_obj)
  return(xds_obj)
}

#' @title Compute exogenous variables and dynamical terms
#' @inheritParams xds_compute_terms
#' @return an **`xds`** model object
#' @keywords internal
#' @export
xds_compute_terms.mosy <- function(t, y, xds_obj){

  # set the values of Exogenous forcing variables
  xds_obj <- Forcing(t, xds_obj)
  # vector control
  xds_obj <- VectorControl1(t, y, xds_obj)
  # set the values of resource variables
  xds_obj <- Resources(t, y, xds_obj)
  # importation: travel and visitors
  xds_obj <- Importation(t, xds_obj)
  # blood feeding: available blood hosts, TaR, relative biting rates
  xds_obj <- BloodFeeding(t, y, xds_obj)
  # egg laying: available habitat, egg distribution matrix
  xds_obj <- AquaticHabitats(t, y, xds_obj)
  # Compute mosquito bionomic parameters
  # (including available sugar)
  xds_obj <- MosquitoBionomics(t, y, xds_obj)
  # Compute total effect size assuming
  # "independent effect sizes"
  xds_obj <- VectorControl2(t, y, xds_obj)
  # Apply vector control effect sizes
  xds_obj <- VectorControlEffectSizes(t, y, xds_obj)
  # ML_interface: compute Lambda
  xds_obj <- ML_interface(t, y, xds_obj)
  return(xds_obj)
}

#' @title Compute exogenous variables and dynamical terms
#' @inheritParams xds_compute_terms
#' @return an **`xds`** model object
#' @keywords internal
#' @export
xds_compute_terms.aquatic <- function(t, y, xds_obj) {
  # set the values of Exogenous forcing variables
  xds_obj <- Forcing(t, xds_obj)
  # set the values of resource variables
  xds_obj <- Resources(t, y, xds_obj)
  # importation: travel and visitors
  xds_obj <- Importation(t, xds_obj)
  # vector control
  xds_obj <- VectorControl1(t, y, xds_obj)
  # Compute mosquito bionomic parameters
  xds_obj <- MosquitoBionomics(t, y, xds_obj)
  # Compute total effect size assuming
  # "independent effect sizes"
  xds_obj <- VectorControl2(t, y, xds_obj)
  # Apply vector control effect sizes
  xds_obj <- VectorControlEffectSizes(t, y, xds_obj)
  # egg laying: compute eta
  xds_obj$eggs_laid[[1]] = F_eggs(t, y, xds_obj, 1)
  return(xds_obj)
}

#' @title Compute exogenous variables and dynamical terms
#' @inheritParams xds_compute_terms
#' @return an **`xds`** model object
#' @keywords internal
#' @export
xds_compute_terms.eir <- function(t, y, xds_obj) {
  with(xds_obj$EIR_obj, {
    age = t-bday
    V_s = get_variables(season_par, t, y, xds_obj, 1)
    V_t = get_variables(trend_par, t, y, xds_obj, 1)
    V_k = get_variables(shock_par, t, y, xds_obj, 1)
    V_a = get_variables(age_par, t, y, xds_obj, 1)
    xds_obj$terms$EIR[[1]] <- eir*F_season(t, V_s)*F_trend(t, V_t)*F_age(age, V_a)*F_shock(t, V_k)
    xds_obj <- Exposure(t, y, xds_obj)
    return(xds_obj)
})}
