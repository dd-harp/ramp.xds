# X

#' @title Time Spent
#' 
#' @description
#' 
#' \describe{
#'   \item{\eqn{\Theta}}{time spent matrix}
#' }
#' 
#' @section Time-Spent Matrix: 
#' 
#' Some fraction of time is spent spent traveling, defined herein as time spent
#' outside of the patches, or *time away* (see [time_away]). The remainder of time is spent in one of the patches. 
#' The risk of exposure here computed with a 
#' *time spent matrix.* 
#' 
#' Let \eqn{N_p} denote the number of patches
#' and \eqn{N_h} the number of strata.
#' The time spent matrix, \eqn{\Theta}, is an \eqn{N_p \times N_h} matrix with the form:
#' \deqn{
#' {\Theta} =
#' \left[
#' \begin{array}{ccccc}
#' j=1&j=2&j=3&\cdots&j=N_h \\
#' \boxed{
#' \begin{array}{c}
#' {\theta}_{1,1} \\
#' {\theta}_{2,1} \\
#' {\theta}_{3,1} \\
#' \vdots \\
#' {\theta}_{N_p,1} \\
#' \end{array}}
#' &
#' \boxed{
#' \begin{array}{c}
#' {\theta}_{1,2} \\
#' {\theta}_{2,2} \\
#' {\theta}_{3,2} \\
#' \vdots \\
#' {\theta}_{N_p,2} \\
#' \end{array}}
#' &
#' \boxed{
#' \begin{array}{c}
#' {\theta}_{1,3} \\
#' {\theta}_{2,3} \\
#' {\theta}_{3,3} \\
#' \vdots \\
#' {\theta}_{N_p,3} \\
#' \end{array}}
#' &
#' \boxed{
#' \begin{array}{c}
#' \cdots \\
#' \cdots \\
#' \cdots \\
#' \ddots \\
#' \cdots
#' \end{array}}
#' &
#' \boxed{
#' \begin{array}{c}
#' {\theta}_{1,N_h} \\
#' {\theta}_{2,N_h} \\
#' {\theta}_{3,N_h} \\
#' \vdots \\
#' {\theta}_{N_p,N_h} \\
#' \end{array}}
#' \end{array}
#' \right]
#' }
#'
#' Each column describes the fraction of time spent by members of a single
#' population stratum in each patch *when not traveling,* so \eqn{\sum_i \theta_{i,j}\leq 1}.
#' The columns need not sum to one. If they do not, it implies 
#' that when a person *here* (in the patches), 
#' some time is spent in places where they are not at risk (*e.g.* in an office building). 
#' It is expected that most 
#' time is spent in the patch where
#' the stratum resides. 
#'
#'  
#' @seealso [setup_timespent], [time_away], [time_at_risk]
#' 
#' @name time_spent_matrix
#' @aliases xds_info_time_spent 
NULL

#' @title Time at Risk
#' 
#' @description
#' 
#' 
#' \describe{
#'   \item{\eqn{\Psi}}{time-at-risk matrix}
#' }
#' 
#' @inheritSection setup_F_circadian Circadian Pattern Function 
#' @inheritSection F_TaR Time at Risk
#'
#' @seealso [xds_info_time_spent], [F_TaR], [setup_F_circadian],
#' [time_away], [available_blood_hosts]
#' @name time_at_risk
#' @aliases xds_info_time_at_risk
NULL

