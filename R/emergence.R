# X 

#' @title Compute emerging adults
#' 
#' @description Compute emerging adults at time \eqn{t}.
#' 
#' @section Emergence: 
#' 
#' During setup, the habitat matrix \eqn{(\mathbf{N})} is built from
#' the habitat membership vector by [make_habitat_matrix]. The function
#' [Emergence] calls [F_emerge] to get the adult mosquitoes emerging from 
#' each habitat \eqn{(\alpha)} and computes and stores 
#' emergence by patch (`Lambda` or \eqn{\Lambda}): 
#' \deqn{\Lambda = \mathbf{N} \cdot \alpha}
#' 
#' @inherit xds_info_emergence description
#' 
#' @seealso [xds_info_interfaces]
#' 
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** model object
#' @export
#' @keywords internal
Emergence = function(t, y, xds_obj){
  for(s in 1:xds_obj$nVectorSpecies){
    N = xds_obj$habitats$matrix
    alpha = F_emerge(t, y, xds_obj, s)
    xds_obj$terms$alpha[[s]] = alpha
    xds_obj$terms$Lambda[[s]] = N %*% alpha
  } 
  return(xds_obj)
}
 