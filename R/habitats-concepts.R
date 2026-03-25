#' @title Aquatic Habitats
#'  
#' @description
#' 
#' Aquatic habitats are distributed among patches on a landscape.   
#'\describe{
#'   \item{`membership`}{a vector: the patch index of each aquatic habitat}
#'   \item{`nHabitats`}{the number of habitats (`length(membership)`) }
#'   \item{`searchQ`}{a vector: a search weight for each aquatic habitat}
#' } 
#'  
#' @section The Habitat Matrix: 
#' 
#' The *habitat matrix* is an `nPatches` \eqn{\times} `nHabitats` matrix called \eqn{N}. 
#' It is constructed from the `membership` vector. 
#' In the matrix, \eqn{N_{i,j}} is 1 if the \eqn{i^{th}} patch contains the \eqn{j^{th}} habitat.  
#' For example, a membership vector \eqn{(1,1,2)} has a corresponding habitat matrix:
#' \deqn{
#' N = \left[ 
#' \begin{array}{ccc}
#' 1&1&0\\
#' 0&0&1\\
#' \end{array}
#' \right]
#' }
#' If \eqn{\alpha} denotes the emergence rate of adult mosquitoes 
#' from each habitat, then the emergence rate at the patch, \eqn{\Lambda} is:
#' \deqn{\Lambda = N \cdot \alpha} 
#' 
#' @section The Egg Distribution Matrix: 
#' 
#' Let \eqn{\omega} denote the habitat search weights. Habitat availability (\eqn{Q}) is computed as:
#' \deqn{Q = N \cdot \omega.} 
#' Mosquitoes could also lay some eggs in available bad habitats (\eqn{B}) or in available ovitraps (\eqn{O}). 
#' The egg distribution matrix (\eqn{U}) is computed as
#' \deqn{U = \left( \mbox{diag} \left(\omega \right) \cdot N^T \right) \cdot \mbox{diag}\left( (Q+B+O)^{-1} \right).}
#' In models where there are patches with no available habitat, \eqn{(Q+B+O)^{-1}} is computed using the function [diag_inverse], which 
#' fixes the divide-by-zero problem.  
#' 
#' @name xds_info_aquatic_habitats 
NULL