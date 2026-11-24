# X 

#' @title The habitat vector
#'
#' @description
#'
#' Each aquatic habitat is located in a patch.
#'
#' The habitat vector is passed to a basic setup function
#' through the argument `membership`.
#'
#' + `membership` is a vector of positive integers
#' + `nHabitats <- length(membership)`
#'
#' @note Information
#' about patch locations is stored in the
#' *habitat vector.*
#'
#' @section Habitat Vector:
#'
#' A habitat vector \eqn{(V)} indexes
#' the patches where each aquatic habitat can be found.
#'
#' If the \eqn{i^{th}}
#' habitat is located in the \eqn{j^{th}} patch, then \eqn{V_i = j.}
#'
#' @seealso [xds_basic_setup], [habitat_matrix]
#'
#' @name habitat_vector
NULL

#' @title The habitat matrix
#'
#' @description
#' \describe{
#'   \item{\eqn{\mathbf N}}{the habitat matrix}
#' }
#'
#' @section Habitat Matrix:
#'
#' The habitat matrix \eqn{(\mathbf N)} is an `nPatches`\eqn{\times}`nHabitats` matrix
#' that is constructed from the [habitat_vector].
#'
#' If the \eqn{i^{th}} habitat is in the \eqn{j^{th}} patch, then
#' \eqn{{N}_{j,i}=1.} Otherwise, \eqn{{N}_{j,i}=0.}
#'
#' If \eqn{x} is any vector describing a quantity in
#' habitats (*i.e.*, \eqn{\left|x\right|=}`nHabitats`), then
#' \deqn{X=\mathbf{N}\cdot x} is a vector that has summed \eqn{x}
#' by patch, and \eqn{\left|X\right|=}`nPatches`.
#'
#' @seealso [habitat_vector], [make_habitat_matrix], [habitat_availability]
#'
#' @name habitat_matrix
NULL

#' @title Available habitats
#'
#' @description
#'
#' \describe{
#'   \item{\eqn{w}}{habitat search weights}
#'   \item{\eqn{Q}}{habitat availability}
#' }
#'
#' @section Habitat Availability:
#'
#' Egg laying by adult mosquitoes was modeled as the outcome of
#' *searching* for aquatic habitats.
#'
#' Each habitat is assigned a *search weight* \eqn{(w).}
#'
#' Habitat availability in a patch is the sum of
#' the search weights of all the habitats in that patch.
#'
#' Habitat availability \eqn{(Q)} is computed
#' using the habitat matrix \eqn{(\mathbf{N})}:
#'
#' \deqn{Q = \mathbf{N}\cdot w}
#'
#' + Patch emigration rates
#' and egg laying rates by adult mosquitoes
#' can be modeled as a functional response to
#' habitat availability.
#'
#' @note
#' + \eqn{\left|w\right| =}`nHabitats`
#' + \eqn{\left|Q\right| =}`nPatches`
#'
#' @seealso [habitat_matrix], [laying_matrix]
#'
#' @name habitat_availability
#'
#' @aliases xds_info_available_habitats
NULL

#' @title The laying matrix
#'
#' @description
#' \describe{
#'   \item{\eqn{\mathbf{U}}}{The laying matrix}
#' }
#'
#' @section Laying Matrix:
#'
#' The egg laying matrix \eqn{(\mathbf {U})} distributes
#' the eggs laid by the adult mosquitoes in each patch
#' among the aquatic habitats in that
#' patch by relative availability.
#'
#' The algorithm is motivated by *searching* for aquatic habitat.
#' Each habitat is assigned a *search weight* \eqn{(w).} Habitat
#' availability \eqn{(Q)} sums the search weights by patch.
#' Of the eggs laid in productive habitats in the \eqn{j^{th}} patch,
#' the fraction laid in the \eqn{i^{th}} habitat is \eqn{w_i / Q_j.}
#'
#' This is computed by constructing the laying matrix, \eqn{\mathbf U},
#' an `nHabitats` \eqn{\times} `nPatches` matrix. To
#' avoid division-by-zero errors
#' for patches with no available habitats, a quantity
#' \eqn{\epsilon = 1e-14} is added
#' to availability:
#' \deqn{
#' \mathbf{U}=
#' \mbox{diag}\left( w \right)
#' \cdot \mathbf{N}^T \cdot
#' \mbox{diag} \left( \frac{1}{\textstyle{Q + \epsilon}} \right)
#' }
#'
#' @seealso [habitat_availability]
#'
#' @name laying_matrix
#' @aliases xds_info_laying_matrix
NULL

#' @title Egg Laying in **`ramp.xds`**
#'
#' @description
#'
#' \describe{
#'   \item{\eqn{\eta}}{The number of eggs laid in each habitat each day}
#'   \item{\eqn{G}}{Number of eggs laid by adult mosquitoes in each patch, each day}
#'   \item{\eqn{\zeta}}{Fraction of eggs laid in a suitable habitat}
#'   \item{\eqn{\mathbf{U}}}{The laying matrix}
#' }
#'
#' @section Egg Laying:
#'
#' **Egg Laying in Patches** --- The number of eggs laid by adult mosquitoes in each patch \eqn{(G)}
#' is computed by an **MY** component function [F_eggs], typically:
#' \deqn{G = F_G(M).}
#'
#' **Habitats and Traps** --- Aquatic habitats compete with "bad habitats" (*i.e.*, water bodies where eggs
#' are laid that are not productive) and ovitraps for eggs laid. The availability
#' of each one in the patch is given by:
#' + \eqn{Q} --- habitat availability (see [habitat_availability])
#' + \eqn{W} --- availability of unsuitable habitats
#' + \eqn{O} --- availability of ovitraps
#'
#' \deqn{\zeta = \frac{\textstyle{Q}}{\textstyle{Q+W+O}}}
#'
#' **Laying in Habitats** --- eggs laid in habitats are distributed by the
#' laying matrix \eqn{(\mathbf{U})}.
#'
#' \deqn{\eta = \mathbf{U} \cdot \zeta G }
#'
#' @seealso [F_eggs], [habitat_availability], [laying_matrix]
#' @name egg_laying
#' @aliases xds_info_egg_laying
NULL

#' @title Emergence in **`ramp.xds`**
#'
#' @description
#'
#' Adult mosquitoes emerge from pupae in aquatic habitats. The process
#' is handled through the habitat interface.
#'
#' @inheritSection Emergence Emergence
#'
#' @seealso [get_habitat_matrix], [make_habitat_matrix]
#'
#' @name xds_info_emergence
NULL

#' @title Unproductive Aquatic Habitats
#'
#' @description
#' Availability of unproductive aquatic habitats.
#'
#' @section Unproductive Aquatic Habitats:
#' To model egg laying, a port
#' was developed to model the availability
#' of unproductive aquatic habitats: water
#' where mosquitoes
#' lay eggs, but no mature adults ever emerge.
#'
#' @name bad_habitats
#' @aliases xds_port_bad_habitats
NULL

#' @title Ovitraps
#'
#' @description
#' Availability of ovitraps.
#'
#' @section Ovitraps:
#' To simulate ovitraps, a port
#' was developed to model the availability
#' of the ovitraps.
#'
#' @name ovitraps
#' @aliases xds_port_ovitraps
NULL