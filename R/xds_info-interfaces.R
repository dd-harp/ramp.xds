# X 

#' @title Dynamical Interfaces
#'
#' @description
#' In the modular design, the dynamical processes interact through two interfaces:
#'
#' @section XY Interface:
#'
#' The **XY** interface coordinates parasite
#' **Transmission Dynamics**
#'  through mosquito biting and blood feeding, and computes the dynamical terms
#'  that pass information between the
#'  **X** and **Y** processes. The XY interface has three parts:
#'
#' - [BloodFeeding] (see the [Blood Feeding vignette](https://dd-harp.github.io/ramp.xds/articles/BloodFeeding.html)) -- mosquitoes *search* for blood hosts, so the algorithms that model
#'  blood feeding were developed around the concept of blood host availability. Models can set
#'  up constant blood feeding rates, but the parameter values should satisfy some constraints:
#'
#'    - the blood feeding rate should be 0 if there are no hosts available;
#'
#'    - the human fraction should be 0 if there are no humans available;
#'
#'    - the human fraction should be 1 if only humans are available;
#'
#' - [Transmission] (see the [Transmission vignette](https://dd-harp.github.io/ramp.xds/articles/Transmission.html)) -- under the blood-feeding model, the biting
#'    distribution matrix (\eqn{\beta}) ensures that every bite and blood meal
#'    is accounted for:
#'    - it updates the local fraction (if there are visitors)
#'    - it computes the local
#'    daily entomological inoculation rate (EIR or \eqn{E}) for all the strata
#'    - it computes net infectiousness (NI or \eqn{\kappa}) in each patch.
#'
#' - [Exposure] (see the [Exposure vignette](https://dd-harp.github.io/ramp.xds/articles/Exposure.html)) -- handles mosquito blood feeding from the human or host perspective. The function
#'    computes the force of infection (FoI) for all the population strata from:
#'    - the local daily EIR (\eqn{E})
#'    - the travel EIR (\eqn{\delta})
#'    - an assumption about the distribution of the expectation
#'    - a model of partial immunity (\eqn{b}) from the **X** component
#'
#' @section ML Interface:
#'
#' **Mosquito Ecology:** the **L** and **M** processes communicate through the habitat interface. Aquatic
#' habitats are located in the patches and configured during basic setup by passing the habitat membership vector. See the [Habitats and Mosquito Ecology vignette](https://dd-harp.github.io/ramp.xds/articles/EggLaying.html).
#'
#'  - **Patch Dynamics** -- adult mosquito populations are distributed in patches (see [xds_info_patch_dynamics])
#'
#'  - **Aquatic Habitats** -- aquatic habitats are located in patches (see [xds_info_aquatic_habitats])
#'
#'      - [Emergence] --- a function that uses the habitat matrix to aggregate adults emerging from aquatic habitats to patches
#'
#'      - [EggLaying] --- a function that uses the habitat matrix and habitat search weights to distribute eggs laid by adults in a patch among the aquatic habitats in that patch.
#'
#' @seealso
#' + XY Interface
#'    - [xds_info_blood_feeding]
#'    - [xds_info_transmission]
#'    - [xds_info_exposure]
#' + ML Interface
#'    - [xds_info_emergence]
#'    - [xds_info_egg_laying]
#'
#' @name xds_info_interfaces
NULL