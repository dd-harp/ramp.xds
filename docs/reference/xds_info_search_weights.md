# Search Weights

- `XH_obj[[i]]$search_weights`:

  blood-feeding search weights for host species `i`, with length
  `nStrata[i]`

- `L_obj[[s]]$search_weights`:

  egg-laying search weights for vector species `s`, with length
  `nHabitats`

By default, the values are all set to 1.

Mosquitoes search for resources, including vertebrate animals for blood
feeding and aquatic habitats for egg laying. In developing a modular
design, a rigorous interface was needed to guarantee mathematical
consistency for blood feeding and parasite transmission, and for adult
mosquito ecology and behaviors around egg laying. Models for
*heterogeneous biting* among host strata and *heterogeneous egg laying*
among habitats are implemented through *search weights.*

A search weight is a scalar value assigned to each human population
stratum or habitat that is used to compute *availability* to mosquitoes
and relative biting rates or relative laying rates. The values of these
search weights are configurable during basic setup.
