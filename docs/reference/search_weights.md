# Search Weights

Mosquitoes search for resources, including vertebrate animals for blood
feeding and aquatic habitats for egg laying. In developing a modular
design, a rigorous interface was needed to guarantee mathematical
consistency for blood feeding and parasite transmission, and for adult
mosquito ecology and behaviors around egg laying. Models for
*heterogeneous biting* among host strata and *heterogeneous egg laying*
among habitats are implemented through *search weights.*

A search weight is a scalar value assigned to each human population
stratum or habitat that is used to compute *availability* to mosquitoes
and relative biting rates or relative laying rates. The value of these
search weights are configurable during basic setup.

## Basic Setup Options

- `searchB`:

  a vector of search weights for blood feeding:
  `length(searchB)=nStrata`

- `searchQ`:

  a vector of search weights for egg laying: `length(searchQ)=nHabitats`

By default, these search values are all set to 1. The

## Blood Search Weights

Blood feeding is an interaction among humans and mosquitoes: in this
model, humans are spending time, and mosquitoes are searching. To model
exposure, each human (or host) population stratum is assigned a *search
weight,* a number used to weigh *time spent* and get a measure of
*availability.* Search weights for blood feeding thus play a key role in
blood feeding (see
[blood_feeding](https://dd-harp.github.io/ramp.xds/reference/blood_feeding.md))

## Habitat Search Weights

where

## See also

[blood_feeding](https://dd-harp.github.io/ramp.xds/reference/blood_feeding.md),
transmission, and
[egg_laying](https://dd-harp.github.io/ramp.xds/reference/egg_laying.md)
