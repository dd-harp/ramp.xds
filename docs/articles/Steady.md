# Steady States & Stable Orbits

**`ramp.xds`** computes steady states and stable orbits as part of the
[qualitative
analysis](https://dd-harp.github.io/ramp.xds/articles/Qualitative.md) of
dynamical systems for mosquito-transmitted pathogens.

## Steady States

Steady states are computed in two ways.

1.  If there is a closed form expression for the steady state of one of
    the dynamical modules, it can be computed as a function of the
    inputs. There are 5 generics that compute the steady states:

    - `steady_state_L` – given a static egg deposition rate, compute the
      steady states, including the emergence rate of adult mosquitoes

    - `steady_state_M` – given a static emergence rate of adult
      mosquitoes from aquatic habitats, compute the steady state
      mosquito population density

    - `steady_state_MY` – given a static emergence rate of adult
      mosquitoes from aquatic habitats, and given a static net
      infectiousness (\\\kappa\\), compute the steady states of an adult
      population model

    - `steady_state_X` – given a static human population size, compute
      the steady states of a model for infection and immunity in the
      human population

    - `steady_state_XH` – compute the steady state for a model of
      demography, infection and immunity

2.  The function `xds_steady` computes the steady state numerically. The
    function has two optional arguments, a runtime, `Y` (in years,
    default `Y=10`) and a tolerance argument (default `tol=1e-5`). The
    system is repeatedly solved, and the sum of squared errors of the
    final states are checked. The system stops after the difference is
    less than `tol` or after iterating 10 times.

The algorithms assume that the equilibrium is globally, asymptotically
stable. This may not be true for some models.

## Steady Orbits
