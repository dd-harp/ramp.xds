# The `basicL` module for the L component

A basic model of aquatic mosquito ecology with density-dependent
mortality and delayed maturation, based on Smith DL, *et al.* (2013).

## State Variables

- `L`:

  density of mosquito larvae in each habitat

## Parameters

- `psi`:

  maturation rate (\\\psi\\)

- `xi`:

  delayed maturation parameter in response to mean crowding (\\\xi\\)

- `phi`:

  density-independent death rate (\\\phi\\)

- `theta`:

  slope of density-dependent mortality (\\\theta\\)

## Egg Laying

- `eta`:

  the egg laying rate, (\\\eta\\)

## Dynamics

\$\$dL/dt = \eta - (\psi e^{-\xi L} + \phi + \theta L) L\$\$

## Emergence

The emergence rate of adult, female mosquitoes from each patch is:
\$\$\alpha = \psi e^{-\xi L} L\$\$

## Notes

Setting \\\xi = 0\\ recovers the model of Smith DL, *et al.* (2013).

## References

Smith DL, Perkins TA, Tusting LS, Scott TW, Lindsay SW (2013) Mosquito
Population Regulation and Larval Source Management in Heterogeneous
Environments. PLOS ONE 8(8): e71247.
[https://doi.org/10.1371/journal.pone.0071247](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0071247).
