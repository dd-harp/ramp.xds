# The `basicL` Module for the L Component

Implements the **L** component using a basic model of aquatic mosquito
ecology with density-dependent mortality and delayed maturation, based
on Smith DL, *et al.* (2013).

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

## Dynamics

\$\$dL/dt = \eta - (\psi e^{-\xi L} + \phi + \theta L) L\$\$

Emergence rate: \$\$\alpha = \psi e^{-\xi L} L\$\$

Setting \\\xi = 0\\ recovers the model of Smith DL, *et al.* (2013).

## References

Smith DL, *et al.* (2013)
