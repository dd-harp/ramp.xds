# The `SIS` Module for the XH Component

Implements the **XH** component using a Susceptible-Infected-Susceptible
(SIS) compartmental model of human infection dynamics.

## State Variables

- `H`:

  total human (or host) population density

- `I`:

  density of infected humans

Note: susceptible density \\S = H - I\\.

## Parameters

- `r`:

  clearance rate for infections

- `xi`:

  mass treatment rate (\\\xi(t)\\)

- `B`:

  time-dependent birth rate function \\B(t, H)\\

- `D`:

  linear operator (matrix) for mortality, migration, aging, and
  transfers

## Dynamics

\$\$ \begin{array}{rl} dH/dt &= B(t,H) + D \cdot H \\ dI/dt &= h(H-I) -
rI - \xi(t) + D \cdot I \\ \end{array}\$\$ where \\h\\ is the force of
infection computed upstream.
