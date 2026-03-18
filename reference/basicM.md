# The `basicM` Module for the MY Component

Implements the **MY** component using a basic model of adult mosquito
ecology without explicit infection dynamics.

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of adult mosquitoes that have fed at least once

## Parameters

- `f`:

  blood feeding rate

- `g`:

  mosquito mortality rate

- `sigma`:

  emigration rate

- `mu`:

  emigration loss rate

- `K`:

  mosquito dispersal matrix

- `Omega`:

  adult mosquito demographic matrix (mortality + migration)

## Dynamics

\$\$ \begin{array}{rl} dM/dt &= \Lambda - \Omega \cdot M \\ dP/dt &=
f(M-P) - \Omega \cdot P \\ \end{array}\$\$
