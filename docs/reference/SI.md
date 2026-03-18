# The `SI` Module for the MY Component

Implements the **MY** component using a simple SI (Susceptible-Infected)
model of adult mosquito infection dynamics.

## State Variables

- `M`:

  density of adult mosquitoes

- `Y`:

  density of infected adult mosquitoes

## Parameters

- `f`:

  blood feeding rate

- `q`:

  human blood fraction

- `eip`:

  extrinsic incubation period (\\\tau\\)

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

The density of infectious mosquitoes is given by: \$\$Z = e^{-\Omega
\tau} \cdot Y\$\$ \$\$ \begin{array}{rl} dM/dt &= \Lambda - \Omega \cdot
M \\ dY/dt &= fq\kappa(M-Y) - \Omega \cdot Y \\ \end{array}\$\$
