# The `GeRM` Module for the MY Component

Implements the **MY** component using a generalized, non-autonomous
Ross-Macdonald model of adult mosquito ecology and infection dynamics,
capable of handling exogenous forcing by weather and vector control.

## State Variables

- `M`:

  density of adult mosquitoes

- `Y`:

  density of infected adult mosquitoes

- `Z`:

  density of infectious adult mosquitoes

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

- `Upsilon`:

  survival and dispersal through the eip: \\\Upsilon = e^{-\Omega\tau}\\

## Dynamics

\$\$ \begin{array}{rl} dM/dt &= \Lambda - \Omega \cdot M \\ dY/dt &=
fq\kappa(M-Y) - \Omega \cdot Y \\ dZ/dt &= \Upsilon \cdot
(fq\kappa)\_\tau(M\_\tau-Y\_\tau) - \Omega \cdot Z \\ \end{array}\$\$
