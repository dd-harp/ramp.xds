# The `macdonald` Module (MY Component)

Implements a Macdonald-style delay differential equation model of adult
mosquito ecology and infection dynamics, consistent with the model
published by George Macdonald in 1952. This formulation is actually
closer to one published by

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

- `g`:

  mortality rate

- `sigma`:

  patch emigration

- `mu`:

  emigration-related loss

- `K`:

  the dispersal matrix

- `tau`:

  extrinsic incubation period (\\\tau\\)

The *demographic matrix* \\\Omega\\ is formulated as \$\$ \Omega =
\mbox{diag} \left( g \right) + K \cdot \mbox{diag} \left( \sigma
\left(1-\mu\right) \right) \$\$ Survival and dispersal through the EIP
is \$\$ \Upsilon = e^{-\Omega \tau} \$\$

## Dynamics

The dynamical system is described by a coupled set of delay differential
equations. In these equations, a sub-scripted variable or term denotes
its lagged value: \\M\_\tau = M(t-\tau)\\.

\$\$ \begin{array}{rl} dM/dt &= \Lambda - \Omega \cdot M \\\\ dY/dt &=
fq\kappa(M-Y) - \Omega \cdot Y \\\\ dZ/dt &= \Upsilon \cdot
(fq\kappa)\_\tau(M\_\tau-Y\_\tau) - \Omega \cdot Z \\\\ \end{array} \$\$

## Note

This model is not capable of handling exogenous forcing by weather or
vector control. Use the `GeRM` module instead.
