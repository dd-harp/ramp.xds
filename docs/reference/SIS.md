# The `SIS` module for the XH component

In the SIS compartmental model, infected individuals are either
susceptible (S), or infected and infectious (I). Susceptible individuals
become infected at the per-capita rate \\h\\. Infected individuals clear
infections at the per-capita rate \\r\\, and then they become
susceptible to infection again.

Human/host population size (H) is a state variable that can be
configured.

and \\H=S+I\\: the model computes \\dH/dt\\ and \\dI/dt\\. When outputs
are parsed, S is computed as \\S=H-I\\.

This module also includes:

- a model for human demographic changes; and

- a port for mass treatment, \\\xi(t)\\.

## State Variables

- `H`:

  total human (or host) population density

- `I`:

  density of infected humans

During parsing, the density of susceptibles is computed as: \$\$S = H -
I\$\$.

## Parameters

- `r`:

  clearance rate for infections

- `xi`:

  mass treatment rate

- `B`:

  time-dependent birth rate function \\B(t, H)\\

- `D`:

  human demographic matrix

## Dynamics

\$\$ \begin{array}{rl} dH/dt &= B(t,H) + D \cdot H \\ dI/dt &= h(H-I) -
rI - \xi(t) + D \cdot I \\ \end{array}\$\$ where \\h\\ is the force of
infection computed upstream.
