# The human population

A human (or host) population is sub-divided into *strata.* The human
population (or other host population) and its structure, size and
behavior are critical aspects of transmission.

- `HPop`:

  initial human population size: `length(HPop) = nStrata`

## Human Population Size, \\H\\

The human (or host) population size plays a key role in the blood
feeding and transmission interface. In most **XH** modules, it is
handled as a variable. Setup requires having some assigning a value to
H, so it is passed at setup as `HPop.` Since \\H\\ is part of the blood
feeding and transmission interface, changing its initial value triggers
functions to refresh the dynamical terms for models where \\dH/dt = 0\\.
