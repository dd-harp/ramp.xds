# The `trivial` Module for the XH Component

Implements the **XH** component using a trivial (trace) model where net
infectiousness \\\kappa\\ is specified as a forced function of time
rather than computed from dynamic infection states. This module has no
state variables.

## State Variables

None — \\\kappa\\ is supplied as a trace function.

## Parameters

- `kappa`:

  net infectiousness of the human population (trace function)

- `HPop`:

  human population density
