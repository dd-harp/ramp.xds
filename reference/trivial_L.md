# The `trivial` Module for the L Component

Implements the **L** component using a trivial (trace) model where the
emergence rate \\\alpha\\ is specified as a forced function of time
rather than computed dynamically. This module has no state variables.

## State Variables

None — \\\alpha\\ is supplied as a trace function.

## Parameters

- `F_alpha`:

  a function of time returning the emergence rate
