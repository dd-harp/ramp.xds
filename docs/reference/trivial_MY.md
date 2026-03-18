# The `trivial` Module for the MY Component

Implements the **MY** component using a trivial (trace) model where the
density of infectious mosquitoes \\Z\\ is specified as a forced function
of time rather than computed dynamically.

## State Variables

No dynamic state variables — \\Z\\ is supplied as a trace function.

## Parameters

- `F_Z`:

  a function of time returning the density of infectious mosquitoes
