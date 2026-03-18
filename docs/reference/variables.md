# Variables

The term *variables* is overloaded.

## Terminology

In **`ramp.xds,`** there are several kinds of variables and terms:

- the **independent variable** is usually time, but it is sometimes age

- the **dependent variables** or *state variables* describe the *state*
  of the system - most dependent variables are computed as part of a
  **Core Dynamical Components**

      - **other state variables** are not can be set up and computed along with state variables (see [Other_State_Variables])

- **accessory variables** are computed for other reasons. For example,
  accessory variables are used in delay differential equations to
  internalize computation of lagged terms using
  [deSolve::lagderiv](https://rdrr.io/pkg/deSolve/man/timelags.html)
  (see [Delay
  Equations](https://dd-harp.github.io/ramp.xds/articles/Delays.html))
