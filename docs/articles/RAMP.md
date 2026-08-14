# RAMP

## Satellite Packages

**`ramp.xds`** handles all core computation, but its utility is extended
by several satellite packages:

- [**`ramp.library`**](https://dd-harp.github.io/ramp.library/) is an
  extended **library of dynamical systems** — stable code that has been
  tested and verified. It includes a large set of model families
  published in the peer-reviewed literature that are not included in
  **`ramp.xds`**. The ability to reuse code reduces the costs of
  replicating studies. Through this library, **`ramp.xds`** also
  supports nimble model building and analytics for other mosquito-borne
  pathogens.

- [**`ramp.forcing`**](https://dd-harp.github.io/ramp.forcing/) is a
  collection of utilities to model **exogenous forcing, vector control,
  and other forms of disease control** in models for **`ramp.xds`**.

- [**`ramp.trace`**](https://dd-harp.github.io/ramp.trace/) is a **trace
  function** library for **RAMP**

- [**`ramp.demog`**](https://dd-harp.github.io/ramp.qa/) is a
  supplementary code library for **`ramp.xds`** that handles **human
  demography and stratification**, including vital dynamics and age
  structure.

- [**`ramp.work`**](https://dd-harp.github.io/ramp.work/) includes
  algorithms to apply the framework, including code to fit models to
  data and to do constrained optimization.

- [**`ramp.qa`**](https://dd-harp.github.io/ramp.qa/) has method for
  **qualitative analysis**, including thresholds, scaling relationships,
  and connectivity
