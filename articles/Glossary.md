# Glossary

This glossary defines the core terminology used in **`ramp.xds`**. Terms
are grouped by theme. For functions related to a given concept, see
`help.search("term", fields = "concept")`.

------------------------------------------------------------------------

## Model Structure

- **xds object**:

  The central data structure in **`ramp.xds`**, an `R` list of class
  `xds_obj`. It holds the model components, parameters, initial values,
  outputs, and metadata needed to define and solve a dynamical system
  for malaria transmission. Built by
  [`xds_setup()`](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
  and its wrappers.

- **Dynamical Component**:

  One of the three parts of an **`xds`** model: the human / host
  component (**XH**), the adult mosquito component (**MY**), and the
  aquatic / immature mosquito component (**L**). Each component defines
  a set of state variables and dynamics.

- **module**:

  A specific implementation of a component. For example, `macdonald`,
  `GeRM`, and `SI` are modules for the MY component; `SIS` and `hMoI`
  are modules for the XH component. Each module is implemented as a set
  of S3 methods dispatching on the module class name.

- **patch**:

  The basic spatial unit for mosquito ecology and transmission. A model
  has `nPatches` patches. Adult mosquitoes live in patches, which are
  connected by dispersal. Blood feeding, egg laying, and transmission
  are computed at the patch level.

- **stratum**:

  The basic unit of human / host population structure. A model has
  `nStrata` strata within each host species. Strata represent
  sub-populations that may differ in location, risk, age, or other
  attributes. Transmission exposure is computed for each stratum.

- **habitat**:

  The basic spatial unit for immature mosquito ecology. A model has
  `nHabitats` habitats. Aquatic mosquito populations develop in
  habitats, which are distributed across patches according to the
  habitat membership matrix.

------------------------------------------------------------------------

## Mosquito Bionomics

- **bionomics**:

  The set of mosquito life-history traits that govern transmission
  potential. In **`ramp.xds`**, the core bionomic parameters are the
  blood feeding rate (\\f\\), the human blood feeding fraction (\\q\\),
  the mosquito mortality rate (\\g\\), and the emigration rate
  (\\\sigma\\).

- **\\f\\ — blood feeding rate**:

  The per-capita rate at which a mosquito takes blood meals per day.
  Together with \\q\\, it determines the rate of contact between
  mosquitoes and humans.

- **\\q\\ — human blood feeding fraction**:

  The fraction of blood meals taken on humans (as opposed to other
  vertebrate hosts). The product \\fq\\ gives the human biting rate per
  mosquito per day.

- **\\g\\ — mosquito mortality rate**:

  The per-capita daily mortality rate of adult mosquitoes. The daily
  survival probability is \\e^{-g}\\.

- **\\\sigma\\ — emigration rate**:

  The per-capita daily rate at which mosquitoes leave a patch. Together
  with the dispersal kernel, \\\sigma\\ determines the spatial
  redistribution of mosquitoes.

- **\\\Omega\\ — mosquito mortality matrix**: A matrix that combines
  mortality and emigration to describe the net loss of mosquitoes from
  each patch. Used in computing steady states.

- **\\\Upsilon\\ — mosquito survival / transition matrix**: A matrix
  describing survival and dispersal over the extrinsic incubation period
  (EIP). It is used to compute the proportion of mosquitoes that survive
  to become infectious.

------------------------------------------------------------------------

## Transmission

- **EIR — Entomological Inoculation Rate**:

  The number of infectious bites received per person per unit time. The
  daily EIR (dEIR) is the per-day rate; the annual EIR (aEIR) integrates
  over a year. The EIR is the primary measure of transmission intensity
  from mosquitoes to humans. Use
  `help.search("EIR", fields = "concept")` to find related functions.

- **\\\beta\\ — blood feeding / mixing matrix**:

  A matrix of dimension `nStrata` × `nPatches` that describes the
  distribution of bites from mosquitoes in each patch onto human strata.
  It reflects both the spatial distribution of humans and their relative
  attractiveness to mosquitoes.

- **\\\kappa\\ — net infectiousness**:

  The probability that a mosquito taking a blood meal on a human
  population becomes infected, averaged over all strata. Computed as a
  weighted sum of per-stratum infectiousness \\c_i\\ weighted by human
  population size.

- **\\b\\ — mosquito-to-human transmission efficiency**: The probability
  that an infectious bite results in a human infection. Also called the
  sporozoite transmission efficiency.

- **\\c\\ — human-to-mosquito transmission efficiency**: The probability
  that a mosquito becomes infected when taking a blood meal on an
  infectious human. Also called the gametocyte transmission efficiency.

- **\\Z\\ — infective mosquito density**:

  The density of infectious (sporozoite-positive) adult mosquitoes per
  patch. Combined with \\fq\\ and \\\beta\\, \\Z\\ determines the EIR
  experienced by each human stratum.

- **\\fqZ\\ — infective biting density**:

  The product of the human biting rate (\\fq\\) and the infective
  mosquito density (\\Z\\), giving the rate of infectious bites per unit
  area per day in each patch.

- **FoI — Force of Infection**: The per-capita rate at which susceptible
  humans become infected per unit time. The FoI depends on the EIR and
  the exposure model (Poisson, negative binomial, etc.).

- **local fraction**: The fraction of bites in a patch that are on
  resident (non-travelling) humans, as opposed to visitors. Used to
  partition the EIR between local exposure and travel-related exposure.

------------------------------------------------------------------------

## Epidemiology

- **PR — Parasite Rate (Prevalence)**:

  The proportion of the human population that is infected at a given
  time. Also called *Pf*PR when referring specifically to *Plasmodium
  falciparum*. Can be measured by microscopy (`true_pr`), PCR, or rapid
  diagnostic test.

- **aEIR — annual EIR**: The EIR integrated or summed over a full year
  (365 days). A common summary statistic for transmission intensity.

- **AR — Attack Rate**: The cumulative probability of infection over a
  given time period. Related to the FoI via the exposure model.

------------------------------------------------------------------------

## Model Dynamics

- **xde — differential equation model**:

  A model solved as a system of differential equations, either ordinary
  (`ode`) or delay (`dde`). Set via `xds_obj$xde`. Solved using routines
  from `deSolve`.

- **ode — ordinary differential equation**:

  The default form of an **`xde`** model, where the right-hand side
  depends only on the current state.

- **dde — delay differential equation**:

  An **`xde`** model in which the right-hand side depends on the state
  at earlier times, specifically at the lagged time \\t - \tau\\ where
  \\\tau\\ is the extrinsic incubation period (EIP). Used by modules
  such as `GeRM`.

- **dts — discrete time system**:

  A model solved by iterating a map (difference equations) rather than
  integrating differential equations. Set via `xds_obj$xde = "dts"`.
  Used by the `RMdts` and `basicM_dts` modules.

------------------------------------------------------------------------

## Forcing & Exogenous Variables

- **forcing**:

  Exogenous (externally imposed) variation in model inputs, such as
  seasonal mosquito emergence driven by rainfall. Models can be forced
  via emergence (`Lambda`), the EIR (`eir`), or other mechanisms.

- **trace function**:

  A function of time \\t\\ used to specify an exogenous input to the
  model. Built using
  [`make_function()`](https://dd-harp.github.io/ramp.xds/reference/make_function.md)
  and parameterised with `makepar_*` helpers. Trace functions can
  represent constant, seasonal, trend-driven, or spline-interpolated
  signals.

- **seasonality**:

  Periodic, typically annual, variation in transmission-relevant
  quantities such as mosquito emergence, rainfall, or EIR. Represented
  in **`ramp.xds`** via the `F_season` family of functions.

- **trend**:

  A long-term directional change in a forcing variable, independent of
  seasonality. Represented via the `F_trend` family of functions.

------------------------------------------------------------------------

## Spatial Structure

- **dispersal kernel**:

  A matrix (or function used to build one) that describes the
  probability that a mosquito emigrating from one patch settles in each
  other patch. See
  [`setup_K_matrix()`](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md).

- **residency matrix**: A matrix describing which patch each human
  stratum is resident in. Used to weight exposure and compute
  patch-level averages.

- **travel**: Exposure to infectious bites outside a person’s home
  patch. Modelled via the travel EIR and the visitor dynamics interface.
