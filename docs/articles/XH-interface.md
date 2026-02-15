# The XH Component

The **XH** component was designed for human malaria epidemiology,
defined in a narrow sense to include exposure, infection dynamics,
immunity, disease, infectiousness, diagnostics and detection. In the
broader sense, human malaria epidemiology also includes malaria
transmission dynamics, but these aspects are handled by other parts of
**`ramp.xds.`**

The **XH** component also handles models for the demography, infection
dynamics, and immunity for vertebrate hosts other mosquito-borne
diseases.

A library of **XH** modules can be found in
[**`ramp.library`**](https://dd-harp.github.io/ramp.library/).

This vignette takes a deep dive into the design and structure of the
**XH** component. It is useful for anyone who wants to learn more about
how the code works.

------------------------------------------------------------------------

## Overview

The **XH** component handles demography and epidemiology. The software
was designed to handle the epidemiology of *Plasmodium falciparum* in a
human population, but the interface has the capability of handling other
models for parasites and pathogens in human or vertebrate host
populations.

The interface handles malaria epidemiology in a narrow sense: it
includes exposure, infection, disease, immunity, infectiousness, and
diagnostics and detection. In the broad sense, malaria epidemiology
includes transmission dynamics and control, but these are handled by
other parts of the model.

Each **XH** module is defined by:

- An **X** component - disease dynamics

  - \\X\\: a state space describing infection and immune states, and
    possibly other variables

  - *dynamics:* a system of equations describing the state transitions

  - *infectivity:* a function \\F_b(X)\\ to compute partial immunity

  - *infectiousness:* a function \\F_I(X)\\ describing the effective
    infectious density

- An **H** component - demography

  - *births:* a popualtion birth rate, \\F_B(t, H)\\

  - *deaths & etc.* matrix describing morality and possibly aging,
    migration, and stratum dynamics

Since the **X** component is embedded in the **H** component, the two
processes are handled together.

### The **X** Component

Models for disease dynamics are developed around a general notion of
exposure defined by two metrics: daily entomological inoculation rates
(dEIR or \\E\\) and either

- the daily force of infection (dFoI or \\h\\), the hazard rate for
  infection for humans / hosts who are susceptible to infection.

- the daily attack rate (dAR or \\1-e^{-h}\\), the daily probability of
  infection for humans / hosts who are susceptible to infection.

Some of **X** component is handled in the interface that translates
local dEIR into a measure of the total dFoI.

Disease dynamics are specified by a function:

- for systems of differential equations, `dXHdt` requires the dFoI and
  computes the derivatives, denoted \\d X/dt\\ and \\d H/dt.\\

- for discrete time systems `Update_XHt` requires the dAR and updates
  the states, either \\X\_{t+1} = X_t + \Delta X\\ and \\H\_{t+1} =
  H_t + \Delta {H}.\\

All **XH** modules must include a function that computes the infectious
density \\I = F_I(X, H),\\ where \\I/H\\ is the fraction of bites that
would infect a perfectly competent mosquito.

All **XH** modules must include a function that computes true prevalence
\\x\\, from the states, \\x=F_x(X, H)\\.

Malaria models must include functions that compute predicted observed
prevalence by three diagnostic methods: by light microscopy, by rapid
diagnostic test, and by PCR. If the functions computing the predicted
*observed* prevalence by various diagnostics are not computable, not
relevant, or if they are a linear function of the true prevalence, then
this is reported as part of the *skill set.*

All models should include a *port* for mass treatment: terms to handle
mass drug administration or mass screen and treat are designed into some
modules and turned off by default. The capability to handle mass
treatment is reported as part of the the skill set.

### The **H** Component

The **H** component provides the ecological / demographic context for
disease dynamics. A large number of models has been developed that
assumes human / host population size is static. In **`ramp.xds,`** a
modules either:

- handle dynamically changing populations, and

  - the equations handle demographic changes

  - a constant population density model by default;

- **OR** report that demographic change is not part of its skill set

Models in **`ramp.xds`** (and in the satellite package
**`ramp.library`**) follow a convention in modeling demographic changes.
If had \\N_X\\ mutually exclusive and collectively exhaustive states,
the modules implement \\N_X-1\\ equations for the state variables. The
compartment not computed is the class that receives newborns, denoted
\\{X}\_0\\ (or one if these, if two or more classes get newborns.)

These models generally use \\H\\ to denote the human / host population
density, and the models are formulated to handle demography. Let \\B(t,
H)\\ denote a population birth rate. Mortality and aging are handled by
the demographic matrix, denoted \\D.\\ The models assume that \\H\\
could be a vector, so \\D\\ is a matrix: the diagonal includes
per-capita death rates. For differential equations, the models compute:\
% \\\frac{dH}{dt} = B(t) - D \cdot H - F\_\mu(X)\\ where \\F\_\mu(X)\\
is a set of terms describing disease-induced mortality.

The matrix \\D\\ is also applied to every state variable:

\\\frac{d X_i}{dt} = F_i({X}) - {D} \cdot {X}\_i\\ The value of
\\{X}\_0\\ is computed and assigned a name using:

\\X_0 = H - \sum\_{i\>0} X_i.\\

By default, the software sets \\B = D = F\_\mu(X) = 0;\\ models with
non-trivial demographic processes are handled as an advanced setup
option.

The demographic operator can also be modified to handle other dynamical
transitions among classes, which is handled through models for cohort
dynamics and .

## Required Functions

Each **XH** module includes 19 required functions and some optional
ones.

Some of these required functions are `S3` class functions in
[human-XH-interface.R](https://github.com/dd-harp/ramp.xds/blob/main/R/human-XH-interface.R).
Others are defined for each module.

One good example is the `SIS` module, posted in the `ramp.xds` github
repository
[human-XH-SIS.R](https://github.com/dd-harp/ramp.xds/blob/main/R/human-XH-SIS.R).

The required functions deal with various tasks for model building and
solving: constructing the **XH** model object; the dynamics; the
parameters; the variables and their initial values; computing terms and
standard outputs; and consistency checks. Optional outputs include other
metrics; functions to compute steady states; and module specific
functions to visualize the outputs.

Each module is defined by a string, generically called `Xname,` that
identifies the module: *e.g.* `SIS.`

### Dynamics

1.  The dynamics are defined by at least one of the following is
    required, depending on whether the model family is a system of
    differential equations or a discrete time system:

    - `dXHdt.Xname` :: differential equations are defined by a function
      that computes the derivatives. In `ramp.xds` these are encoded in
      a function called `dXHdt.` The function is set up to be solved by
      [`deSolve::ode`](https://rdrr.io/pkg/deSolve/man/ode.html) or
      `deSolve::dede.`

    - `Update_XHt.Xname` :: discrete time systems are defined by the
      function that updates the state variables in one time step. In
      `ramp.xds` these are encoded in a function called `Update_XHt`
      that computes and returns the **state variables.** The forms mimic
      the ones used for differential equations.

------------------------------------------------------------------------

## XH Model Object

Each module has a pair of functions that set up a structured list called
the **XH** model object. The object is a list that is assigned to a
`class` that dispatches the `S3` functions described below. It is a
compound list, where some of the sub-lists are assigned their own
`class` that dispatch other `S3` functions.

2.  `make_XH_obj_Xname` :: returns a structured list called an **XH**
    model object:

    - the parameter values are stored by name

    - `class(XH_obj)` = `Xname`

    - the indices for the model variables are stored as `XH_obj$ix`

    - the initial values are stored as `XH_obj$inits`

    - ports are added and set to their `null` values:

      - a function to model the population birth rate

      - a demographic matrix for deaths, aging, and more

      - functions to model mass treatment

    - anything else that is needed can be configured here

3.  `setup_XH_obj.Xname` is a wrapper that calls `make_XH_obj_Xname` and
    (for the \\i^{th}\\ species) attaches the object as
    `xds_obj$XH_obj[[i]]`

### Parameters

4.  `change_XH_pars.Xname` changes the values of some parameters. It is
    designed to be used after setup. New parameter values are passed by
    name in a list called `options.`

5.  `get_XH_pars.Xname` is a utility to inspect the values of the
    parameters.

### Variables

Since the **XH** component is one of three, a function sets up the
indices for all the variables in a model.

Two other functions use those indices: one pulls the variables from the
state variable vector \\y\\; the other one pulls the variables by name
from an output matrix returned by `xds_solve.`

After pulling, both functions return the variables in by name in a list
to make it easy to inspect or use.

6.  `setup_XH_ix.Xname` - is the function that assigns an index to each
    variable in the model, and stores it as `xds_obj$XH_obj[[i]]$ix.`
    The indices are returned as a named list.

7.  `get_XH_vars.Xname` - retrieves the value of variables from the
    state variables vector \\y\\ at a point in time and returns the
    values by name in a list; the function gets called by `dXHdt` and by
    `change_XH_inits` and it can be useful in other contexts.

8.  `parse_XH_orbits.Xname` - this function is like `get_XH_vars` but it
    parses the matrix of outputs returned by `xds_solve.`

### Initial Values

A set of functions is sets up or changes the initial values for the
state variables.

9.  `make_XH_inits_Xname` - each model must include a function that
    makes a set of initial values as a named list. This function does
    not belong to any `S3` class, so it can take any form. The function
    should supply default initial values for all the variables. These
    can be overwritten by passing new initial values in `options.`

10. `setup_XH_inits.Xname` - is a wrapper, that gets called by
    `xds_setup` and that calls `make_XH_inits_Xname.` The setup
    `options` are passed to overwrite default values. The initial values
    are stored as `XH_obj$inits.`

11. `change_XH_inits.Xname` - a utility to change the initial values.

### Dynamical Terms

These functions compute dynamical terms – the outputs passed to an
interface.

12. `F_X.Xname` - compute the effective infectious density of the
    vertebrate hosts. This gets computed and passed through the blood
    feeding and transmission interface to compute \\\kappa.\\

13. `F_H.Xname` - get the human/host population density

14. `F_infectivity.Xname` - compute the probability a host will become
    infected by each infectious bite. This function gets called by the
    function `Exposure,` a part of the blood feeding and transmission
    interface that translates local and travel EIR into an estimated FoI
    under a model of environmental heterogeneity.

### Standard Outputs

Each module must output a few key quantities:

15. `F_prevalence.Xname` - compute prevalence

16. `F_ni.Xname` - compute net infectiousness (NI) for each stratum. If
    \\F_X \rightarrow X\\ and \\F_H \rightarrow H\\, then \\F\_{ni}
    \rightarrow X/H.\\ The function gets called after solving, and the
    NI is attached as a term for inspection and visualization.

17. `HTC.Xname` - compute the human transmitting capacity. This is used
    by functions in **`ramp.work`** that compute threshold conditions.

### Consistency Checks

Some modules in **`ramp.xds`** or **`ramp.library`** have been included
for various reasons. Not all of those models are capable of being
extended. To help users avoid using models in ways that are not
appropriate, we developed two function classes:

18. `skill_set_XH.Xname` :: describes model capabilities and limitations

19. `check_XH.Xname` :: at the end of `xds_setup` and at the beginning
    of `xds_solve,` this function gets run to ensure that some
    quantities have been properly updated, and to see if anything has
    been added to a model that is not in its skill set.

## Optional Functions

The **XH** interface also sets up `S3` classes for some optional
functions, but these might not be appropriate for all models. If a
function is not in the *skill set* of the module, then the limitation
should be noted in the documentation of `skill_set_XH.Xname` with
information in the list.

### Optional Outputs

For malaria models, built in functions compute prevalence by various
diagnostics.

- `F_pfpr_by_lm.Xname` :: output predicted prevalence by light
  microscopy

- `F_pfpr_by_rdt.Xname` :: output predicted prevalence by rapid
  diagnostic test

- `F_pfpr_by_pcr.Xname` :: output predicted prevalence by PCR

### Steady States

Methods are defined to compute various steady states under static
parameter values.

- `steady_state_X.Xname` :: pass the FoI and H and computes steady
  states for the **X** component, if appropriate.

- `steady_state_XH.Xname` :: pass the FoI and compute states for the
  **XH** component, if appropriate.

- `steady_state_H.Xname` :: compute steady states for the **H**
  component, if appropriate.

### Visualization

Functions have been developed to plot the standard terms, but each
module can define its own method for plotting:

- `xds_plot_XH.Xname` is a wrapper that calls `xds_lines_X`

- `xds_lines_XH.Xname` defines a default method for plotting orbits
