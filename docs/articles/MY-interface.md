# The MY Component

The **MY** component was designed to model adult mosquito ecology and
infection dynamics.

This vignette takes a deep dive into the design and structure of the
**MY** component. It is useful for anyone who wants to learn more about
how the code works.

## Overview

The **MY** component models adult mosquito ecology and infection
dynamics. We let **M** describe a state space describing mosquito
ecology, and **Y** an embedded state space describing parasite or
pathogen infection dynamics. Since **Y** is embedded within **M,** the
two processes are handled together.

The **Y** component interacts with **X** through blood feeding and
transmission:

- All **Y** components are developed around net-infectiousness, or
  \\\kappa,\\ the probability of becoming infected after blood feeding
  on a human.

- Each module must supply a function \\F_Z\\ that computes the expected
  number of infectious bites on humans. This is processed by the **XY**
  interface and transformed into the daily EIR for all the **XH**
  component strata.

The **M** component interacts with **L** through egg laying and
emergence.

- The **M** component is developed around the net emergence rate of
  adult mosquitoes, or \\\Lambda.\\

- Each **M** component must supply a function \\F_G\\ that computes the
  number of eggs laid by the adult mosquito population in a patch. The
  **ML**-interface determines how the eggs are apportioned among all the
  habitats in a patch.\
  A function `dMYdt` computes the derivatives for differential equation
  modules. `Update_MYt` updates the variables for discrete time systems.

Two important issues for **MY** components are adult mosquito
demography, including mortality and dispersal and the method for
handling mosquito bionomics as a baseline that is modified by control.

### Mosquito Demography

There are some built-in functions to compute a mosquito demographic
matrix, \\\Omega,\\ and a matrix that computes survival and dispersal
through the EIP: \\\Upsilon = e^{-\Omega \tau}\\

For most of these models: \\\Omega = -g - \sigma + (1-\mu) \sigma K\\

### Mosquito Bionomics

The core challenge for implementing mosquito bionomics was how to handle
time-varying parameters affected by two or more processes: a baseline
value affected by resource availability or weather; and a new value that
has been modified by the presence of vector control. This challenge
applies to all mosquitoes, whether adult or immature, so it is handled
separately:
[Bionomics](https://github.com/dd-harp/ramp.xds/Bionomics.html).

## Required Functions

Each **MY** module includes 21 required functions and some optional
ones.

Some of these required functions are `S3` class functions in
[adult-MY-interface.R](https://github.com/dd-harp/ramp.xds/blob/main/R/adult-MY-interface.R).
Others are defined for each module.

One good example is the `SI` module, posted in the `ramp.xds` github
repository
[adult-MY-SI.R](https://github.com/dd-harp/ramp.xds/blob/main/R/adult-MY-SI.R).

The required functions deal with various tasks for model building and
solving: constructing the **MY** model object; the dynamics; the
parameters; the variables and their initial values; computing terms and
standard outputs; and consistency checks. Optional outputs include other
metrics; functions to compute steady states; and module specific
functions to visualize the outputs.

Each module is defined by a string, generically called `MYname`, that
identifies the module: *e.g.* `SI`.

### Dynamics

At least one of the following is required, depending on whether the
model family is a system of differential equations or a discrete time
system:

    + `dMYdt.MYname` :: differential equations are defined by a function that computes the derivatives. In `ramp.xds` these are encoded in a function called `dMYdt`. The function is set up to be solved by `deSolve::ode` or `deSolve::dede`.

    + `Update_MYt.MYname` :: discrete time systems are defined by the function that updates the state variables in one time step. In `ramp.xds` these are encoded in a function called `Update_MYt` that computes and returns the **state variables.** The forms mimic the ones used for differential equations.

### Bionomics

Two functions update bionomic parameter values at each time step, called
in sequence before the dynamics are computed.

- `MBionomics.MYname` – computes the current bionomic parameter values
  by evaluating the appropriate forcing functions. It also resets all
  effect sizes to 1.

- `MEffectSizes.MYname` – applies vector control effect sizes to the
  bionomic parameters set by `MBionomics`.

## MY Model Object

Each module has a pair of functions that set up a structured list called
the **MY** model object. The object is a list that is assigned to a
`class` that dispatches the `S3` functions described below. It is a
compound list, where some of the sub-lists are assigned their own
`class` that dispatch other `S3` functions.

- `make_MY_obj_MYname` :: returns a structured list called an **MY**
  model object:

  - bionomic parameter values or bionomic parameter objects. In most
    models, the value of baseline bionomic parameters are functions of
    time, or exogenous variables that vary with time.

  - `class(MY_obj)` = `MYname`

  - the indices for the model variables are stored as `MY_obj$ix`

  - the initial values are stored as `MY_obj$inits`

  - anything else that is needed can be configured here

- `setup_MY_obj.MYname` is a wrapper that calls `make_MY_obj_MYname` and
  (for the \\i^{th}\\ species) attaches the object as
  `xds_obj$MY_obj[[i]]`

### Parameters

- `change_MY_pars.MYname` changes the values of some parameters. It is
  designed to be used after setup. New parameter values are passed by
  name in a list called `options`.

- `get_MY_pars.MYname` is a utility to inspect the values of the
  parameters.

### Variables

Since the **MY** component is one of three, a function sets up the
indices for all the variables in a model.

Two other functions use those indices: one pulls the variables from the
state variable vector \\y\\; the other one pulls the variables by name
from an output matrix returned by `xds_solve`.

After pulling, both functions return the variables by name in a list to
make it easy to inspect or use.

- `setup_MY_ix.MYname` - is the function that assigns an index to each
  variable in the model, and stores it as `xds_obj$MY_obj[[i]]$ix`. The
  indices are returned as a named list.

- `get_MY_vars.MYname` - retrieves the value of variables from the state
  variables vector \\y\\ at a point in time and returns the values by
  name in a list; the function gets called by `dMYdt` and by
  `change_MY_inits` and it can be useful in other contexts.

- `parse_MY_orbits.MYname` - this function is like `get_MY_vars` but it
  parses the matrix of outputs returned by `xds_solve`.

### Initial Values

A set of functions sets up or changes the initial values for the state
variables.

- `make_MY_inits_MYname` - each model must include a function that makes
  a set of initial values as a named list. This function does not belong
  to any `S3` class, so it can take any form. The function should supply
  default initial values for all the variables. These can be overwritten
  by passing new initial values in `options`.

- `setup_MY_inits.MYname` - is a wrapper, that gets called by
  `xds_setup` and that calls `make_MY_inits_MYname`. The setup `options`
  are passed to overwrite default values. The initial values are stored
  as `MY_obj$inits`.

- `change_MY_inits.MYname` - a utility to change the initial values.

### Dynamical Terms

These functions compute dynamical terms – the outputs passed to an
interface.

- `F_fqZ.MYname` - compute the daily blood feeding rate of the infective
  mosquito population for all the patches

- `F_fqM.MYname` - compute the daily blood feeding rate of the total
  mosquito population for all the patches

- `F_eggs.MYname` - compute the daily egg laying rate by the adult
  mosquito population for all the patches

### Standard Outputs

Each module must output a few key quantities:

- `F_prevalence.MYname` - compute prevalence

- `F_ni.MYname` - compute net infectiousness (NI) for each stratum. If
  \\F_X \rightarrow X\\ and \\F_H \rightarrow H\\, then \\F\_{ni}
  \rightarrow X/H.\\ The function gets called after solving, and the NI
  is attached as a term for inspection and visualization.

- `get_HTC.MYname` - compute the human transmitting capacity. This is
  used by functions in **`ramp.work`** that compute threshold
  conditions.

### Consistency Checks

Some modules in **`ramp.xds`** or **`ramp.library`** have been included
for various reasons. Not all of those models are capable of being
extended. To help users avoid using models in ways that are not
appropriate, we developed two function classes:

- `skill_set_MY.MYname` :: describes model capabilities and limitations

- `check_MY.MYname` :: at the end of `xds_setup` and at the beginning of
  `xds_solve,` this function gets run to ensure that some quantities
  have been properly updated, and to see if anything has been added to a
  model that is not in its skill set.

## Optional Functions

The **MY** interface also sets up `S3` classes for some optional
functions, but these might not be appropriate for all models. If a
function is not in the *skill set* of the module, then the limitation
should be noted in the documentation of `skill_set_MY.MYname` with
information in the list.

### Steady States

Methods are defined to compute various steady states under static
parameter values.

- `steady_state_M.MYname` :: pass the emergence rate of adult mosquitoes
  and compute steady states for the **M** component.

- `steady_state_MY.MYname` :: pass the emergence rate of adult
  mosquitoes and net infectiousness, and compute the steady states for
  the **MY** component.

- `steady_state_Y.MYname` :: pass adult mosquito population density and
  net infectiousness, and compute the steady states for the **Y**
  component.

### Visualization

Functions have been developed to plot the standard terms, but each
module can define its own method for plotting:

- `xds_plot_MY.MYname` is a wrapper that calls `xds_lines_MY`

- `xds_lines_MY.MYname` defines a default method for plotting orbits
