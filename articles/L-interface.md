# The L Component

The **L** component was designed to model adult mosquito ecology and
infection dynamics.

This vignette takes a deep dive into the design and structure of the
**L** component. It is useful for anyone who wants to learn more about
how the code works.

## Required Functions

Each **L** module includes XX required functions and some optional ones.

Some of these required functions are `S3` class functions in
[adult-L-interface.R](https://github.com/dd-harp/ramp.xds/blob/main/R/adult-L-interface.R).
Others are defined for each module.

One good example is the `SI` module, posted in the `ramp.xds` github
repository
[human-L-SI.R](https://github.com/dd-harp/ramp.xds/blob/main/R/human-L-SIS.R).

The required functions deal with various tasks for model building and
solving: constructing the **L** model object; the dynamics; the
parameters; the variables and their initial values; computing terms and
standard outputs; and consistency checks. Optional outputs include other
metrics; functions to compute steady states; and module specific
functions to visualize the outputs.

Each module is defined by a string, generically called `Lname,` that
identifies the module: *e.g.* `SI.`

### Dynamics

1.  The dynamics are defined by at least one of the following is
    required, depending on whether the model family is a system of
    differential equations or a discrete time system:

    - `dLdt.Lname` :: differential equations are defined by a function
      that computes the derivatives. In `ramp.xds` these are encoded in
      a function called `dLdt.` The function is set up to be solved by
      [`deSolve::ode`](https://rdrr.io/pkg/deSolve/man/ode.html) or
      `deSolve::dede.`

    - `Update_Lt.Lname` :: discrete time systems are defined by the
      function that updates the state variables in one time step. In
      `ramp.xds` these are encoded in a function called `Update_Lt` that
      computes and returns the **state variables.** The forms mimic the
      ones used for differential equations.

## L Model Object

Each module has a pair of functions that set up a structured list called
the **L** model object. The object is a list that is assigned to a
`class` that dispatches the `S3` functions described below. It is a
compound list, where some of the sub-lists are assigned their own
`class` that dispatch other `S3` functions.

2.  `make_L_obj_Lname` :: returns a structured list called an **L**
    model object:

    - bionomic parameter values or bionomic parameter objects. In most
      models, the value of baseline bionomic parameters are functions of
      time, or exogenous variables that vary with time.

    - `class(L_obj)` = `Lname`

    - the indices for the model variables are stored as `L_obj$ix`

    - the initial values are stored as `L_obj$inits`

    - anything else that is needed can be configured here

3.  `setup_L_obj.Lname` is a wrapper that calls `make_L_obj_Lname` and
    (for the \\i^{th}\\ species) attaches the object as
    `xds_obj$L_obj[[i]]`

### Parameters

4.  `change_L_pars.Lname` changes the values of some parameters. It is
    designed to be used after setup. New parameter values are passed by
    name in a list called `options.`

5.  `get_L_pars.Lname` is a utility to inspect the values of the
    parameters.

### Variables

Since the **L** component is one of three, a function sets up the
indices for all the variables in a model.

Two other functions use those indices: one pulls the variables from the
state variable vector \\y\\; the other one pulls the variables by name
from an output matrix returned by `xds_solve.`

After pulling, both functions return the variables in by name in a list
to make it easy to inspect or use.

6.  `setup_L_ix.Lname` - is the function that assigns an index to each
    variable in the model, and stores it as `xds_obj$L_obj[[i]]$ix.` The
    indices are returned as a named list.

7.  `get_L_vars.Lname` - retrieves the value of variables from the state
    variables vector \\y\\ at a point in time and returns the values by
    name in a list; the function gets called by `dLdt` and by
    `change_L_inits` and it can be useful in other contexts.

8.  `parse_L_orbits.Lname` - this function is like `get_L_vars` but it
    parses the matrix of outputs returned by `xds_solve.`

### Initial Values

A set of functions is sets up or changes the initial values for the
state variables.

9.  `make_L_inits_Lname` - each model must include a function that makes
    a set of initial values as a named list. This function does not
    belong to any `S3` class, so it can take any form. The function
    should supply default initial values for all the variables. These
    can be overwritten by passing new initial values in `options.`

10. `setup_L_inits.Lname` - is a wrapper, that gets called by
    `xds_setup` and that calls `make_L_inits_Lname.` The setup `options`
    are passed to overwrite default values. The initial values are
    stored as `L_obj$inits.`

11. `change_L_inits.Lname` - a utility to change the initial values.

### Dynamical Terms

These functions compute dynamical terms – the outputs passed to an
interface.

12. `F_emerge.Lname` -

### Standard Outputs

Each module must output a few key quantities:

15. `F_larvae.Lname` - compute the density of all larvae

16. `F_capacity.Lname` - compute the *carrying capacity* for each
    habitat

### Consistency Checks

Some modules in **`ramp.xds`** or **`ramp.library`** have been included
for various reasons. Not all of those models are capable of being
extended. To help users avoid using models in ways that are not
appropriate, we developed two function classes:

18. `skill_set_L.Lname` :: describes model capabilities and limitations

19. `check_L.Lname` :: at the end of `xds_setup` and at the beginning of
    `xds_solve,` this function gets run to ensure that some quantities
    have been properly updated, and to see if anything has been added to
    a model that is not in its skill set.

## Optional Functions

The **L** interface also sets up `S3` classes for some optional
functions, but these might not be appropriate for all models. If a
function is not in the *skill set* of the module, then the limitation
should be noted in the documentation of `skill_set_L.Lname` with
information in the list.

### Steady States

Methods are defined to compute various steady states under static
parameter values.

- `steady_state_L.Lname` :: pass the egg laying rate of and compute
  steady states for the **L** component.

### Visualization

Functions have been developed to plot the standard terms, but each
module can define its own method for plotting:

- `xds_plot_L.Lname` is a wrapper that calls `xds_lines_X`

- `xds_lines_L.Lname` defines a default method for plotting orbits
