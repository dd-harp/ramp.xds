# Getting Started

**`ramp.xds`** was developed to reduce the costs of building, solving,
analyzing, and using models describing the epidemiology, transmission
dynamics and control of malaria and other mosquito-transmitted
pathogens.

------------------------------------------------------------------------

This vignette gives a basic introduction to **`ramp.xds`** and a quick
demonstration.

- How to install **`ramp.xds`**

- A demonstration:

  - Set up a model

  - Solve it

Next: [Basic
Setup](https://dd-harp.github.io/ramp.xds/articles/BasicSetup.md) or
[Working with
Models](https://dd-harp.github.io/ramp.xds/articles/Working.md)

Additional documentation is available in the [**SimBA**
Project](https://faculty.washington.edu/smitdave/simba/index.html)
website.

------------------------------------------------------------------------

## Installation

The R package is available from github. To download the R package, run
these commands:

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`devtools`](https://devtools.r-lib.org/)`)`\
`devtools``::`[`install_github`](https://devtools.r-lib.org/reference/install-deprecated.html)`(``"dd-harp/ramp.xds"``)`

Next, load **`ramp.xds`**:

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`ramp.xds`](https://dd-harp.github.io/ramp.xds/)`)`

Additional modules and functionality are available in related
R-packages, including:

- [**`ramp.func`**](https://dd-harp.github.io/ramp.func/),
- [**`ramp.library`**](https://dd-harp.github.io/ramp.library/),
- [**`ramp.control`**](https://dd-harp.github.io/ramp.control/),
- [**`ramp.forcing`**](https://dd-harp.github.io/ramp.forcing/),
- [**`ramp.demog`**](https://dd-harp.github.io/ramp.demog/),
- [**`ramp.qa`**](https://dd-harp.github.io/ramp.qa/), and
- [**`ramp.work`**](https://dd-harp.github.io/ramp.work/).

For more information, see the [**SimBA**
Project](https://faculty.washington.edu/smitdave/simba/index.html)
website.

## Setup

**`ramp.xds`** makes it easy to build and use models for the
transmission dynamics and control of malaria or other mosquito-borne
pathogens. Model building in **`ramp.xds`** starts with a function
called `xds_setup().` All arguments in
[`xds_setup()`](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
are assigned default values, so calling
[`xds_setup()`](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
with no arguments returns a fully configured model with all the default
settings.

\
`model`` ``<-`` `[`xds_setup`](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)`(``)`

The object returned by
[`xds_setup()`](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
is called an **xds model object** or an **xds object**: it fully defines
a model that can be solved by
[`xds_solve()`](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md).
In the code chuck, we called it `model`. If you type it, the function
`print.xds_obj` gives a basic report.

\
`model`

    ## 
    ## model_name - unnamed
    ## 
    ## Modules:
    ##   Xname  - SIS
    ##   MYname - SI
    ##   Lname  - trivial
    ## 
    ## Structure:
    ##   nPatches         = 1
    ##   nHabitats        = 1
    ##   nStrata          = 1
    ##   nHostSpecies     = 1
    ##   nVectorSpecies   = 1
    ## 
    ## frame - full
    ## xds   - ode

We can see from the returned value that `Xname` is `SIS`, which is the
name of the module for human / host infection and immune dynamics.

The **xds object** is a compound list. One way to inspect it is to ask
for its names.

\
[`names`](https://rdrr.io/r/base/names.html)`(``model``)`

    ##  [1] "membership"         "nPatches"           "nHabitats"         
    ##  [4] "nStrata"            "nVectorSpecies"     "nHostSpecies"      
    ##  [7] "model_name"         "xde"                "xds"               
    ## [10] "frame"              "forced_by"          "patches"           
    ## [13] "habitats"           "terms"              "blood_feeding_obj" 
    ## [16] "import_obj"         "transmission_obj"   "env_het_obj"       
    ## [19] "Xname"              "XH_obj"             "MYname"            
    ## [22] "MY_obj"             "Lname"              "L_obj"             
    ## [25] "variables"          "nOtherVariables"    "V_obj"             
    ## [28] "resources_obj"      "forcing_obj"        "health_obj"        
    ## [31] "vector_control_obj" "outputs"            "analysis"          
    ## [34] "max_ix"

The \\(19^{th}\\) item is `Xname`:

\
`model``$``Xname`

    ## [1] "SIS"

To inspect information, **`ramp.xds`** has functions with intuitive
names that show what information is stored there. `SIS` was set up with
default parameter values that can be viewed with the function
`get_XH_pars`:

\
[`get_XH_pars`](https://dd-harp.github.io/ramp.xds/reference/get_XH_pars.md)`(``model``)`

    ## $b
    ## [1] 0.55
    ## 
    ## $c
    ## [1] 0.15
    ## 
    ## $r
    ## [1] 0.005555556
    ## 
    ## $q_lm
    ## [1] 0.8
    ## 
    ## $q_rdt
    ## [1] 0.8
    ## 
    ## $q_pcr
    ## [1] 0.9

Functions like `get_XH_pars` are designed to help users work with **xds
model object**. In this case, the parameters are stored as
`model$XH_obj[[i]],` for the \\(i^{th}\\) species. Knowing the names,
the parameters can then be viewed (or changed). For example, this would
change `r` for the first species:

\
`model``$``XH_obj``[[``1``]``]``$``r`` ``<-`` ``1``/``150`

To make it easy to change parameters, we also wrote the `change_XH_pars`
function, which takes new values in a named list.

\
`model`` ``<-`` `[`change_XH_pars`](https://dd-harp.github.io/ramp.xds/reference/change_XH_pars.md)`(``model``, options``=`[`list`](https://rdrr.io/r/base/list.html)`(``r``=``1``/``100``)``, i``=``1``)`\
[`get_XH_pars`](https://dd-harp.github.io/ramp.xds/reference/get_XH_pars.md)`(``model``)``$``r`

    ## [1] 0.01

Similarly, initial values were assigned default values at setup. In
**`ramp.xds,`** human/host demography is treated as a distinct process,
so the initial values are:

\
[`get_XH_inits`](https://dd-harp.github.io/ramp.xds/reference/get_XH_inits.md)`(``model``)`

    ## $H
    ## [1] 1000
    ## 
    ## $I
    ## [1] 1

We already changed the value of parameter from its default after
`xds_setup` (above), but we could have set the parameter values when we
called `xds_setup`. The default parameters and initial values can be
overwritten at setup by passing a named list, as `options = list(...)`
with different values. Since `xds_setup` calls functions to set up
several model objects, each component has its own options name. For the
**XH** component, options are passed in `XHoptions`:

\
`Xo`` ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``r``=``1``/``100``, c``=``0.2``, b``=``0.6``, I``=``2``)`\
`model1`` ``<-`` `[`xds_setup`](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)`(``XHoptions ``=`` ``Xo``)`\
[`get_XH_pars`](https://dd-harp.github.io/ramp.xds/reference/get_XH_pars.md)`(``model1``)`

    ## $b
    ## [1] 0.6
    ## 
    ## $c
    ## [1] 0.2
    ## 
    ## $r
    ## [1] 0.01
    ## 
    ## $q_lm
    ## [1] 0.8
    ## 
    ## $q_rdt
    ## [1] 0.8
    ## 
    ## $q_pcr
    ## [1] 0.9

In the `SIS` model, we set \\(H_0,\\) and \\(I_0.\\) The software
computes \\(dH/dt\\) and \\(dI/dt\\) and sets \\(S=H-I.\\) By policy,
the software computes the total population size, \\(H,\\) dynamically
and treats it differently. This is because it plays an important role in
setting up the *Blood Feeding* interface, so it is not possible to set
\\(S.\\) Instead, \\(S\\) is computed after solving and parsing.

\
[`get_XH_inits`](https://dd-harp.github.io/ramp.xds/reference/get_XH_inits.md)`(``model1``)`

    ## $H
    ## [1] 1000
    ## 
    ## $I
    ## [1] 2

You can set new values of `H` using `change_XH_inits` or\
To change the human population density, use the `change_H` function.

\
`model1`` ``<-`` `[`change_H`](https://dd-harp.github.io/ramp.xds/reference/change_H.md)`(``1004``, ``model1``)`\
[`get_XH_inits`](https://dd-harp.github.io/ramp.xds/reference/get_XH_inits.md)`(``model1``)`

    ## $H
    ## [1] 1004
    ## 
    ## $I
    ## [1] 2

The module for adult mosquito ecology and infection dynamics is called
`MYname.` The default module is an SI model:

\
`model``$``MYname`

    ## [1] "SI"

The design pattern is followed. Parameters can be viewed with
`get_MY_pars`. Parameter values can be set by passing a named list to
`MYoptions` in
[`xds_setup()`](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md),
and changed after setup by passing new values in a named list to
`change_MY_pars`.

The name of the default module for aquatic mosquito ecology is called
`Lname.` The default module is the `trivial` module:

\
`model``$``Lname`

    ## [1] "trivial"

For the `trivial` module, arguments passed at the command line configure
functions that pass values to other dynamical components.

The design pattern is followed: parameters can be viewed with
`get_L_pars` and the method for setting parameter values is to pass a
named list of values to `Loptions` in `xds_setup().` After setup, the
`change_L_pars` works in the same way as the other `change_` functions.

## Solving

After setting up, `model` has an empty placeholder for outputs:

\
`model``$``outputs`` `

    ## list()

Calling `xds_solve(model)` returns solutions to the *initial value
problem(s):* `xds_solve` calls `deSolve,` and the values of the
dependent variables at a set of time points are attached as
`model$outputs.`

There are three ways to pass the argument `time` to `deSolve`: first,
`xds_solve` can set up a vector of time points `seq(0, Tmax, by = dt)`
that are configurable with the arguments `Tmax` and `dt;` second, if no
arguments are passed for time, then it uses the defaults `Tmax=365` and
`dt=1;` third, `deSolve` will use the optional argument `times` if it is
supplied. All four of the following do the same thing:

\
`model`` ``<-`` `[`xds_solve`](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)`(``model``, times``=``0``:``365``)`` `\
`model`` ``<-`` `[`xds_solve`](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)`(``model``, times``=``0``:``365``, Tmax``=``730``, dt``=``5``)`` `\
`model`` ``<-`` `[`xds_solve`](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)`(``model``, Tmax``=``365``, dt``=``1``)`` `\
`model`` ``<-`` `[`xds_solve`](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)`(``model``)`` `

Internally, these are handled by the function `make_times_xde` for
systems of differential equations, and `make_times_dts` for discrete
time systems.

Note `xds_solve` requires an **xds model object** and that it returns an
**xds model object.** In this case, the function passes `model` and the
return value replaces `model.` Before `xds_solve,` `model$outputs` was
an empty list. After solving, the outputs are stored as `model$outputs`:

\
[`names`](https://rdrr.io/r/base/names.html)`(``model``$``outputs``)`

    ## [1] "deout"  "time"   "last_y" "orbits"

The outputs from solving a system of equations include:

- `deout` – the raw (unparsed) outputs\
- `time` – the time points where dependent variables were computed
- `last_y` – the last state of the system\
- `orbits` – solutions with the values of the dependent variables and
  terms parsed by name, accessible separately for each dynamical
  component

Note that `time` is the set of time points where we output the values of
the dependent variables.

\
[`head`](https://rdrr.io/r/utils/head.html)`(``model``$``outputs``$``time``, ``5``)`

    ## [1] 0 1 2 3 4

\
[`tail`](https://rdrr.io/r/utils/head.html)`(``model``$``outputs``$``time``, ``3``)`

    ## [1] 363 364 365

When `xds_solve` is called, the **`xds` model object** it returns has
replaced any old values. If nothing has changed, then the outputs will
be identical.

On the other hand, if anything has changed, any old outputs will get
replaced. If the outputs need to be saved for future analyses, the user
will probably have to write a wrapper function that extracts and saves
it.

## Outputs

To make it easy to deal with the outputs, the dependent variables are
parsed and returned in named lists.
[`xds_solve()`](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)
also computes some standard *terms* that are likely to be of interest.

Functions like
[`get_EIR()`](https://dd-harp.github.io/ramp.xds/reference/get_EIR.md)
make it easy to examine or save a set of standard outputs without
delving into the details.

\
[`head`](https://rdrr.io/r/utils/head.html)`(`[`get_EIR`](https://dd-harp.github.io/ramp.xds/reference/get_EIR.md)`(``model``)``, ``3``)`

    ##              [,1]
    ## [1,] 0.0002850000
    ## [2,] 0.0002628915
    ## [3,] 0.0002439230

## Visualization

**`ramp.xds`** includes functions to plot standard outputs:

- `xds_plot_X` plots the density of infected individuals

- `xds_plot_M` plots the density of adult female mosquitoes in each
  patch

- `xds_plot_Y` plots the density of infected adult female mosquitoes in
  each patch

- `xds_plot_Z` plots the density of infectious adult female mosquitoes
  in each patch

- `xds_plot_PR` plots the true prevalence of infection in the human /
  host population

- `xds_plot_EIR` plots the EIR for each one of the human / host
  population strata

\
[`par`](https://rdrr.io/r/graphics/par.html)`(``mfrow ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``2``,``2``)``)`\
[`xds_plot_X`](https://dd-harp.github.io/ramp.xds/reference/xds_plot_X.md)`(``model``)`\
[`xds_plot_M`](https://dd-harp.github.io/ramp.xds/reference/xds_plot_M.md)`(``model``)`\
[`xds_plot_Y`](https://dd-harp.github.io/ramp.xds/reference/xds_plot_Y.md)`(``model``, add``=``T``)`\
[`xds_plot_Z`](https://dd-harp.github.io/ramp.xds/reference/xds_plot_Z.md)`(``model``, add``=``T``)`\
[`xds_plot_PR`](https://dd-harp.github.io/ramp.xds/reference/xds_plot_PR.md)`(``model``)`\
[`xds_plot_EIR`](https://dd-harp.github.io/ramp.xds/reference/xds_plot_EIR.md)`(``model``)`

![\*\*Figure 1:\*\* Plotting standard
outputs](GettingStarted_files/figure-html/unnamed-chunk-22-1.png)

**Figure 1:** Plotting standard outputs

## Trace

The software was designed to be fully modular, so that any module for
one dynamical component can be replaced with another. The models
included in **`ramp.xds`** illustrate its features and ensure that the
software is working as designed.

A basic requirement for full modularity is that every dynamical
component must include a *trivial* module. In the `trivial` module, no
variables are defined, but the dynamical terms needed by related
dynamical components are passed by a configurable ***composed time
series function*** with a generic form:

\
`mean``*`[`F_season`](https://dd-harp.github.io/ramp.xds/reference/F_season.md)`(``t``)``*`[`F_trend`](https://dd-harp.github.io/ramp.xds/reference/F_trend.md)`(``t``)``*`[`F_shock`](https://dd-harp.github.io/ramp.xds/reference/F_shock.md)`(``t``)`

It is easy to change the forcing function. A function library for
`ramp.xds` is found in
[`ramp.func`](https://github.com/dd-harp/ramp.func). For more
information about configuring trace functions for `ramp.xds`

## Next Steps

Two good next steps are the vignettes:

- [Basic
  Setup](https://dd-harp.github.io/ramp.xds/articles/BasicSetup.html).

- [Working with
  Models](https://dd-harp.github.io/ramp.xds/articles/Working.html).
