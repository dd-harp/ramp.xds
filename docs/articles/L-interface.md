# The L Component

The **L** component models aquatic mosquito ecology across larval
habitats. This vignette describes its interfaces and the methods
implemented by an **L** module (Figure 1). Each **L** module uses the
dynamical term, `eta` or \\(\eta\\), the number of eggs laid in the
habitat each day

    eta <- xds_obj$terms$eta[[s]]

Each module defines a function `F_emerge` (or \\(F\_\alpha\\)) to
compute the number of emergent adults, \\(\alpha\\). Development from
eggs to adults is described by a dynamical system. The following
describes the structure of the **L** component.

------------------------------------------------------------------------

![Figure 1 - A diagram of the L Component in context. The L component is
linked to adult mosquito ecology through egg laying and emergence. A
function, F_eggs returns the number of eggs laid by adult mosquitoes in
each patch, denoted by G. The egg-distribution matrix U allocates eggs
among habitats, so each habitat gets \eta eggs per day. The L module
then computes its dynamics and the emergence rate \alpha in each
habitat. The emergence interface aggregates \alpha, using the habitat
membership matrix, into the patch-specific adult emergence rate
\Lambda.](L_component.png)

**Figure 1** - A diagram of the **L** Component in context. The **L**
component is linked to adult mosquito ecology through egg laying and
emergence. A function, `F_eggs` returns the number of eggs laid by adult
mosquitoes in each patch, denoted by \\(G\\). The egg-distribution
matrix \\(U\\) allocates eggs among habitats, so each habitat gets
\\(\eta\\) eggs per day. The **L** module then computes its dynamics and
the emergence rate \\(\alpha\\) in each habitat. The emergence interface
aggregates \\(\alpha\\), using the habitat membership matrix, into the
patch-specific adult emergence rate \\(\Lambda\\).

------------------------------------------------------------------------

## Module methods

Most methods dispatch on the class of `xds_obj$L_obj[[s]]`, where `s` is
the vector-species index. The setup generic instead dispatches on the
supplied module name; the skill-set generic accepts either a model or a
module name. Model-specific methods use `s` consistently. There is no
fixed number of methods that every module must implement: implement the
dynamics method that matches the model and the outputs and setup methods
needed by its interfaces.

### Dynamics

Implement one of the following:

- `dLdt.Lname` for a continuous-time model. It receives `t`, the
  dependent-variable vector `y`, `xds_obj`, and `s`, and returns
  derivatives in the same order as the model’s state variables.
- `Update_Lt.Lname` for a discrete-time model. It receives the same
  arguments and returns the updated state variables in state-variable
  order.

The functions are used by the continuous-time or discrete-time solver
pathways, respectively.

Derivative titles use “Compute the derivatives for `module` (**L**)”.
Begin descriptions “Compute the derivatives for the `module` module:”
and show the implemented equations immediately below. Inherit the module
NULL anchor’s state, input-term, parameter, and ODE or Dynamics
sections, plus relevant model-specific sections. Preserve existing
section names: basicL uses `Dynamical Term` and `ODE`; stages uses
`Egg Laying`, `Mean Crowding`, and `Dynamics`. Put dispatch on
`xds_obj$L_obj[[s]]` in the generic note.

Return “a \[numeric\] vector of derivatives in the module’s
state-variable order”, with the actual length. Document block order and
the egg-input storage path in `@note`. basicL returns `L` in habitat
order, with length `nHabitats`; stages returns `E`, `L1`, `L2`, `L3`,
`L4`, and `P` blocks, each in habitat order, with total length
`6 * nHabitats`. The trivialL method retains its empty-vector
description and return value.

Update titles use “Update states for `module` (**L**)”. Begin
descriptions “Update the state variables for the `module` module:” and
show the updating equations in `\deqn{}` immediately below. Inherit the
same NULL-anchor sections as the derivative method, replacing `ODE` with
`DTS`. For basicL, retain `State Variables`, `Dynamical Term`,
`Parameters`, `Emergence`, and `Regulation` as well. Document dispatch
in the generic note, and habitat ordering and egg-input storage in the
method note. Return “a \[numeric\] vector of updated states in the
module’s state-variable order, with length `nHabitats`”. The trivialL
method remains an empty-vector no-op; stages has no discrete-time update
method.

### State-variable extraction

The number of state values is generally based on `nHabitats`, not
`nPatches`. The `membership` vector associates each habitat with a
patch.

State-variable getters primarily serve solver evaluations that need
variables by name; users rarely call them directly. Follow the shared
[getter
conventions](https://dd-harp.github.io/ramp.xds/articles/XH-interface.html#state-variable-getters):
use “Get the state variables for `module` (**L**)”, describe extraction
from `y` for model solving, put index storage in `@note`, and return a
named list of state variables and module-specific derived quantities.
Keep this separate from parsing solver-output matrices.

### Aquatic Bionomics

Two methods update bionomic parameter values before the dynamics are
computed.

- `LBionomics.Lname` – evaluates the baseline bionomic parameter
  functions for the current time and resets the effect sizes to 1.

For every parameter `x` configured as a port, `LBionomics` stores the
baseline in `x_t` and resets `es_x` to 1. `LEffectSizes` then sets
`x = x_t * es_x`. Parameters without ports are stored directly as `x`
and are not modified by effect sizes. In particular, `stages` has no
bionomic parameter ports: its constructor and parameter changer store
direct values, and its bionomics and effect-size methods leave them
unchanged.

- `LEffectSizes.Lname` – applies vector control effect sizes to the
  bionomic parameters set by `LBionomics`.

Both methods receive `t`, `y`, `xds_obj`, and `s`, and return the
updated `xds_obj`.

Retain the titles “Mosquito bionomics for `module` (**L**)” and “Apply
effect sizes for `module` (**L**)”. Inherit the module NULL anchor’s
`Parameters` section and link the two methods reciprocally and to the
module. For basicL, distinguish baseline evaluation and effect-size
resets from realization of `psi`, `phi`, `xi`, and `theta`; put the
stored fields and per-habitat assignments in `@note`. For stages, retain
the descriptions that both methods leave directly stored parameters
unchanged. Document dispatch on `xds_obj$L_obj[[s]]` in the generic
notes.

### Egg input and emergence output

The egg-laying interface computes patch-level adult egg production
\\(G\\) using `F_eggs`, then distributes eggs to aquatic habitats. If
\\(O\\) is the habitat-by-patch egg-laying matrix, \\(Q\\) is the
availability of modeled habitats in each patch, and
\\(Q\_{\mathrm{all}}\\) is total egg-laying availability, the
habitat-level input is

\\\[\eta = O\left(G \circ \frac{Q}{Q\_{\mathrm{all}}}\right),\\\]

where \\(\circ\\) denotes element-wise multiplication. Thus, \\(\eta\\)
has one value per habitat and is supplied to the L dynamics.

`F_emerge.Lname` returns the adult emergence quantity \\(\alpha\\), with
one value per habitat. It is a rate for a continuous-time model and a
number for a discrete-time model. The emergence interface aggregates
habitat emergence by patch using the habitat membership matrix \\(N\\):

\\\[\Lambda = N\alpha.\\\]

Use “Compute adult mosquito emergence for `module` (**L**)” for method
titles; the generic omits “for `module`”. Use the short description
“Return adult female mosquito emergence per habitat.” Methods inherit
`@inheritSection module Emergence` from their module NULL anchor, where
the formula and biological details belong. Return “a \[numeric\] vector
of length `nHabitats`” and link to the module and `Emergence`. Document
dispatch on `xds_obj$L_obj[[s]]` in the generic note. Flag formula
discrepancies separately: `F_emerge.stages` currently returns
`psi_P*P/2`, while its anchor gives `psi_P*P`.

### Dynamical parameter accessors

`get_psi`, `get_phi`, `get_xi`, and `get_theta` return the stored
realized maturation rate, density-independent mortality rate, delayed
maturation parameter, and slope of density-dependent mortality,
respectively, per habitat. They do not evaluate ports or apply effect
sizes. Refresh port-backed values through bionomics and effect-size
application before reading them. Currently, only `basicL` implements
these accessor methods.

Retain the titles “Get the parameter for `module` (**L**)”, using the
parameter’s full name; generic titles omit “for `module`”. Use short
stored-realized-value descriptions, inherit the module NULL anchor’s
`Parameters` section, and return “a \[numeric\] vector of length
`nHabitats`”. Put dispatch on `xds_obj$L_obj[[s]]` and the absence of
port evaluation or effect-size application in the generic note. Link
methods to their module and corresponding `change_psi`, `change_phi`,
`change_xi`, or `change_theta`.

### Model object and parameters

`make_L_obj_Lname` creates the module-specific **L** object, and
`setup_L_obj.Lname` attaches it to the full model at
`xds_obj$L_obj[[s]]`. The setup method also receives the
habitat-to-patch `membership` vector and initializes shared habitat
interfaces.

- `get_L_pars.Lname` returns the module’s parameters as a named list.
- `change_L_pars.Lname` changes supported parameter values using the
  named list `options`.

The shared `change_psi`, `change_phi`, `change_xi`, and `change_theta`
helpers change individual static `basicL` parameters. Each accepts a
numeric scalar, repeated across habitats, or values of length
`nHabitats`. Invalid types or lengths cause an error. For parameter `x`,
the helper sets `xds_obj$L_obj[[s]]$x`; when `x_obj` is present, it also
sets `x_t` and `x_obj$x`. It leaves the port class and `es_x` unchanged
and does not evaluate ports or apply effect sizes. It returns the
updated model object. These ordinary functions do not dispatch on the
module or port class; they are not a way to reconfigure a dynamic port.

Retain the “Change the parameter (**L**)” titles, using the full
parameter name. Describe the static `basicL` scope, inherit its
`Parameters` section, document scalar/per-habitat inputs, and put
storage and update mechanics in `@note`. Keep reciprocal links to the
corresponding parameter getters.

### Initial values

`make_L_inits_Lname` returns a named list of default initial values for
all state variables. `setup_L_inits.Lname` stores the values at
`xds_obj$L_obj[[s]]$inits`; `get_L_inits` and `change_L_inits.Lname`
inspect and update them.

`get_L_inits` is an ordinary function, not an S3 generic. It returns the
selected species’ stored `inits` field directly: a named list of initial
values, an empty list for `trivialL` after setup, or `NULL` if absent.
Keep storage in a documentation note and link to setup/change functions;
follow the shared getter wording in the [documentation
conventions](https://dd-harp.github.io/ramp.xds/articles/XH-interface.html#initial-value-constructors).

Title the constructor “Make initial values for `module` (**L**)” and
describe it as “Create a named list of initial values for the `module`
module. Values in `options` override the defaults.” Its return
description is “a named \[list\] of initial values”. Preserve
model-specific explanations. Document numeric scalar expansion to
`nHabitats` and the required length for non-scalar initial values. The
constructor, setup method, and change method each link to the other two
in `@seealso`, following the shared [documentation
conventions](https://dd-harp.github.io/ramp.xds/articles/XH-interface.html#initial-value-constructors).

Use “Set up initial values for `module` (**L**)” as the setup-method
title. Describe calling the initial-value constructor and `options`
overrides. Put storage at `xds_obj$L_obj[[s]]$inits` in `@note`, and
return “an **`xds`** model object”. Trivial methods describe their
actual behavior instead of claiming to call a constructor.

Title initial-value changers “Change initial values for `module`
(**L**)”. Use “Change initial values named in `options` for the `module`
module, leaving other values unchanged.” Preserve supported-value
explanations, put storage explanations in `@note`, and return “an
**`xds`** model object”. Keep reciprocal constructor/setup links, and
report implementation discrepancies separately during documentation
alignment.

### Variable indices

`setup_L_ix.Lname` assigns indices to the module’s state variables;
`get_L_ix` retrieves the stored indices.

`get_L_ix(xds_obj, s = 1)` is an ordinary function, not an S3 generic.
It returns the module’s stored `ix` field directly: a named list of
variable indices, an empty list if stored, or `NULL` if absent. Document
storage in `@note`, link to `setup_L_ix`, and follow the shared
[index-getter
conventions](https://dd-harp.github.io/ramp.xds/articles/XH-interface.html#variable-index-setup).

Title indexing methods “Set up variable indices for `module` (**L**)”.
Describe consecutive allocation after `xds_obj$max_ix`, list block order
and lengths, and return “an **`xds`** model object”. Put storage at
`xds_obj$L_obj[[s]]$ix` and the updated maximum in `@note`. Follow the
shared [indexing
conventions](https://dd-harp.github.io/ramp.xds/articles/XH-interface.html#variable-index-setup);
trivial methods describe their actual no-op behavior.

### Parsing outputs

`get_L_orbits(xds_obj, s = 1)` retrieves saved, parsed orbits
(trajectories) after solving for **L** for the selected species. It
returns a list of module-specific outputs with `time` added from
`xds_obj$outputs$time`. It reads `xds_obj$outputs$orbits$L[[s]]`; it
does not parse outputs or modify the model. Use the title “Get orbits
(**L**)” and the shared [orbit-getter
conventions](https://dd-harp.github.io/ramp.xds/articles/XH-interface.html#output-parsers).

- `parse_L_orbits.Lname` extracts named states and any model-specific
  derived quantities from the solver output matrix.

Follow the shared [output-parser
conventions](https://dd-harp.github.io/ramp.xds/articles/XH-interface.html#output-parsers):
use “Parse outputs for `module` (**L**)”, describe extraction from
`outputs` into a named list, and use “a \[list\]” for returns. Put index
storage at `xds_obj$L_obj[[s]]$ix` in a method note, and describe any
derived quantities. Parsers process trajectories after solving and
return results without modifying the model object; the output-processing
pipeline stores them at `xds_obj$outputs$orbits$L[[s]]`.

### Checks and skill set

`check_L.Lname` provides a hook for module-specific consistency checks
before solving. Existing methods currently return the model unchanged
and perform no checks.

`get_skillset_L.Lname` returns the module’s capabilities and
compatibilities as a list, which setup stores on the module object. A
module without `Update_Lt.Lname` must include `dts = FALSE`.

Non-trivial modules report `psi_is_port`, `phi_is_port`, `xi_is_port`,
and `theta_is_port`. Each flag is `TRUE` for a parameter implemented as
a port and `FALSE` for a direct or absent parameter. All four are `TRUE`
for `basicL` and `FALSE` for `stages`.

## The `basicL` example

The `basicL` module tracks larval density \\(L\\) in each aquatic
habitat. It uses the habitat-level egg input \\(\eta\\), maturation rate
\\(\psi\\), crowding-dependent maturation parameter \\(\xi\\),
density-independent mortality rate \\(\phi\\), and density-dependent
mortality parameter \\(\theta\\). Its continuous-time equation is

\\\[\frac{dL}{dt} = \eta - \left(\psi e^{-\xi L} + \phi + \theta
L\right)L.\\\]

The corresponding habitat-level adult emergence rate is

\\\[\alpha = \psi e^{-\xi L}L.\\\]

Setting \\(\xi=0\\) recovers the model described by Smith *et al.*
(2013). `basicL` also implements `Update_Lt.basicL`. Its steady-state
method, `steady_state_L.basicL`, delegates to `steady_state_L_basicL`,
which selects the differential-equation or discrete-time calculation.

## Solving and visualization

`steady_state_L.Lname` is an optional method for computing a model’s
steady state from habitat-level egg input \\(\eta\\). `get_L_orbits`
retrieves the parsed L output from a solved **`xds`** object. The shared
`xds_plot_L` and `xds_lines_L` functions plot aquatic mosquito
population density; modules do not need to implement L-specific plotting
generics.

Steady-state method titles use “Compute steady states for `module`
(**L**)”, apart from explicitly retained titles. Descriptions identify
the daily egg input `eta` and supported equation system; return
documentation names the states in the returned list. The generic
dispatches on `xds_obj$L_obj[[s]]`. For basicL, the second-stage generic
dispatches on `xds_obj$xds` to select `xde` or `dts`; both return a list
containing `L`. The stages method implements the differential-equation
calculation and returns `E`, `L1`, `L2`, `L3`, `L4`, and `P`. These
functions return values without storing them on the model object.

For details on a specific model, see its module documentation and the
relevant literature. The `basicL` equations are based on Smith *et al.*
(2013), listed in the references below.

## Documentation conventions

Follow the shared [function
order](https://dd-harp.github.io/ramp.xds/articles/XH-interface.html#function-order)
in module and component files and in guide implementation sections. Keep
functions with their documentation and preserve file-level review/TODO
notes at the top.

Follow the shared [documentation
conventions](https://dd-harp.github.io/ramp.xds/articles/XH-interface.html#documentation-conventions)
for titles, equations, descriptions, notes, return values, and
punctuation. Use **L** as the component tag. Bionomics and effect-size
method titles are “Mosquito bionomics for `module` (**L**)” and “Apply
effect sizes for `module` (**L**)”.

Document derivative equations and discrete-time updating equations
separately, matching the implemented formulas and returned state
ordering. State whether emergence is a continuous-time rate or a
discrete-time number, and distinguish habitat-level quantities from
patch-level quantities.

Constructors use “Make a module object for `module` (**L**)” as their
title. Describe parameter initialization and `options` overrides, and
identify the parameter getter and changer. The constructor,
`get_L_pars.Lname`, and `change_L_pars.Lname` each link to the other two
in `@seealso`. Document any differences in the parameters those
functions support.

Parameter-changer titles use “Change parameters for `module` (**L**)”.
Start the description with “Change parameter values named in `options`
for the `module` module, leaving other values unchanged.” Describe which
baseline fields are updated, whether bionomics and effect sizes are
refreshed immediately, and when changes take effect. After parameter or
port changes, call `LBionomics(0, y, xds_obj, s)` using the flattened
initial state `y`; do not call `LEffectSizes` from the changer. Return
an **`xds`** model object.

### Setup documentation

Use the title “Set up the module object for `module` (**L**)” and start
the description with “Set up a/an `module` module object for the **L**
component and the relevant vector species.”

List the constructor using `nHabitats` and `options`, the skill set, and
initial-value setup in implementation order. Describe the
habitat-interface step as “Initialize habitat search weights using
`[setup_habitat_search_weights.setup]`.” End with “Return the updated
**`xds`** model object.”

In roxygen, the bracketed helper name is a link; qualify it with
`ramp.xds::` in `ramp.library`. Keep object storage, class assignments,
and dispatch mechanics in `@note`. Setup methods return an **`xds`**
model object, not just the component list.

See [Developing L
Modules](https://dd-harp.github.io/ramp.library/articles/L-How_To_Add_Model.html)
for an adaptable template.

## References

- Smith DL, Perkins TA, Tusting LS, Scott TW, Lindsay SW (2013) Mosquito
  Population Regulation and Larval Source Management in Heterogeneous
  Environments. *PLOS ONE* 8(8): e71247.
  [doi:10.1371/journal.pone.0071247](https://doi.org/10.1371/journal.pone.0071247).
