# Ports & Junctions

The design of **`ramp.xds`** facilitates modeling malaria as a changing
baseline that has been modified by control. Most of the functionality is
found in the satellite packages, but **`ramp.xds`** sets this up by
adding *ports* and *junctions.*

- a ***port*** in **`ramp.xds`** or one of the satellite packages is an
  object that handles a term or parameter, such as temperature, other
  blood hosts, or search weights. Each *port* is defined by:

  - the port object

  - the value for the port and a function that returns it

  - advanced setup functions

  - an updating function

- a ***junction*** in **`ramp.xds`** is a function that implements
  several *ports.*

------------------------------------------------------------------------

![Ports — The design of a port and junction includes default setup of an
object during xds_setup on the relevant object with parameter values set
to have no effect; an advanced setup function setup_port that replaces
the old default parameter, evaluates the port function and checks the
returned value, and then rebuilds and replaces the port object; and a
function update_port, called by a Junction, that evalutes the function
and stores the updated value.](Ports.drawio.png)

**Ports** — The design of a port and junction includes default setup of
an object during `xds_setup` on the relevant object with parameter
values set to have no effect; an advanced setup function `setup_port`
that replaces the old default parameter, evaluates the port function and
checks the returned value, and then rebuilds and replaces the port
object; and a function `update_port,` called by a `Junction`, that
evalutes the function and stores the updated value.

------------------------------------------------------------------------

## Junctions

The junctions in **`ramp.xds`** get called in a specific order to
minimize *conflicts* that could arise. The junctions are all functions
that get implemented in `xds_compute_terms.` They are:

- **Forcing** — \\\emptyset \in\\ `ramp.xds`

  - **Weather** \\\in\\ `ramp.forcing`

    - `update_temperature`

    - `update_rainfall`

    - `update_humidity`

  - **Weather** \\\in\\ \`ramp.forcing

- **Resources**

  - `update_blood_search_weights`

  - `update_other_blood`

- **Health** - functions to setup mass treatment, mass vaccination, and
  more

- **Vector Control** - functions to *distribute* vector control
  commodities.

## Forcing

`Forcing` :: **`ramp.xds`** sets up an empty *junction* handle exogenous
forcing by weather and hydrology. Models to implement forcing are in
**`ramp.forcing`**

## Resources

## Blood Feeding

Several ports handle blood feeding:

- search weights

- other blood hosts

- traps

## Egg Laying

- search weights

- bad habitats

- ovitraps

## Forcing

## Mosquito Bionomics

Depending on the module, many of the bionomic parameters are set up as
ports.

- Some variables are set up in the first phase of `VectorControl`

- The values of

- The values of parameters or functions stored on the object set the
  *baseline* values, which are set in a call to `MBionomics` or
  `LBionomics`.

- The

`VectorControl` and `VectorControlEffectSizes`:: **`ramp.xds`** sets up
an empty *junction* to handle various aspects of vector control. Models
to implement vector control are in **`ramp.control`**

## Health

`Health`:: **`ramp.xds`** sets up an empty *junctions* to handle various
aspects of health systems interventions.

## Importation

- time away

- travel eir

- visitors

- vis_kappa
