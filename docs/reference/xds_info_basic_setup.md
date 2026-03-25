# **`ramp.xds`**: Basic Setup

**Basic Setup** for **`ramp.xds`** uses
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
or one of its variants.

**Basic Setup** uses one of the following functions to build an
**`xds`** model:

- [xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
  sets up a full model with:

  - an **XH** Component

  - an **MY** Component

  - an **L** Component

- [xds_setup_human](https://dd-harp.github.io/ramp.xds/reference/xds_setup_human.md)
  sets up models to study human epidemiology, given exposure:

  - an **XH** component

  - a trivial **MY** component is configured for infectious biting in
    patches

  - the time spent matrix is used

  - the **L** component is not used

- [xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md)
  sets up models to study human epidemiology, given exposure:

  - an **XH** component with one population stratum

  - a *trace function* to output the daily EIR

  - a function configures relative biting rates by age for cohort
    dynamics

  - the **MY** component is not used

  - the **L** component is not used

- [xds_setup_mosy](https://dd-harp.github.io/ramp.xds/reference/xds_setup_mosy.md)
  sets up models for mosquito ecology:

  - the **L** component

  - the **M** component (*eg*
    [basicM](https://dd-harp.github.io/ramp.xds/reference/basicM.md))

  - a trivial **H** module (for blood feeding)

  - the **X** and **Y** components are not used

- [xds_setup_aquatic](https://dd-harp.github.io/ramp.xds/reference/xds_setup_aquatic.md)
  sets up models for aquatic mosquito ecology:

  - the **L** component

  - a trivial **MY** component is configured for egg laying

  - the **XH** component is not used

## Modularity

**`ramp.xds`** is designed for modular computation. The mathematical
framework has five core biological processes organized into three chunks
called **dynamical components** (see
[xds_info_dynamical_components](https://dd-harp.github.io/ramp.xds/reference/xds_info_dynamical_components.md)).
The modular design makes it possible to build models that fully define
all components (using
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)),
or to develop systems of equations to isolate and study some part of a
system.

To implement *plug-and-play* modularity, a trivial module was developed
for each component: a *trace function* in the trivial *upstream*
component is configured to pass known inputs (see
[xds_info_trivial_forcing](https://dd-harp.github.io/ramp.xds/reference/xds_info_trivial_forcing.md)).

## The Frame

[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
sets up a model that has all three components. Functions were developed
to setup and solve systems that making another component completely
unnecessary.

All `xds_setup_*` functions define `xds_obj$frame` and the functions
used to solve and parse differential equations use the `S3` system, with
methods that dispatch on `class(xds_obj$frame)`.

- [xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
  (`frame="full"`) sets up all three components.

- [xds_setup_mosy](https://dd-harp.github.io/ramp.xds/reference/xds_setup_mosy.md)
  (`frame="mosy"`) sets up models for mosquito ecology:

  - the model includes an **L** component

  - the model has an **M** component but no **Y** component (*eg*
    [basicM](https://dd-harp.github.io/ramp.xds/reference/basicM.md))

  - a trivial **H** module can be configured

  - the **X** component is not used

- [xds_setup_aquatic](https://dd-harp.github.io/ramp.xds/reference/xds_setup_aquatic.md)
  (`frame="aquatic"`) sets up models aquatic mosquito ecology:

  - a trivial **MY** component is configured for egg laying

  - the **XH** component is not used

- [xds_setup_human](https://dd-harp.github.io/ramp.xds/reference/xds_setup_human.md)
  (`frame="human"`) sets up models to study epidemiology

  - a trivial **MY** component is configured for infectious biting in
    patches

  - the **L** component is not used

- [xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md)
  (`frame="eir"`) sets up models to study epidemiology

  - a trace function computes the EIR, denoted \\F_E(t)\\

  - the **MY** component is not used

  - the **XH** component is not used

Each one of the basic setup functions assigns a different value to
`class(xds_obj$frame),` which dispatches the `S3` methods that solve and
parse differential equations:

- [xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
  sets `class(frame) = "full"`

- [xds_setup_human](https://dd-harp.github.io/ramp.xds/reference/xds_setup_human.md)
  sets `class(frame) = "human"`

- [xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md)
  sets `class(frame) = "eir"`

- [xds_setup_mosy](https://dd-harp.github.io/ramp.xds/reference/xds_setup_mosy.md)
  sets `class(frame) = "mosy"`

- [xds_setup_aquatic](https://dd-harp.github.io/ramp.xds/reference/xds_setup_aquatic.md)
  sets `class(frame) = "aquatic"`

## See also

[xds_info_dynamical_components](https://dd-harp.github.io/ramp.xds/reference/xds_info_dynamical_components.md),
[xds_info_trivial_forcing](https://dd-harp.github.io/ramp.xds/reference/xds_info_trivial_forcing.md)
and
[xds_object](https://dd-harp.github.io/ramp.xds/reference/xds_object.md)

[xds_info_dynamical_components](https://dd-harp.github.io/ramp.xds/reference/xds_info_dynamical_components.md),
[xds_info_trivial_forcing](https://dd-harp.github.io/ramp.xds/reference/xds_info_trivial_forcing.md)
and
[xds_object](https://dd-harp.github.io/ramp.xds/reference/xds_object.md)
