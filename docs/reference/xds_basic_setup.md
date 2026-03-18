# Basic Setup for `ramp.xds`

**Basic Setup** for **`ramp.xds`** uses
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
or one of its variants.

## Modularity

**`ramp.xds`** is designed for modular computation. The mathematical
framework has five core biological processes organized into three chunks
called **dynamical components** (see
[dynamical_components](https://dd-harp.github.io/ramp.xds/reference/dynamical_components.md)).
The modular design makes it possible to build models that fully define
all components (using
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)),
or to develop systems of equations to isolate and study some part of a
system.

To implement *plug-and-play* modularity, a trivial module was developed
for each component: a *trace function* in the trivial *upstream*
component is configured to pass known inputs (see
[trivial_forcing](https://dd-harp.github.io/ramp.xds/reference/trivial_forcing.md)).

## The Frame

[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
sets up a model that has all three components. Functions were developed
to setup and solve systems that making at least one of the other
components completely unnecessary.

All `xds_setup_*` functions define `xds_obj$frame` and the functions
used to solve and parse differential equations use the `S3` system, with
methods that dispatch on `class(xds_obj$frame).`

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

## See also

[dynamical_components](https://dd-harp.github.io/ramp.xds/reference/dynamical_components.md),
[trivial_forcing](https://dd-harp.github.io/ramp.xds/reference/trivial_forcing.md)
and
[xds_object](https://dd-harp.github.io/ramp.xds/reference/xds_object.md)
