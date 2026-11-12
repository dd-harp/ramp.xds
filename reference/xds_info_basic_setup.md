# **`ramp.xds`**: Basic Setup

**Basic Setup** is defined by the use of
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
or one of its variants. It returns an **xds** model object that can be
solved or or modified through advanced setup. The following briefly
describes the modular design, the setup variants and when to use them,
and a slightly longer discussion of what they do.

## Modularity

**`ramp.xds`** is designed for modular computation. The mathematical
framework has five core biological processes organized into three chunks
called **dynamical components** (see
[xds_info_dynamical_components](https://dd-harp.github.io/ramp.xds/reference/xds_info_dynamical_components.md)).
The modular design makes it possible to study a system that with all
three components fully defined (using
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)),
or to develop systems of equations that isolate and study some part of a
system.

To implement *plug-and-play* modularity, a trivial module was developed
for each component. The trivial model ignores inputs from upstream
modules, and it congfigures a a *trace function* to pass outputs
downstream (see
[xds_info_trivial_forcing](https://dd-harp.github.io/ramp.xds/reference/xds_info_trivial_forcing.md)).

## Basic Setup Functions

Depending on the purpose of the study, **Basic Setup** is done by one of
five functions:

- [xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
  can be used for most studies. It configures all three dynamical
  components.

- [xds_setup_human](https://dd-harp.github.io/ramp.xds/reference/xds_setup_human.md)
  sets up an **XH** module to study disease dynamics forced by infective
  biting in the patches.

- [xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md)
  sets up an **XH** module to study disease dynamics forced by the daily
  EIR. Use this to study cohort dynamics using
  [xds_solve_cohort](https://dd-harp.github.io/ramp.xds/reference/xds_solve_cohort.md).

- [xds_setup_mosy](https://dd-harp.github.io/ramp.xds/reference/xds_setup_mosy.md)
  sets a model for mosquito ecology that ignores infection dynamics.

- [xds_setup_aquatic](https://dd-harp.github.io/ramp.xds/reference/xds_setup_aquatic.md)
  sets up a model for aquatic dynamics forced by egg laying.

Each function sets `class(xds_obj$frame)` to dispatch functions that are
used to solve dynamical systems.

## [xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)

[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
sets `class(xds_obj$frame)="full"` and requires arguments to set up all
three dynamical components.

## [xds_setup_mosy](https://dd-harp.github.io/ramp.xds/reference/xds_setup_mosy.md)

[xds_setup_mosy](https://dd-harp.github.io/ramp.xds/reference/xds_setup_mosy.md)
sets `class(xds_obj$frame)="mosy"` for models of mosquito ecology. It

- sets up a **L** module

- sets up a **MY** module, but the **Y** processes are ignored

- sets up a trivial **XH** module to pass human population density

## [xds_setup_aquatic](https://dd-harp.github.io/ramp.xds/reference/xds_setup_aquatic.md)

[xds_setup_aquatic](https://dd-harp.github.io/ramp.xds/reference/xds_setup_aquatic.md)
sets `class(xds_obj$frame)="aquatic"`for models of aquatic mosquito
ecology. It

- sets up a **L** module

- sets up a trivial **MY** module and configures a trace function for
  egg laying

- sets up an **XH** component to be ignored

## [xds_setup_human](https://dd-harp.github.io/ramp.xds/reference/xds_setup_human.md)

[xds_setup_human](https://dd-harp.github.io/ramp.xds/reference/xds_setup_human.md)
sets `class(xds_obj$frame)="human"` for models of malaria epidemiology
or other disease dynamics forced by infective biting in the patches. It

- sets up the **XH** module

- sets up a trivial **MY** component and configures a trace function for
  infective biting in patches

- sets up an **L** component to be ignored

## [xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md)

[xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md)
sets `class(xds_obj$frame)="eir"` for models of malaria epidemiology or
other disease dynamics forced by the EIR. These models can also be
solved by
[xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)
or
[xds_solve_cohort](https://dd-harp.github.io/ramp.xds/reference/xds_solve_cohort.md):
in the former, **time** is the independent variable, and in the latter
**age.** Unlike the other variants, these models lack the **MY** and
**L** components. Instead, it

- sets up the **XH** module

  - the average eir

  - a seasonal pattern function

  - a trend pattern function

  - a shock function

  - relative biting by age, which is used by
    [xds_solve_cohort](https://dd-harp.github.io/ramp.xds/reference/xds_solve_cohort.md)

## See also

[xds_info_dynamical_components](https://dd-harp.github.io/ramp.xds/reference/xds_info_dynamical_components.md),
[xds_info_trivial_forcing](https://dd-harp.github.io/ramp.xds/reference/xds_info_trivial_forcing.md)
and
[xds_object](https://dd-harp.github.io/ramp.xds/reference/xds_object.md)
