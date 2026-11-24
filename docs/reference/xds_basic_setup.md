# Basic Setup Functions

**Basic Setup** is defined by the use of
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
or one of its variants. It returns an **xds** model object that can be
solved by
[xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)
or modified through advanced setup. The functions do three things:

- **Dynamics** — set up the dynamical components

- **Model Structure** — set up the basic model structure

- **Basic Spatial Dynamics** — set up the basic spatial parameters

**Advanced Setup** —
[xds_advanced_setup](https://dd-harp.github.io/ramp.xds/reference/xds_advanced_setup.md)

## Basic Setup Functions

Depending on the purpose of the study, **Basic Setup** is done by one of
five functions:

- [xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
  can be used for most studies. It configures all three dynamical
  components.

- [xds_setup_mosy](https://dd-harp.github.io/ramp.xds/reference/xds_setup_mosy.md)
  sets a model for mosquito ecology that ignores infection dynamics.

- [xds_setup_aquatic](https://dd-harp.github.io/ramp.xds/reference/xds_setup_aquatic.md)
  sets up a model for aquatic dynamics forced by egg laying.

- [xds_setup_human](https://dd-harp.github.io/ramp.xds/reference/xds_setup_human.md)
  sets up an **XH** module to study disease dynamics forced by infective
  biting in the patches.

- [xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md)
  sets up an **XH** module to study disease dynamics forced by the daily
  EIR.

Each function sets `class(xds_obj$frame)` to dispatch functions that are
used to solve dynamical systems.

## [xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)

[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
sets `class(xds_obj$frame)="full"` and requires arguments to set up all
three dynamical components.

## [xds_setup_mosy](https://dd-harp.github.io/ramp.xds/reference/xds_setup_mosy.md)

[xds_setup_mosy](https://dd-harp.github.io/ramp.xds/reference/xds_setup_mosy.md)
sets `class(xds_obj$frame)="mosy"` for models of mosquito ecology:

- sets up an **L** module

- sets up a **MY** module, but the **Y** processes are ignored

- sets up a trivial **XH** module to pass human population density

## [xds_setup_aquatic](https://dd-harp.github.io/ramp.xds/reference/xds_setup_aquatic.md)

[xds_setup_aquatic](https://dd-harp.github.io/ramp.xds/reference/xds_setup_aquatic.md)
sets `class(xds_obj$frame)="aquatic"` for models of aquatic mosquito
ecology:

- sets up an **L** module

- sets up a trivial **MY** module and configures a forcing function for
  egg laying

- sets up an **XH** component to be ignored

## [xds_setup_human](https://dd-harp.github.io/ramp.xds/reference/xds_setup_human.md)

[xds_setup_human](https://dd-harp.github.io/ramp.xds/reference/xds_setup_human.md)
sets `class(xds_obj$frame)="human"` for models of malaria epidemiology
or other disease dynamics forced by infective biting in the patches:

- sets up the **XH** module

- sets up a trivial **MY** component and configures a forcing function
  for infective biting in patches

- sets up an **L** component to be ignored

## [xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md)

[xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md)
sets `class(xds_obj$frame)="eir"` for models of malaria epidemiology or
other disease dynamics forced by the EIR:

- sets up the **XH** module

- sets up a composed time series function for the EIR

- (optional) configures relative biting rates by age for cohort
  dynamics; defaults to `F_one`

These models do not have active **MY** or **L** dynamical components.
Use:

- [xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)
  when the independent variable is **time**

- [xds_solve_cohort](https://dd-harp.github.io/ramp.xds/reference/xds_solve_cohort.md)
  for cohort dynamics, when the independent variable is **age**

## See also

[xds_info_dynamical_components](https://dd-harp.github.io/ramp.xds/reference/xds_info_dynamical_components.md),
[xds_info_trivial_forcing](https://dd-harp.github.io/ramp.xds/reference/xds_info_trivial_forcing.md)
and
[xds_object](https://dd-harp.github.io/ramp.xds/reference/xds_object.md)
