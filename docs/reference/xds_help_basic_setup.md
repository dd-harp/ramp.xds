# **`ramp.xds`**: Guide to Basic Setup

**Basic Setup** uses one of the following functions to build an
**`xds`** model:

- [xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
  sets up a full model with:

  - an **XH** Component

  - an **MY** Component

  - an **L** Component

- [xds_setup_human](https://dd-harp.github.io/ramp.xds/reference/xds_setup_human.md)
  sets up a models to study human epidemiology, given exposure:

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

## The Frame

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

[dynamical_components](https://dd-harp.github.io/ramp.xds/reference/dynamical_components.md),
[trivial_forcing](https://dd-harp.github.io/ramp.xds/reference/trivial_forcing.md)
and
[xds_object](https://dd-harp.github.io/ramp.xds/reference/xds_object.md)
