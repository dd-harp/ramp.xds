# The xds object frame

There are several variants of
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
that set the value of `xds_obj$frame.`

The modular design makes it possible to study a dynamical component in
isolation, or to focus on some combination of those elements.

To facilitate those studies, each component has a trivial module that
computes the dynamical term that gets passed to another component. The
user configures the *trace function* of the upstream component.

With
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md),
a user defines all three components, but other variants were developed
to make it easier to set up partial systems. The functions that solve
differential equations and parse outputs are written as `S3` functions
with methods that dispatch on `xds_obj$frame`

- [xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
  (`frame="full"`) sets up all three components.

- [xds_setup_mosy](https://dd-harp.github.io/ramp.xds/reference/xds_setup_mosy.md)
  (`frame="mosy"`) sets up models for mosquito ecology:

  - the model includes an **L** component

  - the model has an **M** component but no **Y** component (*eg*
    [basicM](https://dd-harp.github.io/ramp.xds/reference/basicM.md))

  - a trivial **H** module can be configured, but the **XH** derivatives
    are not computed

- [xds_setup_aquatic](https://dd-harp.github.io/ramp.xds/reference/xds_setup_aquatic.md)
  (`frame="aquatic"`) sets up models aquatic mosquito ecology:

  - a trivial **MY** component is configured for egg laying

  - the **XH** component is missing

- [xds_setup_human](https://dd-harp.github.io/ramp.xds/reference/xds_setup_human.md)
  (`frame="human"`) sets up models to study epidemiology

  - a trivial **MY** component is configured for infectious biting in
    patches

  - the **L** component is missing

- [xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md)
  (`frame="eir"`) sets up models to study epidemiology

  - a function \\F_E(t),\\ a trace function that passes the EIR. These
    models lack the **MY** and **L** components.

  - a trivial **MY** component is configured for infectious biting in
    patches

  - the **XH** component is missing

## See also

[trivial_forcing](https://dd-harp.github.io/ramp.xds/reference/trivial_forcing.md)
and
[xds_object](https://dd-harp.github.io/ramp.xds/reference/xds_object.md)
