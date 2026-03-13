# Ports

In **`ramp.xds,`** a *port* is any parameter or term that has a flexible
implementation, usually by calling an `S3` function. Ports are an
important design feature that make the software extensible through the
addition of new `S3` methods.

By policy, the default static values assigned to ports at basic setup
are the "simplest" method. Other methods in **`ramp.xds`** are easily
configured through a `setup_*` function.

Examples:

- [mosquito_bionomics](https://dd-harp.github.io/ramp.xds/reference/mosquito_bionomics.md)
  parameters in some **MY** modules are handled as ports
