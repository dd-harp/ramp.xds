# Ports

**port** : a parameter or variable whose value is set by a function call
with a standardized, flexible implementation

The port is a design template that facilitates nimble model building by
establishing a protocol for adding model *features,* including aspects
of heterogeneous transmission, exogenous forcing (*eg* weather), malaria
control, malaria importation, and habitat dynamics. Ports add
functionality to an
[xds_object](https://dd-harp.github.io/ramp.xds/reference/xds_object.md):
default values assigned during
[xds_help_basic_setup](https://dd-harp.github.io/ramp.xds/reference/xds_help_basic_setup.md)
have no effect; features can be configured after basic setup (see
[xds_help_setup_options](https://dd-harp.github.io/ramp.xds/reference/xds_help_setup_options.md)).

In some cases, ports were added to
[dynamical_components](https://dd-harp.github.io/ramp.xds/reference/dynamical_components.md)
or the the blood feeding or egg laying interfaces (see
[xds_interfaces](https://dd-harp.github.io/ramp.xds/reference/xds_interfaces.md)).
In some cases, a
[junction](https://dd-harp.github.io/ramp.xds/reference/junction.md) is
added to handle bundles of ports (*eg* Vector Control).

## Value

Each port is a flexible method for assigning a value parameter or
variable. The current value of the parameter or variable has a location
on some object or module object, and it is always retrieved from that
location:

`xds_obj$...$value`

## Functional Response

The value of the parameter is set by a function call with a standard
form:

`xds_obj <- this_case_port(t, xds_obj)`

where `t` is time. The `xds_obj` is passed, modified, and returned.

Inside `this_case_port` functions can use any variables that have been
defined. The value is set by a function call:

`xds_obj$...$value <- F_this_case(t, vars)`

where `F_this_case` is an `S3` object that dispatches on `class(value)`.

## Default Case

The default case for `F_this_case` is a pair of methods that set a
constant value for the parameter:

- `this_case_port.setup`

  - assigns a static value to `xds_obj$...$value`

  - sets `class(xds_obj$...$value) = "static"`

- `this_case_port.static` returns `xds_obj` unmodified

## Trace Case

For most ports, an alternative case sets the value by a call to a trace
function, configured using the package standard decomposable time series
trace function (see
[make_ts_function](https://dd-harp.github.io/ramp.xds/reference/make_ts_function.md)).

## Setup

All methods should have well-defined setup functions:

`xds_obj <- setup_this_case(case_name, xds_obj, options, s)`

should work.

## Extensibility

Any port can be modified by adding a new `S3` method. Since most of
these examples involve some form of mechanistic forcing, new cases are
generally added to **`ramp.forcing`.** All new methods should be fully
documented.

For every port, the mathematical consequences of *exogenous forcing*
should be carefully examined. For any parameter or term that is
implemented as a port, the supporting mathematics should be fully worked
out, so the user can *set it and forget it.*

## Example

In
[macdonald](https://dd-harp.github.io/ramp.xds/reference/macdonald.md),
the bionomic parameters are assigned constant values: those parameters
are not ports. That module does not have the *skill set* to handle
exogenous forcing.

In the generalized Ross-Macdonald
[GeRM](https://dd-harp.github.io/ramp.xds/reference/GeRM.md), several
bionomic parameters are set by calling an `S3` function. In this case,
the equations have been set up to handle time-varying survival through a
time-varying EIP (see
[mosquito_bionomics](https://dd-harp.github.io/ramp.xds/reference/mosquito_bionomics.md)).

## See also

[xds_help_setup_options](https://dd-harp.github.io/ramp.xds/reference/xds_help_setup_options.md),
[junction](https://dd-harp.github.io/ramp.xds/reference/junction.md)
