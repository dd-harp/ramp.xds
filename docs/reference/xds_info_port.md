# Ports

- **port** – a parameter or variable whose value is set by a function
  call with a standardized, flexible implementation

- junction – a function that handles a bundle of ports (see
  [xds_info_junction](https://dd-harp.github.io/ramp.xds/reference/xds_info_junction.md))

The implementation of a port follows a design template. That template
establishes a protocol for writing modules with *features* that could be
implemented in several ways to add realism: heterogeneous transmission,
exogenous forcing (*eg* weather), malaria control, malaria importation,
habitat dynamics, or something else.

By implementing a parameter or variable as a port, a module or other
component becomes extensible with negligible computational costs:
default values assigned during
[xds_help_basic_setup](https://dd-harp.github.io/ramp.xds/reference/xds_help_basic_setup.md)
have no effect; but features can be configured after basic setup (see
[xds_help_setup_options](https://dd-harp.github.io/ramp.xds/reference/xds_help_setup_options.md)).

In developing ports, the mathematical consequences *ought to be* fully
considered. If some implementations of a port could be mathematically
inconsistent, the port documentation should include warnings.

## Port Value

Each port is a flexible method for assigning a value parameter or
variable (`par`). The current value of the parameter or variable has a
location on a base object (`base_obj`), and it is always retrieved from
that location:

`xds_obj$base_obj$par`

## Port Object

A port is set up and implemented through an object (`par_obj`) that is
located on the same base object:

`xds_obj$base_obj$par_obj`

The main purpose of `class(par_obj)` is to dispatch the port function,
so `par_obj` could be an empty list. If an empty list would do, it's
more useful for it to be informative. For example:

`par_obj <- this_case`

where `this_case = class(par_obj)`

In some cases, `par_obj` is a named list that holds other parameter
values.

## Port Function

The value of the parameter is set by a function call with a standard
form:

`xds_obj <- this_port(t, xds_obj, ...)`

where

- `t` is time

- The `xds_obj` is passed,

- `this_port` dispatches on `class(xds_obj$base_obj$par_obj).`

- `...` other arguments often include the index of the host or vector
  species.

Inside `this_port.this_case`, the parameter value can be set by a
function call:

`xds_obj$base_obj$par <- F_this_port(t, ...)`

where `F_this_port` is not constrained. Since `xds_obj` is passed to
`this_port`, functions can use any variables that have been defined
anywhere on the `xds_obj`.

## Default Case

The default case is:

`class(xds_obj$base_obj$par_obj) = "static"`

and `this_port.static` returns `xds_obj` unmodified.

## Trace Function Case

For most ports, an alternative case is to configure a trace function
using package standard *multiplicative decomposable time series* trace
function (see
[make_ts_function](https://dd-harp.github.io/ramp.xds/reference/make_ts_function.md)).

## Setup

All methods should have well-defined setup functions:

`xds_obj <- setup_this_port(case_name, xds_obj, options, ...)`

where `case_name` is used to dispatch `setup_this_port`

In cases where changing `par` should trigger function calls to change
other parameters that depend on it, `setup_this_port.static` should set

`class(xds_obj$base_obj$par_obj) = "setup"`

The "setup" method

- calls those other functions

- sets `class(par_obj) = "static"`

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

In the generalized Macdonald
[GeM](https://dd-harp.github.io/ramp.xds/reference/GeM.md), several
bionomic parameters are set by calling an `S3` function. In this case,
the equations have been set up to handle time-varying survival through a
time-varying EIP (see
[mosquito_bionomics](https://dd-harp.github.io/ramp.xds/reference/mosquito_bionomics.md)).

## See also

[xds_help_setup_options](https://dd-harp.github.io/ramp.xds/reference/xds_help_setup_options.md)
\|
[xds_info_junction](https://dd-harp.github.io/ramp.xds/reference/xds_info_junction.md)
