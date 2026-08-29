# Forcing with Trivial Modules

Trivial modules were developed for each one of the three dynamical
components, making it possible to develop studies of some focal process
with known inputs: a *trace function* approach.

The trivial modules and EIR forcing (see
[xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md))
construct trace functions as composed time series functions. The value
of a forced variable \\x(t)\\ is computed as a product of four
configurable elements:

- \\\bar x\\: a mean value

- \\F_S(t, V)\\: a seasonal pattern

- \\F_T(t, V)\\: a trend

- \\F_K(t, V)\\: a shock

\$\$x(t) = \bar x \times F_S(t, V) \times F_T(t,V) \times F_K(t, V)\$\$

- A trace function library for RAMP is in `ramp.func`

- Functions that integrate `ramp.func` and `ramp.xds` are found in
  `ramp.forcing`

For a discussion of mechanistic approaches to forcing, see [SimBA: Trace
Functions](https://faculty.washington.edu/smitdave/simba/trace.html)

## See also

[trivial_XH](https://dd-harp.github.io/ramp.xds/reference/trivial_XH.md),
[trivial_MY](https://dd-harp.github.io/ramp.xds/reference/trivial_MY.md),
[trivial_L](https://dd-harp.github.io/ramp.xds/reference/trivial_L.md),
[xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md)
