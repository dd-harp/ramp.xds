# Forcing with Trivial Modules

Trivial modules were developed for each one of the three dynamical
components, making it possible to develop studies of some focal process
with known inputs: a *trace function* approach.

Three trivial modules and EIR forcing
[xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md)
construct trace functions as decomposable time series. The value of a
forced variable \\x(t)\\ is computed as a product of four configurable
elements:

- \\\bar x\\: a mean value

- \\S(t)\\: a seasonal pattern

- \\T(t)\\: a trend

- \\K(t)\\: a shock

\$\$x(t) = \bar x \times S(t) \times T(t) \times K(t)\$\$

In the trivial modules, the functions are specified by passing
parameters generated for ramp.func::make_function.

- `season_par` creates \\S(t)\\ or `F_season` (*eg,* using
  ramp.func::makepar_F_sin)

- `trend_par` creates \\T(t)\\ or `F_trend` (*eg,* using
  ramp.func::makepar_F_spline)

- `shock_par` creates \\K(t)\\ or `F_shock` (*eg,* using
  ramp.func::makepar_F_sharkbite)

For a discussion of mechanistic approaches to forcing, see
[Forcing](https://dd-harp.github.io/ramp.xds/reference/Forcing.md)

## See also

[trivial_XH](https://dd-harp.github.io/ramp.xds/reference/trivial_XH.md),
[trivial_MY](https://dd-harp.github.io/ramp.xds/reference/trivial_MY.md),
[trivial_L](https://dd-harp.github.io/ramp.xds/reference/trivial_L.md),
[xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md),
ramp.func::make_ts_function, and
[Forcing](https://dd-harp.github.io/ramp.xds/reference/Forcing.md)
