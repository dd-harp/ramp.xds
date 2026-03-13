# Trivial Forcing

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
parameters generated for
[make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md).

- `season_par` creates \\S(t)\\ or `F_season` (*eg,* using
  [makepar_F_sin](https://dd-harp.github.io/ramp.xds/reference/makepar_F_sin.md))

- `trend_par` creates \\T(t)\\ or `F_trend` (*eg,* using
  [makepar_F_spline](https://dd-harp.github.io/ramp.xds/reference/makepar_F_spline.md))

- `shock_par` creates \\K(t)\\ or `F_shock` (*eg,* using
  [makepar_F_sharkbite](https://dd-harp.github.io/ramp.xds/reference/makepar_F_sharkbite.md))

For a discussion of mechanistic approaches to forcing, see
[Forcing](https://dd-harp.github.io/ramp.xds/reference/Forcing.md)

## See also

[trivial_XH](https://dd-harp.github.io/ramp.xds/reference/trivial_XH.md),
[trivial_MY](https://dd-harp.github.io/ramp.xds/reference/trivial_MY.md),
[trivial_L](https://dd-harp.github.io/ramp.xds/reference/trivial_L.md),
[xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md),
[make_ts_function](https://dd-harp.github.io/ramp.xds/reference/make_ts_function.md),
and [Forcing](https://dd-harp.github.io/ramp.xds/reference/Forcing.md)
