# `trivial` — **MY** Module

The trivial **MY** module configures two trace functions:

- infectious biting: \$\$F\_{fqZ}(t) = fqZ S(t) T(t) K(t)\$\$

- egg laying: \$\$F_G(t) = G S(t) T(t) K(t)\$\$

where

- \\G\\ or `eggs` is the mean egg laying rate

- \\fqZ\\ or `fqZ` is the mean number of infectious bites on humans, per
  patch

- \\S(t)\\ or `F_season` is a seasonal pattern

- \\T(t)\\ or `F_trend` is a trend pattern

- \\K(t)\\ or `F_shock` is a perturbation

## Parameters

- `eggs`:

  the mean number of eggs laid, per day

- `Z`:

  the mean density of infectious mosquitoes

- `f`:

  the blood feeding rate

- `q`:

  the human fraction

- `season_par`:

  parameters for
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md):
  `F_season=make_par(season_par`)

- `trend_par`:

  parameters for
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md):
  `F_trend=make_par(trend_par)`

- `shock_par`:

  parameters for
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md):
  `F_shock=make_par(shock_par)`

The default setup options:

- for the
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md)
  parameters, `season_par = trend_par = shock_par = makepar_F_one()`.

- for the bionomic parameters, `f=q=Z=eggs=1`.

## Get

- `get_MY_pars` — the `trivial` method returns all the parameters

- `get_mean_forcing` — returns `Z`

- `get_f` — returns `f`

- `get_q` — returns `q`

- `get_season` — returns `season_par`

- `get_trend` — returns `trend_par`

- `get_shock` — returns `shock_par`

Note: use `get_MY_pars` to inspect `eggs`

## Change

- `change_MY_pars` — change parameters by name

- `change_mean_forcing` — changes `Z`

- `change_season` — changes elements of `season_par`

- `change_trend` — changes elements of `trend_par`

- `change_shock` — changes elements of `shock_par`

Note: use `change_MY_pars` to change `eggs`

## Notes

1.  The module has no state variables.

2.  The size of an object saved by `saveRDS` balloons if it saves a
    function, so `saveXDS` function strips the functions and `readRDS`
    remakes the function from the stored parameters. `F_season`,
    `F_trend`, and `F_shock` can be set up manually by passing any user
    defined function. If so, the user should use `saveRDS` and `readRDS`
    rather than `saveXDS` and `readXDS`
