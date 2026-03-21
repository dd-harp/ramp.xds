# `trivial` — **L** Module

The trivial module outputs the emergence rate of adult mosquitoes by
calling a *trace function,* \$\$F\_\alpha(t) = \Lambda S(t) T(t)
K(t)\$\$ where

- \\\Lambda\\ or `Lambda` is the mean number of adult female mosquitoes
  emerging per day

- \\S(t)\\ or `F_season` is a seasonal pattern

- \\T(t)\\ or `F_trend` is a trend pattern

- \\K(t)\\ or `F_shock` is a perturbation

## Parameters

- `Lambda`:

  the mean daily emergence rate

- `season_par`:

  parameters for
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md):
  `F_season=make_function(season_par)`

- `trend_par`:

  parameters for
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md):
  `F_trend=make_function(trend_par)`

- `shock_par`:

  parameters for
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md):
  `F_shock=make_function(shock_par)`

The default setup option is
`season_par = trend_par = shock_par = makepar_F_one()`.

## Get

- `get_L_pars` — the `trivial` method returns all the parameters

- `get_mean_forcing` — get `Lambda`

- `get_season` — returns `season_par`

- `get_trend` — returns `trend_par`

- `get_shock` — returns `shock_par`

## Change

- `change_L_pars` — change parameters by name

- `change_mean_forcing` — changes `Lambda`

- `change_season` — changes elements of `season_par`

- `change_trend` — changes elements of `trend_par`

- `change_shock` — changes elements of `shock_par`

## Notes

1.  The module has no state variables.

2.  The size of an object saved by `saveRDS` balloons if it saves a
    function, so `saveXDS` function strips the functions and `readRDS`
    remakes the function from the stored parameters. `F_season`,
    `F_trend`, and `F_shock` can be set up manually by passing any user
    defined function. If so, the user should use `saveRDS` and `readRDS`
    rather than `saveXDS` and `readXDS`

3.  Setup expects that `membership= c(1:nPatches),` but any membership
    vector works.
