# `trivial` — **L** module

The trivial module outputs the emergence rate of adult mosquitoes by
calling a *trace function,* \$\$F\_\alpha(t) = \Lambda \times F_S(t,
V_s) \times F_T(t, V_t) \times F_K(t, V_k)\$\$ where

- \\\Lambda\\ is the mean daily emergence rate of adult mosquitoes

- \\F_S(t,V_s)\\ or `F_season` is a seasonal pattern function

- \\F_T(t,V_t)\\ or `F_trend` is a trend pattern function

- \\F_K(t,V_k)\\ or `F_shock` is a perturbation function

The variables \\V_s\\, \\V_t\\, and \\V_t\\ are called by
[get_variables](https://dd-harp.github.io/ramp.xds/reference/get_variables.md),
which dispatches on the class of `season_par` or `trend_par` or
`shock_par`.

## Parameters

- `Lambda`:

  the mean daily emergence rate

- `F_season`:

  a seasonal pattern function, \\{F_S(t,V_s)}\\

- `F_trend`:

  a trend function, \\F_T(t,V_t)\\

- `F_shock`:

  a shock function, \\F_K(t,V_k)\\

- `season_par`:

  dispatches
  [get_variables](https://dd-harp.github.io/ramp.xds/reference/get_variables.md)
  to get \\V_s\\

- `trend_par`:

  dispatches
  [get_variables](https://dd-harp.github.io/ramp.xds/reference/get_variables.md)
  to get \\V_t\\

- `shock_par`:

  dispatches
  [get_variables](https://dd-harp.github.io/ramp.xds/reference/get_variables.md)
  to get \\V_k\\

The default values are `F_season=F_trend=F_shock=F_one` and the classes
of the objects `season_par` and `trend_par` and `shock_par` are all
`list`

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
