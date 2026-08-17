# `trivial` — **MY** module

The trivial **MY** module configures two trace functions:

- infectious biting: \$\$F\_{fqZ}(t, V) = fqZ \times S(t, V_s) \times
  T(t, V_t) \times K(t, V_k)\$\$

- egg laying: \$\$F_G(t, V) = G \times S(t,V_s) \times T(t, V_t) \times
  K(t, V_k)\$\$

where

- \\G\\ or `eggs` is the mean egg laying rate

- \\fqZ\\ or `fqZ` is the mean number of infectious bites on humans, per
  patch

- \\S(t,V_s)\\ or `F_season` is a seasonal pattern function

- \\T(t,V_t)\\ or `F_trend` is a trend pattern function

- \\K(t,V_k)\\ or `F_shock` is a perturbation function

The variables \\V_s\\, \\V_t\\, and \\V_t\\ are called by
[get_variables](https://dd-harp.github.io/ramp.xds/reference/get_variables.md),
which dispatches on the class of `season_par` or `trend_par` or
`shock_par`.

The implementation assumes that only one of these functions gets used.

## Parameters

- `eggs`:

  the mean number of eggs laid, per day

- `Z`:

  the mean density of infectious mosquitoes

- `f`:

  the blood feeding rate

- `q`:

  the human fraction

- `F_season`:

  a seasonal pattern function, \\{S(t,V_s)}\\

- `F_trend`:

  a trend function, \\T(t,V_t)\\

- `F_shock`:

  a shock function, \\K(t,V_k)\\

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
'list'

For the bionomic parameters, `f=q=Z=eggs=1`.

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

- `change_MY_pars` — change bionomic parameters by name

- `change_mean_forcing` — changes `Z`

- `change_F_season` — changes `F_season`

- `change_F_trend` — changes `F_trend`

- `change_F_shock` — changes `F_shock`

## Notes

1.  The module has no state variables.

2.  The size of an object saved by `saveRDS` balloons if it saves a
    function, so `saveXDS` function strips the functions and `readRDS`
    remakes the function from the stored parameters.

`F_season`, `F_trend`, and `F_shock` can be set up manually by passing
any user defined function. If so, the user should use `saveRDS` and
`readRDS` rather than `saveXDS` and `readXDS`
