# `trivial` — **MY** module

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

- `F_season`:

  a seasonal pattern function, \\{S(t)}\\

- `F_trend`:

  a trend function, \\T(t)\\

- `F_shock`:

  a shock function, \\K(t)\\

The default values are `F_season=F_trend=F_shock=F_one`

Setup also adds the objects `season_par` and `trend_par` and `shock_par`
for use by `ramp.trace`

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
