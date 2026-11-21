# `trivialMY` — **MY** module

The trivial **MY** module configures one of two forcing functions,
either:

- infectious biting: \$\$F\_{fqZ}(t, V) = fqZ \times F_S(t, V_s) \times
  F_T(t, V_t) \times F_K(t, V_k)\$\$

- egg laying: \$\$F_G(t, V) = G \times F_S(t,V_s) \times F_T(t, V_t)
  \times F_K(t, V_k)\$\$

where

- \\G\\ or `eggs` is the mean egg laying rate

- \\fqZ\\ or `fqZ` is the mean number of infectious bites on humans, per
  patch

- \\F_S(t,V_s)\\ or `F_season` is a seasonal pattern function

- \\F_T(t,V_t)\\ or `F_trend` is a trend pattern function

- \\F_K(t,V_k)\\ or `F_shock` is a perturbation function

The variables \\V_s\\, \\V_t\\, and \\V_t\\ are called by
[get_variables](https://dd-harp.github.io/ramp.xds/reference/get_variables.md),
which dispatches on the class of `season_par` or `trend_par` or
`shock_par`.

The implementation assumes that only one of these functions will be
used, so it is not possible to configure different pattern functions for
egg laying and infective biting.

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
'list'

For the bionomic parameters, `f=q=Z=eggs=1`.

## Notes

1.  The module has no state variables.

2.  The size of an object saved by `saveRDS` balloons if it saves a
    function, so `saveXDS` function strips the functions and `readRDS`
    remakes the function from the stored parameters.

`F_season`, `F_trend`, and `F_shock` can be set up manually by passing
any user defined function. If so, the user should use `saveRDS` and
`readRDS` rather than `saveXDS` and `readXDS`

## See also

[make_MY_obj_trivial](https://dd-harp.github.io/ramp.xds/reference/make_MY_obj_trivial.md)
