# The trivial module for the L component

The trivial module outputs the emergence rate of adult mosquitoes by
calling a *forcing function*: \$\$F\_\alpha(t) = \Lambda \times F_S(t,
V_s) \times F_T(t, V_t) \times F_K(t, V_k)\$\$ where

- \\(\Lambda\\) is the mean daily emergence rate of adult mosquitoes
  from each aquatic habitat

- \\(F_S(t,V_s)\\) or `F_season` is a seasonal pattern function

- \\(F_T(t,V_t)\\) or `F_trend` is a trend pattern function

- \\(F_K(t,V_k)\\) or `F_shock` is a perturbation function

The variables \\(V_s\\), \\(V_t\\), and \\(V_k\\) are called by
[get_variables](https://dd-harp.github.io/ramp.xds/reference/get_variables.md),
which dispatches on the class of `season_par` or `trend_par` or
`shock_par`.

## Parameters

- `Lambda`:

  the mean daily emergence rate of adult mosquitoes from each aquatic
  habitat

- `F_season`:

  a seasonal pattern function, \\({F_S(t,V_s)}\\)

- `F_trend`:

  a trend function, \\(F_T(t,V_t)\\)

- `F_shock`:

  a shock function, \\(F_K(t,V_k)\\)

- `season_par`:

  dispatches
  [get_variables](https://dd-harp.github.io/ramp.xds/reference/get_variables.md)
  to get \\(V_s\\)

- `trend_par`:

  dispatches
  [get_variables](https://dd-harp.github.io/ramp.xds/reference/get_variables.md)
  to get \\(V_t\\)

- `shock_par`:

  dispatches
  [get_variables](https://dd-harp.github.io/ramp.xds/reference/get_variables.md)
  to get \\(V_k\\)

By default, `season_par`, `trend_par`, and `shock_par` configure
constant-one forcing functions.

## Notes

1.  The module has no state variables.

2.  The size of an object saved by `saveRDS` balloons if it saves a
    function, so the `saveXDS` function strips the functions and
    `readXDS` remakes the functions from the stored parameters.
    `F_season`, `F_trend`, and `F_shock` can be set up manually by
    passing any user-defined function. If so, the user should use
    `saveRDS` and `readRDS` rather than `saveXDS` and `readXDS`

3.  Setup expects that `membership= c(1:nPatches)`, but any membership
    vector works.

## See also

- [get_L_pars](https://dd-harp.github.io/ramp.xds/reference/get_L_pars.md),
  [get_mean_forcing](https://dd-harp.github.io/ramp.xds/reference/get_mean_forcing.md),
  [get_season](https://dd-harp.github.io/ramp.xds/reference/get_season.md),
  [get_trend](https://dd-harp.github.io/ramp.xds/reference/get_trend.md),
  [get_shock](https://dd-harp.github.io/ramp.xds/reference/get_shock.md)

- [show_season](https://dd-harp.github.io/ramp.xds/reference/show_season.md),
  [show_trend](https://dd-harp.github.io/ramp.xds/reference/show_trend.md),
  [show_shock](https://dd-harp.github.io/ramp.xds/reference/show_shock.md)

- [change_L_pars](https://dd-harp.github.io/ramp.xds/reference/change_L_pars.md),
  [change_mean_forcing](https://dd-harp.github.io/ramp.xds/reference/change_mean_forcing.md),
  [change_season](https://dd-harp.github.io/ramp.xds/reference/change_season.md),
  [change_trend](https://dd-harp.github.io/ramp.xds/reference/change_trend.md),
  [change_shock](https://dd-harp.github.io/ramp.xds/reference/change_shock.md)
