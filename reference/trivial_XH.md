# `trivial` — **XH** module

The trivial module outputs the net infectiousness, \\\kappa\\, by
calling a *trace function,* \$\$F\_\kappa(t) = \kappa \times S(t, V_s)
\times T(t, V_t) \times K(t, V_k)\$\$ where

- \\\kappa\\ or `kappa` is the mean net infectiousness

- \\S(t,V_s)\\ or `F_season` is a seasonal pattern function

- \\T(t,V_t)\\ or `F_trend` is a trend pattern function

- \\K(t,V_k)\\ or `F_shock` is a perturbation function

The variables \\V_s\\, \\V_t\\, and \\V_t\\ are called by
[get_variables](https://dd-harp.github.io/ramp.xds/reference/get_variables.md),
which dispatches on the class of `season_par` or `trend_par` or
`shock_par`.

Note: \\0 \leq F\_\kappa(t) \leq 1\\

## Parameters

- `kappa`:

  the net infectiousness

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

Setup also adds the objects `season_par` and `trend_par` and `shock_par`
for use by `ramp.trace`

## Get and Change

- `get_XH_pars` — the `trivial` method returns all the parameters

- `change_XH_pars` — change parameters by name

## Notes

1.  The module has no state variables.

2.  The size of an object saved by `saveRDS` balloons if it saves a
    function, so `saveXDS` function strips the functions and `readRDS`
    remakes the function from the stored parameters.

3.  `F_season`, `F_trend`, and `F_shock` can be set up manually by
    passing any user defined function. If so, the user should use
    `saveRDS` and `readRDS` rather than `saveXDS` and `readXDS`

4.  Setup expects that `membership= c(1:nPatches),` but any membership
    vector works.
