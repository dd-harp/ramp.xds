# `trivialXH` — **XH** module

The trivial module sets up two trace functions:

1.  a trace function to output net infectiousness, \\(\kappa\\)

2.  a trace function to output human population density, \\(H\\)

Basic setup uses a function if it is passed. If no function is passed,
the functions are built from function objects (see
[ramp.func::F_obj](https://dd-harp.github.io/ramp.func/reference/F_obj.html))
using
[make_function](https://dd-harp.github.io/ramp.func/reference/make_function.html),
which returns a function of the form \\(F(t,V).\\)

The variables passed to the functions, \\(V\\), are returned by
[get_variables](https://dd-harp.github.io/ramp.xds/reference/get_variables.md),
which dispatches on the class of the stored function object (by default,
an empty list).

## Net Infectiousness

The trace functions are set up as composed time series functions with
four separate composable elements: \$\$F\_\kappa(t) = \kappa \times S(t,
V_s) \times T(t, V_t) \times K(t, V_k)\$\$ where

- \\(\kappa\\) or `kappa` is the mean net infectiousness

- \\(S(t,V_s)\\) or `F_season` is a seasonal pattern function

- \\(T(t,V_t)\\) or `F_trend` is a trend pattern function

- \\(K(t,V_k)\\) or `F_shock` is a perturbation function

## Human Population Density

The trace functions are set up as composed time series functions with
four separate composable elements: \$\$F_H(t) = H \times S(t, V_s)
\times T(t, V_t) \times K(t, V_k)\$\$ where

- \\(\kappa\\) or `kappa` is the mean net infectiousness

- \\(S(t,V_s)\\) or `H_season` is a seasonal pattern function

- \\(T(t,V_t)\\) or `H_trend` is a trend pattern function

- \\(K(t,V_k)\\) or `H_shock` is a perturbation function

## Seasonal Pattern

Note: \\(0 \leq F\_\kappa(t) \leq 1\\)

## Parameters

**Net Infectivity**

- `kappa`:

  mean net infectiousness

- `F_season`:

  a seasonal pattern function, \\({S(t,V_s)}\\)

- `F_trend`:

  a trend function, \\(T(t,V_t)\\)

- `F_shock`:

  a shock function, \\(K(t,V_k)\\)

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

**Human Population Density**

- `H`:

  human population density

- `H_season`:

  a seasonal pattern function, \\({S(t,V_s)}\\)

- `H_trend`:

  a trend function, \\(T(t,V_t)\\)

- `H_shock`:

  a shock function, \\(K(t,V_k)\\)

- `H_season_par`:

  dispatches
  [get_variables](https://dd-harp.github.io/ramp.xds/reference/get_variables.md)
  to get \\(V_s\\)

- `H_trend_par`:

  dispatches
  [get_variables](https://dd-harp.github.io/ramp.xds/reference/get_variables.md)
  to get \\(V_t\\)

- `H_shock_par`:

  dispatches
  [get_variables](https://dd-harp.github.io/ramp.xds/reference/get_variables.md)
  to get \\(V_k\\)

Setup also adds the objects `season_par` and `trend_par` and `shock_par`
for use by `ramp.func`

## Get and Change

- `get_XH_pars` — the `trivialXH` method returns all the parameters

- `change_XH_pars` — change parameters by name

## Notes

1.  The module has no state variables.

2.  The size of an object saved by `saveRDS` balloons if it saves a
    function, so `saveXDS` function strips the functions and `readXDS`
    remakes the function from the stored function object using
    [make_function](https://dd-harp.github.io/ramp.func/reference/make_function.html).

3.  `F_season`, `F_trend`, and `F_shock` can be set up manually by
    passing any user defined function. If so, the user should use
    `saveRDS` and `readRDS` rather than `saveXDS` and `readXDS`

4.  Setup expects that `membership= c(1:nPatches),` but any membership
    vector works.
