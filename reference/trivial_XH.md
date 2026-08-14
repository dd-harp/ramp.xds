# `trivial` — **XH** module

The trivial module outputs the net infectiousness, \\\kappa\\, by
calling a *trace function,* \$\$F\_\kappa(t) = \kappa S(t) T(t) K(t)\$\$
where

- \\\kappa\\ or `kappa` is the mean net infectiousness

- \\S(t)\\ or `F_season` is a seasonal pattern

- \\T(t)\\ or `F_trend` is a trend pattern

- \\K(t)\\ or `F_shock` is a perturbation

Note: \\0 \leq F\_\kappa(t) \leq 1\\

## Parameters

- `kappa`:

  the net infectiousness

- `F_season`:

  a seasonal pattern function, \\{S(t)}\\

- `F_trend`:

  a trend function, \\T(t)\\

- `F_shock`:

  a shock function, \\K(t)\\

Setup also adds the objects `season_par` and `trend_par` and `shock_par`
for use by `ramp.trace`

## Get

- `get_XH_pars` — the `trivial` method returns all the parameters

## Change

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
