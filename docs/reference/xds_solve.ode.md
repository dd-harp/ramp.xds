# Solve a System of Ordinary Differential Equations

Implements
[xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)
for ordinary differential equations. This method solves a system of
equations and computes the values of the dependent variables at times
\\t=0, dt, 2dt, ... Tmax\\. The function dispatches on `xds_obj$dlay` to
call:

- [deSolve::ode](https://rdrr.io/pkg/deSolve/man/ode.html) if
  `class(dlay) == 'ode'`

- [deSolve::dede](https://rdrr.io/pkg/deSolve/man/dede.html) if
  `class(dlay) == 'dde'`

## Usage

``` r
# S3 method for class 'ode'
xds_solve(xds_obj, Tmax = 365, dt = 1, times = NULL)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- Tmax:

  the last time point, run from 0...Tmax

- dt:

  the time step

- times:

  the times

## Value

an **`xds`** object
