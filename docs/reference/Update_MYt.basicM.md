# Update state variables for `basicM` (**MY**)

Implements
[Update_MYt](https://dd-harp.github.io/ramp.xds/reference/Update_MYt.md)
for the basicM model.

## Usage

``` r
# S3 method for class 'basicM'
Update_MYt(t, y, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes: those that have blood-fed at least
  once

## Dynamics

Adult mosquitoes emerge from aquatic habitats at rate \\(\Lambda\\) and
disperse and die according to \\(\Omega\\). The ODE model is \$\$
\begin{array}{rl} dM/dt &= \Lambda-\Omega\cdot M \\\\\\\\ dP/dt &=
f(M-P)-\Omega\cdot P \end{array}\$\$ Here, \\(P\\) is the density of
mosquitoes that have blood-fed at least once. Parity, \\(P/M\\), is
computed during parsing and stored as `parous`. For discrete-time
models, Update_MYt.basicM uses the one-day update equations.

## See also

[basicM](https://dd-harp.github.io/ramp.xds/reference/basicM.md),
[dMYdt.basicM](https://dd-harp.github.io/ramp.xds/reference/dMYdt.basicM.md)
