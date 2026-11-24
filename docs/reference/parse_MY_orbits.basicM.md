# Parse outputs for `basicM` (**MY**)

Extract the model state outputs for the `basicM` module from `outputs`
and return them by name in a list. Also compute `parous = P/M`.

## Usage

``` r
# S3 method for class 'basicM'
parse_MY_orbits(outputs, xds_obj, s)
```

## Arguments

- outputs:

  a [matrix](https://rdrr.io/r/base/matrix.html) of model state outputs

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [list](https://rdrr.io/r/base/list.html)

## Note

Variable indices are stored at `xds_obj$MY_obj[[s]]$ix`.

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes: those that have blood-fed at least
  once

## ODE

Adult mosquitoes emerge from aquatic habitats at rate \\(\Lambda\\) and
disperse and die according to \\(\Omega\\). The ODE model is \$\$
\begin{array}{rl} dM/dt &= \Lambda-\Omega\cdot M \\\\\\\\ dP/dt &=
f(M-P)-\Omega\cdot P \end{array}\$\$ Here, \\(P\\) is the density of
mosquitoes that have blood-fed at least once.

## DTS

Adult mosquitoes emerge from aquatic habitats at rate \\(\Lambda\\) and
disperse and die according to \\(\Omega\\). The one-day update in
[Update_MYt.basicM](https://dd-harp.github.io/ramp.xds/reference/Update_MYt.basicM.md)
is \$\$ \begin{array}{rl} M_t &= \Lambda+e^{-\Omega}\cdot M\_{t-1}
\\\\\\\\ P_t &= f(M\_{t-1}-P\_{t-1})+e^{-\Omega}\cdot P\_{t-1}
\end{array}\$\$ Here, \\(P\\) is the density of mosquitoes that have
blood-fed at least once.

## See also

[basicM](https://dd-harp.github.io/ramp.xds/reference/basicM.md),
[get_MY_vars.basicM](https://dd-harp.github.io/ramp.xds/reference/get_MY_vars.basicM.md)
