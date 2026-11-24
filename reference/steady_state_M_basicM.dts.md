# Compute steady states for `basicM` (**MY**)

Compute steady-state mosquito densities for the `basicM` module given
adult mosquito emergence `Lambda` for the discrete-time system. Minimize
the sum of squared fixed-point residuals using
[stats::nlm](https://rdrr.io/r/stats/nlm.html).

## Usage

``` r
# S3 method for class 'dts'
steady_state_M_basicM(Lambda, xds_obj, s = 1)
```

## Arguments

- Lambda:

  the daily emergence rate of adult mosquitoes

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a named [numeric](https://rdrr.io/r/base/numeric.html) vector containing
`M` followed by `P`, each with one value per patch

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes: those that have blood-fed at least
  once

## DTS

Adult mosquitoes emerge from aquatic habitats at rate \\\Lambda\\ and
disperse and die according to \\\Omega\\. The one-day update in
[Update_MYt.basicM](https://dd-harp.github.io/ramp.xds/reference/Update_MYt.basicM.md)
is \$\$ \begin{array}{rl} M_t &= \Lambda+e^{-\Omega}\cdot M\_{t-1} \\\\
P_t &= f(M\_{t-1}-P\_{t-1})+e^{-\Omega}\cdot P\_{t-1} \end{array}\$\$
Here, \\P\\ is the density of mosquitoes that have blood-fed at least
once.

## See also

[basicM](https://dd-harp.github.io/ramp.xds/reference/basicM.md)
