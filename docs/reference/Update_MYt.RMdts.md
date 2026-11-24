# Update states for `RMdts` (**MY**)

Update the state variables for the `RMdts` module: \$\$\begin{array}{rl}
M_t &= \Lambda+\Omega\cdot M\_{t-1} \\\\\\\\ P_t &=
f(M\_{t-1}-P\_{t-1})+\Omega\cdot P\_{t-1} \\\\\\\\ U_t &=
\Lambda+\Omega\cdot(e^{-fq\kappa}U\_{t-1}) \\\\\\\\ \widetilde{Y}\_t &=
\Omega\cdot\left(Y\_{t-1}\cdot\operatorname{diag}(1-G_t)\right) \\\\\\\\
Z_t &= \Omega\cdot(Y\_{t-1}\cdot G_t)+\Omega\cdot Z\_{t-1}
\end{array}\$\$ Here `G_t` is `G[Gix]`, where
`Gix = c(t-1:max_eip) %% max_eip + 1`. Let \\(a\\) be
`eip_day_ix = (t %% max_eip) + 1` and \\(b\\) be
`eip_yday_ix = ((t-1) %% max_eip) + 1`. Starting from
\\(Y_t=\widetilde{Y}\_t\\), apply these column assignments in order:
\$\$\begin{array}{rl} Y_t\[,b\] &\leftarrow Y_t\[,b\]+Y_t\[,a\] \\\\\\\\
Y_t\[,a\] &\leftarrow \Omega\cdot\left((1-e^{-fq\kappa})U\_{t-1}\right)
\end{array}\$\$

## Usage

``` r
# S3 method for class 'RMdts'
Update_MYt(t, y, xds_obj, s)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [list](https://rdrr.io/r/base/list.html)

## State Variables

- `M`:

  density of adult mosquitoes

- `Y`:

  density of infected adult mosquitoes

- `Z`:

  density of infectious adult mosquitoes

## Dynamical Terms

- `Lambda` or \\(\Lambda\\):

  adult mosquito emergence, supplied by
  [Emergence](https://dd-harp.github.io/ramp.xds/reference/Emergence.md)

- `kappa` or \\(\kappa\\):

  net infectiousness, supplied by
  [Transmission](https://dd-harp.github.io/ramp.xds/reference/Transmission.md)

## Parameters

- `f`:

  blood feeding rate

- `q`:

  human blood fraction

- `eip`:

  extrinsic incubation period (time steps)

- `g`:

  mosquito mortality rate

- `sigma`:

  emigration rate

- `mu`:

  emigration loss rate

- `K`:

  mosquito dispersal matrix

- `Omega`:

  adult mosquito demographic matrix (mortality + migration)

- `Upsilon`:

  survival and dispersal through the eip

## DTS

Update_MYt.RMdts updates `M`, `P`, `U`, the cohort matrix `Y`, and `Z`.
It uses the stored `Omega` as a one-step matrix and `G` to weight
progression out of the infected cohorts. Cohort columns are merged and
replaced after computing survival and progression.

## See also

[RMdts](https://dd-harp.github.io/ramp.xds/reference/RMdts.md)
