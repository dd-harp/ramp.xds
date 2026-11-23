# Steady states: MY-SI

Compute steady-state mosquito densities for the `SI` module.

## Usage

``` r
# S3 method for class 'xde'
steady_state_MY_SI(Lambda, kappa, xds_obj, s = 1)

# S3 method for class 'SI_ode'
steady_state_MY(Lambda, kappa, xds_obj, s = 1)
```

## Arguments

- Lambda:

  the daily emergence rate of adult mosquitoes

- kappa:

  net infectiousness

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [list](https://rdrr.io/r/base/list.html)

## ODE

**Ecology**

Adult mosquitoes emerge from aquatic habitats at rate \\(\Lambda\\) and
disperse and die according to \\(\Omega\\): \$\$ dM/dt = \Lambda -
\Omega \cdot M \$\$ The model also computes the density of parous adult
mosquitoes: those that have blood fed at least once: \$\$ dP/dt =
fq(M-P) - \Omega \cdot P \\\\ \$\$ Parity is computed during parsing as
\\(P/M\\) and its orbit is stored as `parity`.

**Infection Dynamics**

Given the per-capita daily rate of human blood feeding \\((fq)\\) and
net infectiousness \\((\kappa)\\), the FoI for mosquitoes is
\\(fq\kappa\\). Changes in the density of infected mosquitoes are given
by: \$\$ dY/dt = fq\kappa(M-Y) - \Omega \cdot Y \\\\ \$\$

## See also

[SI](https://dd-harp.github.io/ramp.xds/reference/SI.md),
[dMYdt.SI](https://dd-harp.github.io/ramp.xds/reference/dMYdt.SI.md)
