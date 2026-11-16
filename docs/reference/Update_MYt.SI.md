# Update state variables for `SI` (**MY**)

Implements
[Update_MYt](https://dd-harp.github.io/ramp.xds/reference/Update_MYt.md)
for the SI model.

## Usage

``` r
# S3 method for class 'SI'
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

## Dynamics

**Ecology**

Adult mosquitoes emerge from aquatic habitats \\((\Lambda)\\) and
disperse and die (\\(\Omega\\)): \$\$ dM/dt = \Lambda - \Omega \cdot M
\$\$ The model also computes the density of parous adult mosquitoes:
those that have blood fed at least once: \$\$ dP/dt = fq(M-P) - \Omega
\cdot P \\\\ \$\$ Parity is computed during parsing as \\(P/M\\) and its
orbit is stored as `parity.`

**Infection Dynamics**

Given the per-capita daily rate of human blood feeding \\((fq)\\) and
net infectiousness \\((\kappa)\\), the FoI for mosquitoes is
\\(fq\kappa\\). Changes in the density of infected mosquitoes is given
by: \$\$ dY/dt = fq\kappa(M-Y) - \Omega \cdot Y \\\\ \$\$

## See also

[SI](https://dd-harp.github.io/ramp.xds/reference/SI.md),
[dMYdt.SI](https://dd-harp.github.io/ramp.xds/reference/dMYdt.SI.md)
