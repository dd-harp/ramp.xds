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

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector

## DTS

**Ecology**

Adult mosquitoes emerge from aquatic habitats at rate \\(\Lambda\\) and
disperse and die according to \\(\Omega\\). The one-day update in
Update_MYt.SI is: \$\$M_t = \Lambda + e^{-\Omega}\cdot M\_{t-1}\$\$ The
model also computes the density of parous adult mosquitoes: those that
have blood fed at least once: \$\$P_t =
e^{-\Omega}\cdot\left\[(1-e^{-fq})(M\_{t-1}-P\_{t-1}) +
P\_{t-1}\right\]\$\$ Parity is computed during parsing as \\(P/M\\) and
its orbit is stored as `parity`.

**Infection Dynamics**

Given the per-capita daily rate of human blood feeding \\((fq)\\) and
net infectiousness \\((\kappa)\\), the FoI for mosquitoes is
\\(fq\kappa\\). The density of infected mosquitoes is updated by:
\$\$Y_t = e^{-\Omega}\cdot\left\[(1-e^{-fq\kappa})(M\_{t-1}-Y\_{t-1}) +
Y\_{t-1}\right\]\$\$

## See also

[SI](https://dd-harp.github.io/ramp.xds/reference/SI.md),
[dMYdt.SI](https://dd-harp.github.io/ramp.xds/reference/dMYdt.SI.md)
