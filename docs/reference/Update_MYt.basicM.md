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

  time

- y:

  dependent variables

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

## DTS

Adult mosquitoes emerge from aquatic habitats at rate \\(\Lambda\\) and
disperse and die according to \\(\Omega\\). The one-day update in
Update_MYt.basicM is \$\$ \begin{array}{rl} M_t &=
\Lambda+e^{-\Omega}\cdot M\_{t-1} \\\\\\\\ P_t &=
f(M\_{t-1}-P\_{t-1})+e^{-\Omega}\cdot P\_{t-1} \end{array}\$\$ Here,
\\(P\\) is the density of mosquitoes that have blood-fed at least once.
Parity, \\(P/M\\), is computed during parsing and stored as `parous`.

## See also

[basicM](https://dd-harp.github.io/ramp.xds/reference/basicM.md),
[dMYdt.basicM](https://dd-harp.github.io/ramp.xds/reference/dMYdt.basicM.md)
