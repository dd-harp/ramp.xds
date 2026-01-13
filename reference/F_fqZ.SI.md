# Net Blood Feeding by Infectious Mosquitoes - `SI` Mosquito Model

The variable \\Y\\ is the density of *infected* mosquitoes. The model
blood feeding **parameters** are:

- \\f\\ is the overall blood feeding rate

- \\q\\ is the human fraction for blood feeding

- \\\Omega\\ is the demographic matrix

- \\\tau\\ is the EIP

The net blood feeding rate by infectious mosquitoes is computed by
accounting for survival and dispersal that would occur in a delay
differential equation: \$\$\Upsilon = e^{-\Omega \tau}\$\$ so \$\$fqZ =
fq (\Upsilon \cdot Y)\$\$ The daily EIR for the population strata is
\\\beta \cdot fqZ\\

## Usage

``` r
# S3 method for class 'SI'
F_fqZ(t, y, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`
