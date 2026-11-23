# Compute emergent adults for `basicL` (**L**)

Compute the emergence rate of adult female mosquitoes from each habitat.

## Usage

``` r
# S3 method for class 'basicL'
F_emerge(t, y, xds_obj, s)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nHabitats`

## Emergence

The emergence rate of adult female mosquitoes from each habitat is
computed by F_emerge.basicL: \$\$\alpha = \psi e^{-\xi L} L\$\$

## See also

[dLdt.basicL](https://dd-harp.github.io/ramp.xds/reference/dLdt.basicL.md)
