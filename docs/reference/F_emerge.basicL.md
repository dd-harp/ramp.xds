# Compute adult mosquito emergence for `basicL` (**L**)

Return adult female mosquito emergence per habitat.

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

[basicL](https://dd-harp.github.io/ramp.xds/reference/basicL.md),
[dLdt.basicL](https://dd-harp.github.io/ramp.xds/reference/dLdt.basicL.md),
[Emergence](https://dd-harp.github.io/ramp.xds/reference/Emergence.md)
