# Get the delayed maturation parameter for `basicL` (**L**)

Return the stored realized delayed maturation parameter per habitat.

## Usage

``` r
# S3 method for class 'basicL'
get_xi(xds_obj, s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nHabitats`

## Parameters

- `psi` or \\(\psi\\):

  maturation rate

- `xi` or \\(\xi\\):

  delayed maturation parameter in response to mean crowding

- `phi` or \\(\phi\\):

  density-independent mortality rate

- `theta` or \\(\theta\\):

  slope of density-dependent mortality

## See also

[basicL](https://dd-harp.github.io/ramp.xds/reference/basicL.md),
[change_xi](https://dd-harp.github.io/ramp.xds/reference/change_xi.md)
