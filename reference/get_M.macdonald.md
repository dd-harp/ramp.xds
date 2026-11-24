# Get the initial adult mosquito density for `macdonald` (**MY**)

Return the initial adult mosquito density per patch.

## Usage

``` r
# S3 method for class 'macdonald'
get_M(xds_obj, s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes

- `Y`:

  density of infected adult mosquitoes

- `Z`:

  density of infectious adult mosquitoes

## See also

[macdonald](https://dd-harp.github.io/ramp.xds/reference/macdonald.md),
[get_MY_inits](https://dd-harp.github.io/ramp.xds/reference/get_MY_inits.md)
