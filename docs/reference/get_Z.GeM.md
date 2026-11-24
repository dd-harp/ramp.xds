# Get the initial infective mosquito density for `GeM` (**MY**)

Return the initial infective mosquito density per patch.

## Usage

``` r
# S3 method for class 'GeM'
get_Z(xds_obj, s = 1)
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

[GeM](https://dd-harp.github.io/ramp.xds/reference/GeM.md),
[get_MY_inits](https://dd-harp.github.io/ramp.xds/reference/get_MY_inits.md)
