# Get the initial mosquito population density

Get the initial mosquito population density

## Usage

``` r
# S3 method for class 'basicM'
get_M(xds_obj, s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

the initial density as a [numeric](https://rdrr.io/r/base/numeric.html)
vector

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes: those that have blood-fed at least
  once

## See also

[basicM](https://dd-harp.github.io/ramp.xds/reference/basicM.md),
[get_MY_inits](https://dd-harp.github.io/ramp.xds/reference/get_MY_inits.md)
