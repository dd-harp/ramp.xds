# Set mosquito bionomics to baseline

Implements
[MBaseline](https://dd-harp.github.io/ramp.xds/reference/MBaseline.md)
for models with no forcing on the baseline

## Usage

``` r
# S3 method for class 'basicM'
MBaseline(t, y, xds_obj, s)
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

the model as a [list](https://rdrr.io/r/base/list.html)
