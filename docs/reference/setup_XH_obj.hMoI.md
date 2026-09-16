# Set up `hMoI` (**XH**)

Implements
[setup_XH_obj](https://dd-harp.github.io/ramp.xds/reference/setup_XH_obj.md)
for the hMoI model

## Usage

``` r
# S3 method for class 'hMoI'
setup_XH_obj(Xname, residence, HPop, xds_obj, i, options = list())
```

## Arguments

- Xname:

  the model name

- residence:

  the residence vector

- HPop:

  the initial human population size

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

- options:

  model options as a named list

## Value

a [list](https://rdrr.io/r/base/list.html) vector
