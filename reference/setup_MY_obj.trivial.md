# Setup the trivial model for an adult mosquito model

Set up the trivial adult mosquito model. In general, this should be used
for aquatic mosquito ecology or human / host epidemiology. In the former
case, the user configures `F_eggs`; in the latter, `F_fqZ`

## Usage

``` r
# S3 method for class 'trivial'
setup_MY_obj(MYname, xds_obj, s, options = list())
```

## Arguments

- MYname:

  the name of the model

- xds_obj:

  an **`xds`** model object

- s:

  the species index

- options:

  a [list](https://rdrr.io/r/base/list.html)

## Value

a modified **`xds`** model object
