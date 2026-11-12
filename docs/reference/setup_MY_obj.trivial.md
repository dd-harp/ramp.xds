# Set up `trivial` (**MY**)

Set up the trivial adult mosquito model. In general, this should be used
for aquatic mosquito ecology or human / host epidemiology. In the former
case, the user configures `F_eggs`; in the latter, `F_fqZ`

## Usage

``` r
# S3 method for class 'trivial'
setup_MY_obj(MYname, xds_obj, options = list(), s = 1)
```

## Arguments

- MYname:

  the name of the model

- xds_obj:

  an **`xds`** model object

- options:

  a [list](https://rdrr.io/r/base/list.html)

- s:

  the vector species index

## Value

an **`xds`** object
