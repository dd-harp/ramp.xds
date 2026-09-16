# The **XH** module skill set

The `hMoI` module is an example of a model that is not extensible. There
is no way to add either dynamic human population density or mass
treatment without violating the model assumptions.

The model does output observed population density and here it is

## Usage

``` r
# S3 method for class 'hMoI'
setup_skillset_XH(xds_obj, i)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- i:

  the species index

## Value

the **`xds`** object

## Note

This method dispatches on `class(xds_obj$XH_obj)`
