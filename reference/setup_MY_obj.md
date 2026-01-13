# Setup an **MY** Model Object

This function is a structured interface, a way of building a model
objects describing adult mosquito ecology & infection dynamics. The
string *MYname* is assigned its own class (`class(MYname) <- MYname`).

Since each model will require different arguments, the dispatched
function `setup_MY_obj.MYname` calls `make_MY_obj_MYname,` passing the
`options` to set the values of parameters or terms.

## Usage

``` r
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

[list](https://rdrr.io/r/base/list.html)
