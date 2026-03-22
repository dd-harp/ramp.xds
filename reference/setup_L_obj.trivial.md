# Set up `trivial` (**L**)

Call
[make_L_obj_trivial](https://dd-harp.github.io/ramp.xds/reference/make_L_obj_trivial.md)
and set `class(xds_obj$forced_by) = "Lambda"`

## Usage

``` r
# S3 method for class 'trivial'
setup_L_obj(Lname, xds_obj, s, options = list())
```

## Arguments

- Lname:

  the class name of the **L** module

- xds_obj:

  an **`xds`** model object

- s:

  the species index

- options:

  a named list to configure **`L_obj`**

## Value

an **`xds`** object
