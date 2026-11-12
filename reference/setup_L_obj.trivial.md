# Set up `trivial` (**L**)

Call
[make_L_obj_trivial](https://dd-harp.github.io/ramp.xds/reference/make_L_obj_trivial.md)
and set `class(xds_obj$forced_by) = "Lambda"`

## Usage

``` r
# S3 method for class 'trivial'
setup_L_obj(Lname, membership, xds_obj, options = list(), s = 1)
```

## Arguments

- Lname:

  the class name of the **L** module

- membership:

  the habitats' patch membership vector

- xds_obj:

  an **`xds`** model object

- options:

  a named list to configure **`L_obj`**

- s:

  the species index

## Value

an **`xds`** object
