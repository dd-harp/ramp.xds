# Set up the module object for `trivialL` (**L**)

This sets up a `trivialL` model object for the **L** module for the
\\(s^{th}\\) vector species

- the object is created by
  [make_L_obj_trivial](https://dd-harp.github.io/ramp.xds/reference/make_L_obj_trivial.md)

- `options` is passed to override the defaults

There are no dynamical variables requiring initial values. This method
sets `class(xds_obj$forced_by) = "Lambda"`.

## Usage

``` r
# S3 method for class 'trivialL'
setup_L_obj(Lname, membership, xds_obj, options = list(), s = 1)

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

an **`xds`** model object

## Note

The module object is stored at `xds_obj$L_obj[[s]]` The compatibility
name `"trivial"` creates the same `trivialL` object.

## See also

[trivialL](https://dd-harp.github.io/ramp.xds/reference/trivialL.md),
[make_L_obj_trivial](https://dd-harp.github.io/ramp.xds/reference/make_L_obj_trivial.md)
