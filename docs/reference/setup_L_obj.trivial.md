# Set up `trivial` (**L**)

This sets up a `trivial` model object for the **L** module for the
\\(s^{th}\\) vector species

- the object is created by
  [make_L_obj_trivial](https://dd-harp.github.io/ramp.xds/reference/make_L_obj_trivial.md)

- `options` is passed to override the defaults

There are no dynamical variables requiring initial values. This method
sets `class(xds_obj$forced_by) = "Lambda"`.

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

## Note

The module object is stored at `xds_obj$L_obj[[s]]`

## See also

[trivial_L](https://dd-harp.github.io/ramp.xds/reference/trivial_L.md),
[make_L_obj_trivial](https://dd-harp.github.io/ramp.xds/reference/make_L_obj_trivial.md)
