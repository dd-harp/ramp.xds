# Set up `basicL` (**L**)

This sets up a `basicL` model object for the **L** module for the
\\(s^{th}\\) vector species

- the object is created by
  [make_L_obj_basicL](https://dd-harp.github.io/ramp.xds/reference/make_L_obj_basicL.md)

- the initial conditions are set by
  [setup_L_inits](https://dd-harp.github.io/ramp.xds/reference/setup_L_inits.md)

- `options` is passed to both to override the defaults

## Usage

``` r
# S3 method for class 'basicL'
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

[basicL](https://dd-harp.github.io/ramp.xds/reference/basicL.md),
[make_L_obj_basicL](https://dd-harp.github.io/ramp.xds/reference/make_L_obj_basicL.md)
