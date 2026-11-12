# Set up `basicL` (**L**)

The function sets up `L_obj` for the \\(s^{th}\\) species by calling
[make_L_obj_basicL](https://dd-harp.github.io/ramp.xds/reference/make_L_obj_basicL.md)

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

## See also

[make_L_obj_basicL](https://dd-harp.github.io/ramp.xds/reference/make_L_obj_basicL.md)
