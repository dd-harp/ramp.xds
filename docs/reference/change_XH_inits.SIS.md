# `SIS` - Change Initial Values

For the **XH** module, `SIS,` change the initial value(s) for the
variable \\I\\.

The argument passed as `options` should be a named list, and the new
initial value(s) are set to `options$I`

## Usage

``` r
# S3 method for class 'SIS'
change_XH_inits(xds_obj, i = 1, options = list())
```

## Arguments

- xds_obj:

  an **`xds`** model object

- i:

  the vector species index

- options:

  a named list

## Value

an **`xds`** object

## Note

Initial values for human population size, the variable \\H\\, must be
set using
[change_H](https://dd-harp.github.io/ramp.xds/reference/change_H.md)

## See also

The variable names are returned by calling
[get_XH_inits](https://dd-harp.github.io/ramp.xds/reference/get_XH_inits.md).
The function
[make_XH_inits_SIS](https://dd-harp.github.io/ramp.xds/reference/make_XH_inits_SIS.md)
is called during setup.
