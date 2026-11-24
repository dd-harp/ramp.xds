# Set up blood feeding rates (**MY**)

If an options list is passed as the first argument, use its `name` field
to select the setup method. If `name` is NULL, use the `no_setup`
method. Pass the list as `options` to
[setup_F_f](https://dd-harp.github.io/ramp.xds/reference/setup_F_f.md).

## Usage

``` r
# S3 method for class 'list'
setup_F_f(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  a blood feeding rate or setup method name

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- s:

  the vector species index

## Value

an **`xds`** model object
