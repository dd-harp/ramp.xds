# Set up F_mu (**MY**)

Set up emigration-loss fractions using an options list.

## Usage

``` r
# S3 method for class 'list'
setup_F_mu(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  an emigration-loss fraction or setup method name

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- s:

  the vector species index

## Value

an **`xds`** model object

## Note

If an options list is passed as the first argument, use its `name` field
to select the setup method. If `name` is NULL, use the `no_setup`
method. Pass the list as `options` to
[setup_F_mu](https://dd-harp.github.io/ramp.xds/reference/setup_F_mu.md).
