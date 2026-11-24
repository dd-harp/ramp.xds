# Set up visitor net infectiousness

Pass a pre-configured vis_kappa. If it passes the checks, it replaces
the current vis_kappa.

If called with `name = "as_is"`, the vis_kappa must be at
`options$vis_kappa_asis`.

## Usage

``` r
# S3 method for class 'as_is'
setup_vis_kappa(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  a numeric vector, a setup function name, or an options list

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- s:

  the vector species index

## Value

an **`xds`** model object
