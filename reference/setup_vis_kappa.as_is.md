# Set up a vis_kappa

Pass a pre-configured vis_kappa. If it passes the checks, it replaces
the current vis_kappa.

If called with `name = "as_is"`, the vis_kappa must be at
`options$vis_kappa`

## Usage

``` r
# S3 method for class 'as_is'
setup_vis_kappa(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  a or setup function name

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- s:

  the host species index

## Value

an **`xds`** object
