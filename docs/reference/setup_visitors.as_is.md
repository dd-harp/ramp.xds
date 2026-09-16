# Set up a visitors

Pass a pre-configured visitors. If it passes the checks, it replaces the
current visitors.

If called with `name = "as_is"`, the visitors must be at
`options$visitors`

## Usage

``` r
# S3 method for class 'as_is'
setup_visitors(name, xds_obj, options = list(), s = 1)
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
