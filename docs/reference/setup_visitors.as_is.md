# Set up visitors

Pass pre-configured visitors. If it passes the checks, it replaces the
current visitors.

If called with `name = "as_is"`, the visitors must be at
`options$visitors_asis`.

## Usage

``` r
# S3 method for class 'as_is'
setup_visitors(name, xds_obj, options = list(), s = 1)
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
