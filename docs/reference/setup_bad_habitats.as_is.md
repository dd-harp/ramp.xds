# Set up bad habitats

Pass a pre-configured bad_habitats. If it passes the checks, it replaces
the current bad_habitats.

If called with `name = "as_is"`, the bad_habitats must be at
`options$bad_habitats`

## Usage

``` r
# S3 method for class 'as_is'
setup_bad_habitats(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  a or setup function name

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- s:

  the vector species index

## Value

an **`xds`** object
