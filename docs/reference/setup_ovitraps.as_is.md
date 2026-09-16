# Set up ovitraps

Pass a pre-configured ovitraps. If it passes the checks, it replaces the
current ovitraps.

If called with `name = "as_is"`, the ovitraps must be at
`options$ovitraps`

## Usage

``` r
# S3 method for class 'as_is'
setup_ovitraps(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  a setup function name

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- s:

  the vector species index

## Value

an **`xds`** object
