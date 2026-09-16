# Set up a habitat_search_weights

Pass a pre-configured habitat_search_weights. If it passes the checks,
it replaces the current habitat_search_weights.

If called with `name = "as_is"`, the habitat_search_weights must be at
`options$habitat_search_weights`

## Usage

``` r
# S3 method for class 'as_is'
setup_habitat_search_weights(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  a or setup function name

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- s:

  the mosquito species index

## Value

an **`xds`** object
