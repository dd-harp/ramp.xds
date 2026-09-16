# Set up a habitat_search_weights

If an options list is passed as the first argument, then set

- `HSWname = name$name`

- `options = name` and call
  `setup_habitat_search_weights(HSWname, xds_obj, options, s)`

## Usage

``` r
# S3 method for class 'list'
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

a numeric vector
