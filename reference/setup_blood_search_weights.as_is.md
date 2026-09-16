# Set up a blood_search_weights

Pass a pre-configured blood_search_weights. If it passes the checks, it
replaces the current blood_search_weights.

If called with `name = "as_is"`, the blood_search_weights must be at
`options$blood_search_weights`

## Usage

``` r
# S3 method for class 'as_is'
setup_blood_search_weights(name, xds_obj, options = list(), i = 1)
```

## Arguments

- name:

  a or setup function name

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- i:

  the host species index

## Value

an **`xds`** object
