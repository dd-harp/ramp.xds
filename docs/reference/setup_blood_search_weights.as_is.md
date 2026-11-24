# Set up blood search weights from supplied values

Pass a pre-configured blood_search_weights. If it passes the checks, it
replaces the current blood_search_weights.

If the first argument to `setup_blood_search_weights` is a numeric
vector, the `"as_is"` method is used.

When `name = "as_is"`, pass the new vector in `options$weights`.

## Usage

``` r
# S3 method for class 'as_is'
setup_blood_search_weights(name, xds_obj, options = list(), i = 1)
```

## Arguments

- name:

  a setup method name or a numeric vector of weights

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- i:

  the host species index

## Value

an **`xds`** model object
