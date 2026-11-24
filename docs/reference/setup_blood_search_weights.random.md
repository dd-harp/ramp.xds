# Set up random blood search weights

Generate random blood search weights using
[make_blood_search_weights_random](https://dd-harp.github.io/ramp.xds/reference/make_blood_search_weights_random.md).

## Usage

``` r
# S3 method for class 'random'
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
