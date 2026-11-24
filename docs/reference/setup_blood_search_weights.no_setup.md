# Leave blood search weights unchanged

Return `xds_obj` without changing the configured search weights.

## Usage

``` r
# S3 method for class 'no_setup'
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
