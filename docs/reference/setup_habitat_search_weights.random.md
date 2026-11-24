# Set up habitat_search_weights

Generate random habitat search weights using
[make_habitat_search_weights_random](https://dd-harp.github.io/ramp.xds/reference/make_habitat_search_weights_random.md)
and apply them using
[change_habitat_search_weights](https://dd-harp.github.io/ramp.xds/reference/change_habitat_search_weights.md).

## Usage

``` r
# S3 method for class 'random'
setup_habitat_search_weights(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  a setup function name

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- s:

  the mosquito species index

## Value

an **`xds`** model object
