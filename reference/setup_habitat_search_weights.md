# Set up habitat search weights

The advanced set up function for habitat search weights

Available methods are:

- `as_is` – user generated

- `random` – generate a random vector (see
  [make_habitat_search_weights_random](https://dd-harp.github.io/ramp.xds/reference/make_habitat_search_weights_random.md))

## Usage

``` r
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

## See also

[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md),
[xds_info_search_weights_habitat](https://dd-harp.github.io/ramp.xds/reference/xds_info_search_weights_habitat.md)
