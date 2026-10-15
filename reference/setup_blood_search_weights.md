# Set up blood search weights

Advanced setup for heterogeneous biting. Useful methods include:

- "as_is" changes the values to a vector of values

- "random" sets random search weights (see
  [make_blood_search_weights_random](https://dd-harp.github.io/ramp.xds/reference/make_blood_search_weights_random.md))

## Usage

``` r
setup_blood_search_weights(name, xds_obj, optionStrata = list(), i = 1)
```

## Arguments

- name:

  a or setup function name

- xds_obj:

  an **`xds`** model object

- optionStrata:

  configuration optionStrata

- i:

  the host species index

## Value

an **`xds`** object
