# Set up a blood_search_weights

Pass a pre-configured blood_search_weights. If it passes the checks, it
replaces the current blood_search_weights.

If the first argument to `setup_blood_search_weights` is a numeric
vector, the `"as_is"` method is used.

The conventional way to call this method is `name = "as_is"`, and the
new blood search weights are passed as
`optionStrata$blood_search_weights`

## Usage

``` r
# S3 method for class 'as_is'
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
