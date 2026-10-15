# Set up a blood_search_weights

If an optionStrata list is passed as the first argument, then set

- `BSWname = name$name`

- `optionStrata = name` and call
  `setup_blood_search_weights(BSWname, xds_obj, optionStrata, i)`

## Usage

``` r
# S3 method for class 'list'
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

a numeric vector
