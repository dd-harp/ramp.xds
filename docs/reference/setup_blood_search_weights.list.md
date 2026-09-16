# Set up a blood_search_weights

If an options list is passed as the first argument, then set

- `BSWname = name$name`

- `options = name` and call
  `setup_blood_search_weights(BSWname, xds_obj, options, s)`

## Usage

``` r
# S3 method for class 'list'
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

a numeric vector
