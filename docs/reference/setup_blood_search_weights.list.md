# Set up blood search weights

If an options list is passed as the first argument, then set

- `BSWname = name$name`

- `options = name` and call
  `setup_blood_search_weights(BSWname, xds_obj, options, i)`

## Usage

``` r
# S3 method for class 'list'
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
