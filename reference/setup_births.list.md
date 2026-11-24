# Set up births

Set up births using an options list.

## Usage

``` r
# S3 method for class 'list'
setup_births(name, xds_obj, options = list(), i = 1)
```

## Arguments

- name:

  a numeric vector, a setup function name, or an options list

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- i:

  the host species index

## Value

an **`xds`** model object

## Note

If an options list is passed as the first argument, then set

- `births_name = name$name`

- `options = name` and call
  `setup_births(births_name, xds_obj, options, i)`.
