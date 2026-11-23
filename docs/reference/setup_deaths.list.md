# Set up deaths

If an options list is passed as the first argument, then set

- `deaths_name = name$name`

- `options = name` and call
  `setup_deaths(deaths_name, xds_obj, options, i)`.

## Usage

``` r
# S3 method for class 'list'
setup_deaths(name, xds_obj, options = list(), i = 1)
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
