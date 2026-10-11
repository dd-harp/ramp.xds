# Set up a births

If an options list is passed as the first argument, then set

- `birth_ratesname = name$name`

- `options = name` and call
  `setup_births(birth_ratesname, xds_obj, options, s)`

## Usage

``` r
# S3 method for class 'list'
setup_births(name, xds_obj, options = list(), i = 1)
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

an **`xds`** object
