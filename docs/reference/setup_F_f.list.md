# Set up blood feeding rates

If an options list is passed as the first argument, use its `name` field
to select the setup method.

- `Ff_name = name$name`

- `options = name` and call `setup_F_f(Ff_name, xds_obj, options, s)`.

## Usage

``` r
# S3 method for class 'list'
setup_F_f(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  a or setup function name

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- s:

  the vector species index

## Value

an **`xds`** object
