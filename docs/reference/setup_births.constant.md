# Set up birth rates

Pass pre-configured constant birth rates. If it passes the checks, it
replaces the current birth rates.

If called with `name = "as_is"`, the births must be at `options$births`

## Usage

``` r
# S3 method for class 'constant'
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
