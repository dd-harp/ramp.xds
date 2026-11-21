# Set up birth rates

Pass pre-configured constant birth rates. If they pass the checks, they
replace the current birth rates.

The birth rates must be at `options$birth_rates`.

## Usage

``` r
# S3 method for class 'constant'
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
