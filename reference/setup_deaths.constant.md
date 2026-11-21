# Set up death rates

Pass pre-configured constant death rates. If they pass the checks, they
replace the current death rates.

The death rates must be at `options$death_rates`.

## Usage

``` r
# S3 method for class 'constant'
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
