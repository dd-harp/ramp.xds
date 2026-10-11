# Set up death rates

Pass pre-configured constant death rates. If it passes the checks, it
replaces the current death rates.

If called with `name = "as_is"`, the deaths must be at
\`options\$death_rates

## Usage

``` r
# S3 method for class 'zero'
setup_deaths(name, xds_obj, options = list(), i = 1)
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
