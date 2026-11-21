# Set up deaths

Pass pre-configured deaths. If they pass the checks, they replace the
current deaths.

If called with `name = "as_is"`, the deaths must be at
`options$death_rates`.

## Usage

``` r
# S3 method for class 'as_is'
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
