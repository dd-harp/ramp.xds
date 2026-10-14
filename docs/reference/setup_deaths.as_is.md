# Set up a deaths

Pass a pre-configured deaths . If it passes the checks, it replaces the
current deaths .

If called with `name = "as_is"`, the deaths must be at `options$deaths`

## Usage

``` r
# S3 method for class 'as_is'
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
