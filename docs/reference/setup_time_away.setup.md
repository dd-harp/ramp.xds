# Set up a time spent

Pass a pre-configured time spent . If it passes the checks, it replaces
the current time_away .

If called with `name = "as_is"`, the time spent must be at
`options$time_away`

## Usage

``` r
# S3 method for class 'setup'
setup_time_away(name, xds_obj, options = list(), i = 1)
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
