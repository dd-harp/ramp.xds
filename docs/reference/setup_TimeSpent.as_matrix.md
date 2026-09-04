# Set up a time spent matrix

Pass a pre-configured time spent matrix. If it passes the checks, it
replaces the current timespent matrix.

If called with `name = "as_matrix"`, the time spent matrix must be at
`options$timespent_matrix`

## Usage

``` r
# S3 method for class 'as_matrix'
setup_timespent(name, xds_obj, options = list(), i = 1)
```

## Arguments

- name:

  a matrix or setup function name

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- i:

  the host species index

## Value

an **`xds`** object
