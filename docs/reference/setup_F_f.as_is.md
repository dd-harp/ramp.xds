# Set up static blood feeding rates (**MY**)

Set up supplied static blood feeding rates from `options$f` or a numeric
`name`. Values in `options$f` take precedence.

## Usage

``` r
# S3 method for class 'as_is'
setup_F_f(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  a blood feeding rate or setup method name

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- s:

  the vector species index

## Value

an **`xds`** model object
