# Set up static mosquito mortality rate (**MY**)

Set up supplied static mosquito mortality rates from `options$g` or a
numeric `name`. Values in `options$g` take precedence.

## Usage

``` r
# S3 method for class 'as_is'
setup_F_g(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  a mosquito mortality rate or setup method name

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- s:

  the vector species index

## Value

an **`xds`** model object
