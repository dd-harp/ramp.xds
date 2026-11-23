# Set up static EIP values (**MY**)

Set up supplied static extrinsic incubation periods from `options$eip`
or a numeric `name`. Values in `options$eip` take precedence.

## Usage

``` r
# S3 method for class 'as_is'
setup_F_eip(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  an EIP value or setup method name

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- s:

  the vector species index

## Value

an **`xds`** model object
