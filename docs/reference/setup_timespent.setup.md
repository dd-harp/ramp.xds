# Set up the default time spent matrix

Store residence on the selected host module, initialize its time spent
matrix from the residence matrix, and initialize a static port. Read the
residence vector from `options$residence` and remove the temporary
top-level `xds_obj$residence`.

## Usage

``` r
# S3 method for class 'setup'
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

an **`xds`** model object
