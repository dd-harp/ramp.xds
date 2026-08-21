# Setup K_matrix

If the options list is passed as the first argument, the set

- `Kname = name$name`

- `options = name` and call `setup_K_matrix(Kname, xds_obj, options, s)`

## Usage

``` r
# S3 method for class 'list'
setup_K_matrix(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  a method name: or a matrix, or a list

- xds_obj:

  an **`xds`** model object

- options:

  a list of options to configure K_matrix

- s:

  the vector species index

## Value

a [matrix](https://rdrr.io/r/base/matrix.html)
