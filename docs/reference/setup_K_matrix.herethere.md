# Setup a Here-There Dispersal Matrix

Implements
[setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)
for the herethere model: dispersal to every other patch, with equal
probability

## Usage

``` r
# S3 method for class 'herethere'
setup_K_matrix(Kname, xds_obj, options = list(), s = 1)
```

## Arguments

- Kname:

  a name, a matrix, or a list

- xds_obj:

  an **`xds`** model object

- options:

  a list of options to configure K_matrix

- s:

  the vector species index

## Value

an **`xds`** object
