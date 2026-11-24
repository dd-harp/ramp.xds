# Setup a Kernel-Based Mosquito Dispersal Matrix

Set up a mosquito dispersal matrix from a set of \\(x,y\\) coordinates
and a *kernel,* a function that assigns weights by distance. The
fractions leaving from each patch that arrive at other patches form the
vector of normalized weights.

## Usage

``` r
# S3 method for class 'xy'
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

an **`xds`** model object
