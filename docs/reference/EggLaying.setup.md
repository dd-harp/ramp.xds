# Compute eggs laid, the first time

In autonomous models, this function gets called after the `ML_interface`
is set up, or after any structural element in the ML_interface changes

describing host availability (\\Q\\) and the egg distribution matrix
(\\O\\).

## Usage

``` r
# S3 method for class 'setup'
EggLaying(t, y, xds_obj)
```

## Arguments

- t:

  the time

- y:

  the state variables

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** model object

## Details

If conditions are time, invariant, then
[EggLaying.static](https://dd-harp.github.io/ramp.xds/reference/EggLaying.static.md)
computes eggs laid. The functions that compute \\N\\ and \\O\\ are
called *once* after setup. The class of `ML_interface` is set to
`static` and the function is never called again. If any parameters are
changed that would affect egg laying, then the class of `ML_interface`
should get reset to `setup` to reconfigure \\N\\ and \\O\\.

## See also

For \\Q\\, see
[compute_Qall](https://dd-harp.github.io/ramp.xds/reference/compute_Qall.md)

For \\N\\, see
[make_habitat_matrix](https://dd-harp.github.io/ramp.xds/reference/make_habitat_matrix.md)
