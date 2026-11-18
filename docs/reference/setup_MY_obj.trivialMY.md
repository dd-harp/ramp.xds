# Set up `trivialMY` (**MY**)

This sets up a `trivialMY` model object for the **MY** module for the
\\(s^{th}\\) vector species

- the object is created by
  [make_MY_obj_trivial](https://dd-harp.github.io/ramp.xds/reference/make_MY_obj_trivial.md)

- `options` is passed to override the defaults

In general, this should be used for aquatic mosquito ecology or human /
host epidemiology. In the former case, the user configures `F_eggs`; in
the latter, `F_fqZ`

This builds a trace function as a *composed time series function* using
either functions or parameters:

- Use a function if passed, and set the function object to an empty list

- If no function is passed, the parameters should be a function object
  (see
  [ramp.func::F_obj](https://dd-harp.github.io/ramp.func/reference/F_obj.html))

- Use `make_function` to create a function \\(F(t,V)\\)

## Usage

``` r
# S3 method for class 'trivialMY'
setup_MY_obj(MYname, xds_obj, options = list(), s = 1)

# S3 method for class 'trivial'
setup_MY_obj(MYname, xds_obj, options = list(), s = 1)
```

## Arguments

- MYname:

  the name of the model

- xds_obj:

  an **`xds`** model object

- options:

  a [list](https://rdrr.io/r/base/list.html)

- s:

  the vector species index

## Value

an **`xds`** model object

## Note

The module object is stored at `xds_obj$MY_obj[[s]]` The compatibility
name `"trivial"` creates the same `trivialMY` object.

## See also

[trivialMY](https://dd-harp.github.io/ramp.xds/reference/trivialMY.md),
[make_MY_obj_trivial](https://dd-harp.github.io/ramp.xds/reference/make_MY_obj_trivial.md)
