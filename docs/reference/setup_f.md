# Set up constant blood feeding rate

Configure and validate models with constant blood feeding rates. This
calls
[setup_F_f](https://dd-harp.github.io/ramp.xds/reference/setup_F_f.md)
to configure a function \\F_f\\, and call
[change_f](https://dd-harp.github.io/ramp.xds/reference/change_f.md).

## Usage

``` r
setup_f(name, xds_obj, options = list(), s = 1)
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

## Note

Use
[setup_F_f](https://dd-harp.github.io/ramp.xds/reference/setup_F_f.md)
and not setup_f if the baseline values of \\f\\ will vary over time.

## See also

[setup_F_f](https://dd-harp.github.io/ramp.xds/reference/setup_F_f.md)
