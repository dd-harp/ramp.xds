# Set up visitor net infectiousness

Implements the "random" case for
[setup_vis_kappa](https://dd-harp.github.io/ramp.xds/reference/setup_vis_kappa.md).
See
[make_vis_kappa_random](https://dd-harp.github.io/ramp.xds/reference/make_vis_kappa_random.md).

## Usage

``` r
# S3 method for class 'random'
setup_vis_kappa(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  a numeric vector, a setup function name, or an options list

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- s:

  the vector species index

## Value

an **`xds`** model object
