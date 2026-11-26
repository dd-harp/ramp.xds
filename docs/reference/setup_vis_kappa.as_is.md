# Set up visitor net infectiousness

- `vis_kappa` or \\(\kappa_V\\):

  the net infectiousness of visitors to mosquitoes

Visitor infectiousness represents the probability that a mosquito
becomes infected after blood feeding on a visitor.

## Usage

``` r
# S3 method for class 'as_is'
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

## Details

Pass a pre-configured vis_kappa. If it passes the checks, it replaces
the current vis_kappa.

If called with `name = "as_is"`, the vis_kappa must be at
`options$vis_kappa_asis`.

## See also

[visitor_infectiousness](https://dd-harp.github.io/ramp.xds/reference/visitor_infectiousness.md),
[get_vis_kappa](https://dd-harp.github.io/ramp.xds/reference/get_vis_kappa.md),
[change_vis_kappa](https://dd-harp.github.io/ramp.xds/reference/change_vis_kappa.md),
[malaria_importation](https://dd-harp.github.io/ramp.xds/reference/malaria_importation.md),
[update_vis_kappa](https://dd-harp.github.io/ramp.xds/reference/update_vis_kappa.md)
