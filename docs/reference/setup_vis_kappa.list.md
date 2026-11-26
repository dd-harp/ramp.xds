# Set up visitor net infectiousness

- `vis_kappa` or \\(\kappa_V\\):

  the net infectiousness of visitors to mosquitoes

Visitor infectiousness represents the probability that a mosquito
becomes infected after blood feeding on a visitor.

## Usage

``` r
# S3 method for class 'list'
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

Set up the net infectiousness of visitors using an options list.

## Note

If an options list is passed as the first argument, then set

- `Vname = name$name`

- `options = name` and call
  `setup_vis_kappa(Vname, xds_obj, options, s)`.

## See also

[visitor_infectiousness](https://dd-harp.github.io/ramp.xds/reference/visitor_infectiousness.md),
[get_vis_kappa](https://dd-harp.github.io/ramp.xds/reference/get_vis_kappa.md),
[change_vis_kappa](https://dd-harp.github.io/ramp.xds/reference/change_vis_kappa.md),
[malaria_importation](https://dd-harp.github.io/ramp.xds/reference/malaria_importation.md),
[update_vis_kappa](https://dd-harp.github.io/ramp.xds/reference/update_vis_kappa.md)
