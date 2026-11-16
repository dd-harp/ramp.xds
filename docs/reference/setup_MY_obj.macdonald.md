# Set up `macdonald` (**MY**)

This sets up a `macdonald` model object for the **MY** module for the
\\(s^{th}\\) vector species

- the object is created by
  [make_MY_obj_macdonald](https://dd-harp.github.io/ramp.xds/reference/make_MY_obj_macdonald.md)

- the initial conditions are set by
  [setup_MY_inits](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.md)

- `options` is passed to both to override the defaults

## Usage

``` r
# S3 method for class 'macdonald'
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

an **`xds`** object

## Note

The module object is stored at `xds_obj$MY_obj[[s]]`

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes

- `Y`:

  density of infected adult mosquitoes

- `Z`:

  density of infectious adult mosquitoes

## Parameters

- `f`:

  blood feeding rate — use
  [setup_f](https://dd-harp.github.io/ramp.xds/reference/setup_f.md) or
  [setup_blood_feeding_rates](https://dd-harp.github.io/ramp.xds/reference/setup_f.md)

- `q`:

  human fraction — use
  [setup_q](https://dd-harp.github.io/ramp.xds/reference/setup_q.md) or
  [setup_human_fractions](https://dd-harp.github.io/ramp.xds/reference/setup_q.md)

- `g`:

  mosquito mortality rate — use
  [setup_g](https://dd-harp.github.io/ramp.xds/reference/setup_g.md) or
  [setup_mosquito_mortality](https://dd-harp.github.io/ramp.xds/reference/setup_g.md)

- `eip` or \\(\tau\\):

  extrinsic incubation period — use
  [setup_eip](https://dd-harp.github.io/ramp.xds/reference/setup_eip.md)

- `sigma` or \\(\sigma\\):

  emigration rate — use
  [setup_sigma](https://dd-harp.github.io/ramp.xds/reference/setup_sigma.md)
  or
  [setup_emigration_rates](https://dd-harp.github.io/ramp.xds/reference/setup_sigma.md)

- `mu` or \\(\mu\\):

  emigration loss fraction — use
  [setup_mu](https://dd-harp.github.io/ramp.xds/reference/setup_mu.md)
  or
  [setup_emigration_loss](https://dd-harp.github.io/ramp.xds/reference/setup_mu.md)

- `K` or `K_matrix`:

  mosquito dispersal matrix — use
  [setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)

- `nu` or \\(\nu\\):

  egg batch rate — use
  [setup_nu](https://dd-harp.github.io/ramp.xds/reference/setup_nu.md)
  or
  [setup_egg_batch_rates](https://dd-harp.github.io/ramp.xds/reference/setup_nu.md)

- `eggsPerBatch` or \\(\Gamma\\):

  number of eggs per batch

After basic setup,
[change_MY_pars.macdonald](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.macdonald.md)
can be used to change static parameter values. This module does not
support dynamic bionomic ports.

## See also

[macdonald](https://dd-harp.github.io/ramp.xds/reference/macdonald.md),
[make_MY_obj_macdonald](https://dd-harp.github.io/ramp.xds/reference/make_MY_obj_macdonald.md),
[setup_MY_inits.macdonald](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.macdonald.md)
