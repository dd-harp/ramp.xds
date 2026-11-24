# Change parameters for `SI` (**MY**)

Change parameter values named in `options` for the `SI` module, leaving
other values unchanged. Refresh baseline bionomics at time zero using
[MBionomics](https://dd-harp.github.io/ramp.xds/reference/MBionomics.md),
without calling
[MEffectSizes](https://dd-harp.github.io/ramp.xds/reference/MEffectSizes.md).
This method changes static parameter values. To change `K_matrix`, use
[setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md);
this method does not change the dispersal matrix.

## Usage

``` r
# S3 method for class 'SI'
change_MY_pars(xds_obj, options = list(), s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- options:

  a named list

- s:

  the vector species index

## Value

an **`xds`** model object

## Note

This is designed to work for static parameters or for parameters that
are set up as ports, where the class of the bionomic object is "static."

Use advanced setup to modify dynamic bionomic ports.

## Parameters

- `f`:

  blood feeding rates — use
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
  [setup_mosy_emigration](https://dd-harp.github.io/ramp.xds/reference/setup_sigma.md)

- `mu` or \\(\mu\\):

  emigration loss fraction — use
  [setup_mu](https://dd-harp.github.io/ramp.xds/reference/setup_mu.md)
  or
  [setup_emigration_loss](https://dd-harp.github.io/ramp.xds/reference/setup_mu.md)

- `K`:

  mosquito dispersal matrix — use
  [setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)

- `nu` or \\(\nu\\):

  the oviposition rate: the number of egg batches laid in each patch,
  per mosquito, per day — use
  [setup_nu](https://dd-harp.github.io/ramp.xds/reference/setup_nu.md)
  or
  [setup_egg_batch_rates](https://dd-harp.github.io/ramp.xds/reference/setup_nu.md)

- `eggsPerBatch` or \\(\Gamma\\):

  the number of eggs per batch

After basic setup,
[change_MY_pars](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.md)
can be used for static parameters, or for parameters that are set up as
static ports (the default). Use advanced setup to modify dynamic ports.

## See also

[make_MY_obj_SI](https://dd-harp.github.io/ramp.xds/reference/make_MY_obj_SI.md),
[get_MY_pars.SI](https://dd-harp.github.io/ramp.xds/reference/get_MY_pars.SI.md)

[SI](https://dd-harp.github.io/ramp.xds/reference/SI.md),
[get_MY_pars.SI](https://dd-harp.github.io/ramp.xds/reference/get_MY_pars.SI.md)
