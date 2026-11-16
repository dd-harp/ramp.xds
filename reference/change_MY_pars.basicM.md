# Change parameters for `basicM` (**MY**)

For the `basicM` module (**MY**) for the \\s^{th}\\ species, change
parameter values named in `options` leaving other values unchanged.

## Usage

``` r
# S3 method for class 'basicM'
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

an **`xds`** object

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

- `sigma` or \\\sigma\\:

  emigration rate — use
  [setup_sigma](https://dd-harp.github.io/ramp.xds/reference/setup_sigma.md)
  or
  [setup_emigration_rates](https://dd-harp.github.io/ramp.xds/reference/setup_sigma.md)

- `mu` or \\\mu\\:

  emigration loss fraction — use
  [setup_mu](https://dd-harp.github.io/ramp.xds/reference/setup_mu.md)
  or
  [setup_emigration_loss](https://dd-harp.github.io/ramp.xds/reference/setup_mu.md)

- `K` or `K_matrix`:

  mosquito dispersal matrix — use
  [setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)

- `nu` or \\\nu\\:

  egg batch rate — use
  [setup_nu](https://dd-harp.github.io/ramp.xds/reference/setup_nu.md)
  or
  [setup_egg_batch_rates](https://dd-harp.github.io/ramp.xds/reference/setup_nu.md)

- `eggsPerBatch` or \\\Gamma\\:

  number of eggs per batch

After basic setup, change_MY_pars.basicM can be used to change static
parameter values. The `basicM` module does not implement dynamic
bionomic ports.
