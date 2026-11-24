# Set up the module object for `basicM` (**MY**)

Set up a `basicM` module object for the **MY** component and the
\\(s^{th}\\) vector species.

- Set the module name to `basicM`.

- Create the module object with
  [make_M_obj_basicM](https://dd-harp.github.io/ramp.xds/reference/make_M_obj_basicM.md),
  passing `nPatches` and `options`.

- Set the skill set using
  [get_skillset_MY](https://dd-harp.github.io/ramp.xds/reference/get_skillset_MY.md).

- Initialize the circadian function using
  [setup_F_circadian.setup](https://dd-harp.github.io/ramp.xds/reference/setup_F_circadian.setup.md).

- Initialize the mosquito demographic matrix port.

- Initialize the mosquito dispersal matrix using
  [setup_K_matrix.zero](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.zero.md).

- Set initial values using
  [setup_MY_inits](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.md),
  passing `options` to override defaults.

- Return the updated **`xds`** model object.

## Usage

``` r
# S3 method for class 'basicM'
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

The module object is stored at `xds_obj$MY_obj[[s]]`. Its classes are
`basicM` and `basicM_` followed by `xds_obj$xds`.

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes: those that have blood-fed at least
  once

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

- `K` or `K_matrix`:

  mosquito dispersal matrix — use
  [setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)

- `nu` or \\(\nu\\):

  oviposition rate: the number of egg batches laid in each patch, per
  mosquito, per day — use
  [setup_nu](https://dd-harp.github.io/ramp.xds/reference/setup_nu.md)
  or
  [setup_egg_batch_rates](https://dd-harp.github.io/ramp.xds/reference/setup_nu.md)

- `eggsPerBatch` or \\(\Gamma\\):

  number of eggs per batch

After basic setup,
[change_MY_pars.basicM](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.basicM.md)
can be used to change static parameter values. The `basicM` module
evaluates bionomic parameter functions through
[MBionomics](https://dd-harp.github.io/ramp.xds/reference/MBionomics.md).

## See also

[basicM](https://dd-harp.github.io/ramp.xds/reference/basicM.md),
[make_M_obj_basicM](https://dd-harp.github.io/ramp.xds/reference/make_M_obj_basicM.md),
[setup_MY_inits.basicM](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.basicM.md)
