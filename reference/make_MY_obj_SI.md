# Make parameters for SI ODE adult mosquito model

Make parameters for SI ODE adult mosquito model

## Usage

``` r
make_MY_obj_SI(
  nPatches,
  options = list(),
  eip = 0,
  g = 1/12,
  sigma = 1/8,
  mu = 0,
  f = 0.3,
  q = 0.95,
  nu = 1,
  eggsPerBatch = 60
)
```

## Arguments

- nPatches:

  is the number of patches, an integer

- options:

  a named list: named values override defaults

- eip:

  the extrinsic incubation period

- g:

  mosquito mortality rate

- sigma:

  emigration rate

- mu:

  fraction lost during emigration

- f:

  feeding rate

- q:

  human blood fraction

- nu:

  oviposition rate, per mosquito

- eggsPerBatch:

  eggs laid per oviposition

## Value

a [list](https://rdrr.io/r/base/list.html)

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

- `eip` or \\\tau\\:

  extrinsic incubation period — use
  [setup_eip](https://dd-harp.github.io/ramp.xds/reference/setup_eip.md)

- `sigma` or \\\sigma\\:

  emigration rate — use
  [setup_sigma](https://dd-harp.github.io/ramp.xds/reference/setup_sigma.md)
  or
  [setup_emigration_rates](https://dd-harp.github.io/ramp.xds/reference/setup_sigma.md)

- `mu` or \\\mu\\:

  emigration loss rate — use
  [setup_mu](https://dd-harp.github.io/ramp.xds/reference/setup_mu.md)
  or
  [setup_emigration_loss](https://dd-harp.github.io/ramp.xds/reference/setup_mu.md)

- `K`:

  mosquito dispersal matrix — use
  [setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)

- `nu` or \\\nu\\:

  the egg batch rate, in batches per mosquito per day — use
  [setup_nu](https://dd-harp.github.io/ramp.xds/reference/setup_nu.md)
  or
  [setup_egg_batch_rates](https://dd-harp.github.io/ramp.xds/reference/setup_nu.md)

- `eggsPerBatch` or \\\Gamma\\:

  the number of eggs per batch

After basic setup,
[change_MY_pars](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.md)
can be used for static parameters, or for for parameters that are set up
as static ports (the default). Use advanced setup to modify dynamic
ports.

## See also

[SI](https://dd-harp.github.io/ramp.xds/reference/SI.md),
[setup_MY_obj.SI](https://dd-harp.github.io/ramp.xds/reference/setup_MY_obj.SI.md)
