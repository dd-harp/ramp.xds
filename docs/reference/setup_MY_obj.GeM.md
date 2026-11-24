# Set up the module object for `GeM` (**MY**)

Set up a `GeM` module object for the **MY** component and the
\\(s^{th}\\) vector species.

- Set the module name to `GeM`.

- Create the module object with
  [make_MY_obj_GeM](https://dd-harp.github.io/ramp.xds/reference/make_MY_obj_GeM.md),
  passing `nPatches` and `options`.

- Set the skill set using
  [get_skillset_MY](https://dd-harp.github.io/ramp.xds/reference/get_skillset_MY.md).

- Initialize the circadian function using
  [setup_F_circadian.setup](https://dd-harp.github.io/ramp.xds/reference/setup_F_circadian.setup.md).

- Initialize the mosquito demographic matrix port.

- Initialize the EIP survival and dispersal matrix port.

- Initialize the mosquito dispersal matrix using
  [setup_K_matrix.zero](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.zero.md).

- Set initial values using
  [setup_MY_inits](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.md),
  passing `options` to override defaults.

- Return the updated **`xds`** model object.

## Usage

``` r
# S3 method for class 'GeM'
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

The module object is stored at `xds_obj$MY_obj[[s]]` Its class is set to
`GeM`.

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

- `eggs_per_batch`:

  number of eggs per batch

After basic setup,
[change_MY_pars.GeM](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.GeM.md)
can be used to change static parameter values. By default, bionomic
parameters are configured as static baselines; advanced setup can be
used to define dynamic ports.

## EIP

Let \\(\tau(t)\\) be the EIP for a mosquito infected at time \\(t\\); it
becomes infectious at time \\(t+\tau(t)\\). Let \\(\tau'(t)\\) be the
elapsed time since infection for a mosquito that becomes infectious at
time \\(t\\). These quantities satisfy \$\$\tau(t)=\tau'(t+\tau(t))
\qquad \tau'(t)=\tau(t-\tau'(t)).\$\$ The `F_eip` function returns the
lag \\(\tau'(t)\\) used by the delay equations. A time-varying EIP also
requires its derivative, computed by `dF_eip`.

## Accessory Variable

The matrix \\(\Upsilon(t)\\) represents survival and dispersal through
the time-varying EIP, from \\(t\_\tau=t-\tau'(t)\\) to \\(t\\). In the
autonomous case it is \\(e^{-\Omega\tau}\\). For time-varying demography
and EIP, it is computed as an accessory variable:
\$\$\frac{d\Upsilon}{dt}=\left(\Omega(t\_\tau)\left(1-\frac{d\tau'(t)}{dt}\right)-\Omega(t)\right)\cdot\Upsilon(t).\$\$
The initial matrix is set to \\(\exp(-\Omega eip)\\). This variable is
internal to the model and is not an output orbit.

## See also

[GeM](https://dd-harp.github.io/ramp.xds/reference/GeM.md),
[make_MY_obj_GeM](https://dd-harp.github.io/ramp.xds/reference/make_MY_obj_GeM.md),
[setup_MY_inits.GeM](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.GeM.md)
