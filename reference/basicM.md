# The `basicM` module for the **MY** component

Implements a basic model of adult mosquito ecology without explicit
infection dynamics. It can be used when mosquito population dynamics are
needed without modeling mosquito infection.

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes: those that have blood-fed at least
  once

## Dynamical Terms

- `Lambda` or \\\Lambda\\:

  adult mosquito emergence rate, computed by
  [Emergence](https://dd-harp.github.io/ramp.xds/reference/Emergence.md)

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

After basic setup,
[change_MY_pars.basicM](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.basicM.md)
can be used to change static parameter values. The `basicM` module does
not implement dynamic bionomic ports.

## Survival and Dispersal

Mosquito survival and dispersal are described by the demographic matrix
\\\Omega\\. The parameter \\g\\ is the per-capita mortality rate while
mosquitoes are in a patch. Mosquitoes emigrate from patches at
per-capita rates \\\sigma\\, and a fraction \\\mu\\ of emigrating
mosquitoes is lost. The total rate of loss from a patch is
\$\$g+\mu\sigma.\$\$

The columns of the mosquito dispersal matrix \\K\\ sum to zero. For
non-trivial dispersal matrices, \\\mbox{diag}(K)=-1\\; the off-diagonal
elements describe the fractions of surviving emigrants that arrive in
other patches. The demographic matrix is
\$\$\Omega=\mbox{diag}(g+\sigma\mu)-K\cdot\mbox{diag}(\sigma(1-\mu)).\$\$

## Blood Feeding

The blood feeding rate \\f\\ and human fraction \\q\\ are parameters of
the module. At baseline they are static. They should satisfy natural
constraints: \\f=0\\ if no blood hosts are available, \\q=0\\ if no
humans are available, and \\q=1\\ if humans are the only available
hosts. The module does not model infection, but `fqM` is used to compute
the human biting rate.

## Dynamics

Adult mosquitoes emerge from aquatic habitats at rate \\\Lambda\\ and
disperse and die according to \\\Omega\\. The ODE model is \$\$
\begin{array}{rl} dM/dt &= \Lambda-\Omega\cdot M \\\\ dP/dt &=
f(M-P)-\Omega\cdot P \end{array}\$\$ Here, \\P\\ is the density of
mosquitoes that have blood-fed at least once. Parity, \\P/M\\, is
computed during parsing and stored as `parous`. For discrete-time
models,
[Update_MYt.basicM](https://dd-harp.github.io/ramp.xds/reference/Update_MYt.basicM.md)
uses the one-day update equations.

## Infective Biting

The `basicM` module does not represent mosquito infection, so its
infective biting density \\fqZ\\ is zero. Its human biting density,
\\fqM\\, is the number of bites on humans in each patch per day.
[F_fqM.basicM](https://dd-harp.github.io/ramp.xds/reference/F_fqM.basicM.md)
computes this patch-level quantity.
[F_hbr](https://dd-harp.github.io/ramp.xds/reference/F_hbr.md) combines
`fqM` with the biting distribution matrix and resident-bite fraction to
compute the stratum-level human biting rate (`HBR`). The `EIR` is zero
for this module because there are no infective bites.

## Egg Laying

The egg batch rate \\\nu\\ and the number of eggs per batch \\\Gamma\\
determine the egg production rate per patch: \$\$G=\nu\Gamma M.\$\$
[F_eggs.basicM](https://dd-harp.github.io/ramp.xds/reference/F_eggs.basicM.md)
computes \\G\\, which is passed to
[EggLaying](https://dd-harp.github.io/ramp.xds/reference/EggLaying.md)
for distribution among aquatic habitats.

## See also

[make_M_obj_basicM](https://dd-harp.github.io/ramp.xds/reference/make_M_obj_basicM.md),
[setup_MY_obj.basicM](https://dd-harp.github.io/ramp.xds/reference/setup_MY_obj.basicM.md),
[dMYdt.basicM](https://dd-harp.github.io/ramp.xds/reference/dMYdt.basicM.md)
