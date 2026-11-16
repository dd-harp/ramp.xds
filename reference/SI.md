# The `SI` module for the **MY** component

This implements an `SI` compartmental model for mosquito infection
dynamics. Adult mosquitoes are either susceptible or infected and
infectious, and infected mosquitoes die rather than recover from
infection. In its simplest form, the dynamics describe adult mosquito
ecology \\(M)\\ and infection dynamics \\(Y)\\: \$\$ \begin{array}{rl}
dM/dt &= \Lambda - g M \\ dY/dt &= fq\kappa(M-Y) - g Y \end{array} \$\$
It is equivalent to an S-I compartmental model under a change of
variables \\S = M-Y\\ and \\I=Y.\\

The `SI` module is a generalized implementation of the S-I model:

- the module adds a variable to compute the density of parous mosquitoes

- mosquito ecology and infection dynamics are spatial – a patch-based
  meta-population model – so state variables and most parameters are
  vectors of length `nPatches.` The dispersal matrix has dimension
  `nPatches` \\\times\\ `nPatches`

- biting by infectious mosquitoes is computed at time \\t\\ as if the
  infected mosquitoes had survived through the EIP \\(\tau)\\.

- parameters describe egg laying by the adult mosquito population

- several of the the core bionomic parameters are set up as *ports* and
  designed to be computed as a *changing baseline* that can be *modified
  by vector control*

The [SI
Vignette](https://dd-harp.github.io/ramp.xds/articles/adult_si.html)
discusses the model family, and the module design and implementation.

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes

- `Y`:

  density of infected adult mosquitoes

## Dynamical Terms

- `Lambda` or \\\Lambda\\:

  the number of adult mosquitoes emerging, per patch, per day, computed
  by
  [Emergence](https://dd-harp.github.io/ramp.xds/reference/Emergence.md)

- `kappa` or \\\kappa\\:

  net infectiousness: the probability a mosquito becomes infected after
  blood feeding on a human, is computed by
  [Transmission](https://dd-harp.github.io/ramp.xds/reference/Transmission.md)

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

## Survival and Dispersal

Mosquito survival and dispersal are described by a single matrix,
\\\Omega.\\

The parameter \\g\\ is interpreted as the per-capita mortality rate for
mosquitoes while they are in a patch. Mosquitoes emigrate from patches
at the per-capita rates \\\sigma\\, and some fraction of emigrating
mosquitoes is lost (\\\mu\\). The total rate of loss from a patch is
\$\$g + \mu \sigma.\$\$

The columns of the mosquito dispersal matrix \\(K)\\ sum to zero. The
default matrix is all zeros, but for non-trivial dispersal matrices:
\$\$\mbox{diag}\left(K\right)=-1\$\$ and the non-diagonal elements are
fraction of surviving mosquitoes that arrive at another patch after a
flight bout. (If there is any population loss associated with
emigration, it goes in the term \\\mu\\.)

The matrix \\\Omega\\ is computed as \$\$\Omega = \mbox{diag}\left(g +
\sigma \mu \right) - K \cdot \mbox{diag}\left(\sigma
\left(1-\mu\right)\right)\$\$

## Blood Feeding

The overall blood feeding rate (\\f\\) and the human fraction (\\q\\)
are configured as *ports.* By default, the baseline values are constant,
but they can be modified by control. At their baseline, the parameter
values *ought* to be subject to some natural constraints:

- If no blood hosts are available, then it should be true that \\f=0.\\

- If no humans are available, then it should be true that \\q=0.\\

- If only humans are available, then it should be true that \\q=1.\\

Using advanced setup options, the blood feeding parameters can be forced
by variables describing resource availability or weather.

## Dynamics

**Ecology**

Adult mosquitoes emerge from aquatic habitats \\(\Lambda)\\ and disperse
and die (\\\Omega\\): \$\$ dM/dt = \Lambda - \Omega \cdot M \$\$ The
model also computes the density of parous adult mosquitoes: those that
have blood fed at least once: \$\$ dP/dt = fq(M-P) - \Omega \cdot P \\
\$\$ Parity is computed during parsing as \\P/M\\ and its orbit is
stored as `parity.`

**Infection Dynamics**

Given the per-capita daily rate of human blood feeding \\(fq)\\ and net
infectiousness \\(\kappa)\\, the FoI for mosquitoes is \\fq\kappa\\.
Changes in the density of infected mosquitoes is given by: \$\$ dY/dt =
fq\kappa(M-Y) - \Omega \cdot Y \\ \$\$

## Infective Biting

**Infective biting density** is defined as the number of human
infectious bites by the mosquito population in each patch, each day. In
`SI`, infective biting density is computed if infected mosquitoes at
time \\t\\, \\Y(t),\\ had survived and dispersed through an EIP of
\\\tau\\ days \\e^{- \Omega \tau}\\.

- \\Z\\:

  density of infectious mosquitoes, imputed from \\Y\\

- `fqZ`:

  Infective biting density: the number of infectious bites, per patch,
  per day \$\$fqZ = e^{-\Omega \tau} \cdot \left( f q Y \right) \$\$

- `fqM`:

  Human biting density: the number of human infectious bites, per patch,
  per day

Note that \\e^{-\Omega \tau}\\ is computed with the matrix exponential
function [expm](https://rdrr.io/pkg/expm/man/expm.html)

**The EIR**

\\fqZ\\ is the dynamical term computed by
[F_fqZ.SI](https://dd-harp.github.io/ramp.xds/reference/F_fqZ.SI.md). It
is called by
[compute_EIR](https://dd-harp.github.io/ramp.xds/reference/compute_EIR.md)
(in
[Transmission](https://dd-harp.github.io/ramp.xds/reference/Transmission.md))
to compute and store `EIR,` the local daily EIR \\(E)\\. Note that `fqZ`
is a vector of length `nPatches` but the local EIR is a vector of length
`nStrata.` The **XY** interface uses information about humans and
mosquitoes to build the biting distribution matrix, \\\beta,\\ and if
there are no visitors, then \$\$E = \beta \cdot fqZ\$\$ The orbits of
`fqZ` and `EIR` are computed and stored by
[xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)
by the functions that parse the outputs.

**The HBR**

Since estimates of the EIR are computed by taking the product of the
human biting rate (HBR or \\B\\) and the sporozoite rate, there is
interest in knowing \\fqM\\ and \\B.\\ The quantity `fqM` is computed by
[F_fqM.SI](https://dd-harp.github.io/ramp.xds/reference/F_fqM.SI.md).
The orbits of `fqM` and `HBR` are also computed during parsing, as \$\$B
= \beta \cdot fqM\$\$

## Egg Laying

- `G`:

  the number of eggs laid, per patch, per day \$\$G = \nu M \Gamma \$\$

\\G\\ is the dynamical term passed to
[EggLaying](https://dd-harp.github.io/ramp.xds/reference/EggLaying.md)
to distribute eggs among the habitats that are found in each patch.

## See also

- Basic Setup:
  [make_MY_obj_SI](https://dd-harp.github.io/ramp.xds/reference/make_MY_obj_SI.md),
  [setup_MY_obj.SI](https://dd-harp.github.io/ramp.xds/reference/setup_MY_obj.SI.md)

- Dynamics:
  [dMYdt.SI](https://dd-harp.github.io/ramp.xds/reference/dMYdt.SI.md)

- Parameters:
  [get_MY_pars.SI](https://dd-harp.github.io/ramp.xds/reference/get_MY_pars.SI.md),
  [change_MY_pars.SI](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.SI.md)

- Variables:
  [make_MY_inits_SI](https://dd-harp.github.io/ramp.xds/reference/make_MY_inits_SI.md),
  [get_MY_inits](https://dd-harp.github.io/ramp.xds/reference/get_MY_inits.md),
  [change_MY_inits.SI](https://dd-harp.github.io/ramp.xds/reference/change_MY_inits.SI.md),
  [setup_MY_inits.SI](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.SI.md)

- The [SI
  Vignette](https://dd-harp.github.io/ramp.xds/articles/adult_si.html)
