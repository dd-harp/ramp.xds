# The `SIS` module for the **XH** component

`SIS` implements a family of SIS compartmental models for malaria as a
system of differential equations: individuals are either susceptible
\\(S)\\ or infected and infectious \\(I)\\. In its simplest form, SIS
dynamics are: \$\$ dI/dt = h S - r I \$\$

This is a generalized implementation includes:

- parameters to model treating and curing infections;

- functions to model human demographic change;

- functions to model mass treatment;

- and progressive stratification

## State Variables

- `H`:

  the total human or host population density

- `I`:

  the density of infected humans or hosts

- `S=H-I`:

  the density of susceptible humans or hosts

## Prevalence

- `x`:

  true prevalence is \\x = I/H\\

## Parameters

The parameters that are returned by
[get_XH_pars.SIS](https://dd-harp.github.io/ramp.xds/reference/get_XH_pars.SIS.md)
and also modifiable by
[change_XH_pars.SIS](https://dd-harp.github.io/ramp.xds/reference/change_XH_pars.SIS.md)
describe the dynamics, or detection by diagnostic methods.

- `b`:

  the fraction of infective bites that would cause an infection

- `r`:

  clearance rate for infections

- `c`:

  the probability an infected person would infect a mosquito

- `rho` or \\\rho\\:

  the fraction of incident infections that are treated and cured

- `sigma` or \\\sigma\\:

  the rate that prevalent infections are treated and cured

- `q_lm`:

  expected prevalence by light microscopy would be \\q\_{lm} \\x\\

- `q_rdt`:

  expected prevalence by RDT would be \\q\_{rdt} \\x\\

- `q_pcr`:

  expected prevalence by PCR would be \\q\_{pcr} \\x\\

## Dynamical Term

- \\h\\ or `foi`:

  The force of infection is computed and stored by
  [Exposure](https://dd-harp.github.io/ramp.xds/reference/Exposure.md)
  as `FoI`

## Demographic Change

- `Births` or \\\mathbf{B}\\:

  the births function

- `Deaths` or \\\mathbf{D}\\:

  the deaths function

**NOTES:**

- The defaults are no births and no deaths

- Use
  [setup_births](https://dd-harp.github.io/ramp.xds/reference/setup_births.md)
  to configure `Births`

- Use
  [setup_deaths](https://dd-harp.github.io/ramp.xds/reference/setup_deaths.md)
  to configure `Deaths`

Also, see
[xds_info_demographic_change](https://dd-harp.github.io/ramp.xds/reference/xds_info_demographic_change.md).

## Dynamics

The model for demographic change has terms to compute births
(\\\mathbf{B}\\) and deaths (\\\mathbf{D}\\): \$\$ dH/dt =
\mathbf{B}(t,H) - \mathbf{D}(H)\$\$

The model for infection dynamics assumes that infections clear naturally
at a constant per-capita rate \\r\\. The model also includes two
parameters to model treating and curing malaria infections:

- a fraction \\\rho\\ of incident infections are treated and cured;

- in addition, prevalent infections are treated and cured at the rate
  \\\sigma\\,

- so the total clearance rate from treatment is \\\rho h + \sigma\\
  After clearing, individuals are susceptible to infection. A term for
  mass treatment increases the rate infections are cured: (\\\Xi(t)\\).

Infection dynamics are described by:

\$\$ dI/dt = h(1-\rho)(H-I) - (r + \rho h + \sigma) I - \Xi(t) I -
\mathbf{D}(I) \$\$

The mortality function \\\mathbf{D}(I)\\ computes total mortality for
\\I\\ using the same model as for \\H\\: for example, if \\D(H)=\mu H\\,
then \\D(I)= \mu I.\\ This model has no parameter or terms to describe
disease-induced mortality.

In the compartmental model, individuals are either susceptible (\\S\\),
or infected and infectious (\\I\\), so \\S=H-I.\\ The equation is
redundant. After solving, the orbit of the variable \\S=H-I\\ is
computed and attached.

## Exposure

For `SIS` the `FoI` is computed (in
[Exposure](https://dd-harp.github.io/ramp.xds/reference/Exposure.md))
under the assumption that a constant fraction of infectious bites causes
an infection: the parameter \\b.\\

## Infectiousness

The fraction of mosquitoes that become infected after blood feeding on
someone who is infectious is \\c.\\ The infectious density of the human
(hos) population is \\cI.\\

## Mass Treatment

Mass treatment clears infections. At basic setup \\\Xi(t)=0\\. To set up
mass treatment, see
[setup_mass_treatment](https://dd-harp.github.io/ramp.xds/reference/setup_mass_treatment.md).
Mass treatment is either:

- `mda(t)` or \\\Xi(t)\\:

  mass drug administration

- `msat` or \\\Xi(t)\\:

  mass screen and treat

## See also

- Basic Setup:
  [make_XH_obj_SIS](https://dd-harp.github.io/ramp.xds/reference/make_XH_obj_SIS.md),
  [setup_XH_obj.SIS](https://dd-harp.github.io/ramp.xds/reference/setup_XH_obj.SIS.md)

- Dynamics:
  [dXHdt.SIS](https://dd-harp.github.io/ramp.xds/reference/dXHdt.SIS.md)

- Parameters:
  [get_XH_pars.SIS](https://dd-harp.github.io/ramp.xds/reference/get_XH_pars.SIS.md),
  [change_XH_pars.SIS](https://dd-harp.github.io/ramp.xds/reference/change_XH_pars.SIS.md)

- Variables:
  [make_XH_inits_SIS](https://dd-harp.github.io/ramp.xds/reference/make_XH_inits_SIS.md),
  [get_XH_inits](https://dd-harp.github.io/ramp.xds/reference/get_XH_inits.md),
  [change_XH_inits.SIS](https://dd-harp.github.io/ramp.xds/reference/change_XH_inits.SIS.md),
  [setup_XH_inits.SIS](https://dd-harp.github.io/ramp.xds/reference/setup_XH_inits.SIS.md)

- [The SIS
  Vignette](https://dd-harp.github.io/ramp.xds/articles/human_sis.html)
