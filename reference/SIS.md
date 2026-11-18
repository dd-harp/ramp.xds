# The `SIS` module for the **XH** component

`SIS` implements a generalized S-I-S compartmental models for malaria as
a system of differential equations including:

- parameters to model treating and curing infections;

- functions to model human demographic change;

- functions to model mass treatment;

- support for progressive stratification

The [SIS
Vignette](https://dd-harp.github.io/ramp.xds/articles/human_sis.html)
discusses the module.

## Infection Dynamics

The model for infection dynamics (**X**) assumes that infections clear
naturally at a constant per-capita rate \\r\\.

The model also includes two parameters to model treating and curing
malaria infections:

- a fraction \\\rho\\ of incident infections are treated and cured;

- in addition, prevalent infections are treated and cured at the rate
  \\\sigma\\

A function describes an increase in clearance caused by mass treatment,
\\\Xi(t)\\, a *shock* function.

The total per-capita clearance rate from treatment is thus \\\rho h +
\sigma + \Xi(t)\\ After clearing, individuals are susceptible to
infection.

Infection dynamics are described by:

\$\$ dI/dt = h(1-\rho)(H-I) - (r + \rho h + \sigma) I - \Xi(t) I -
\mathbf{D}(I) \$\$

The mortality function \\\mathbf{D}(I)\\ computes total mortality for
\\I\\ using the same model as for \\H\\: for example, if \\D(H)=\mu H\\,
then \\D(I)= \mu I.\\

In the compartmental model, individuals are either susceptible (\\S\\),
or infected and infectious (\\I\\), so \\S=H-I.\\ The equation is
redundant. After solving, the orbit of the variable \\S=H-I\\ is
computed and attached.

## State Variables

- `H`:

  the total human or host population density

- `I`:

  the density of infected humans or hosts

- `S=H-I`:

  the density of susceptible humans or hosts

## Demography

Demographic changes (**H**) are given by the equation:

\$\$ dH/dt = \mathbf{B}(t,H) - \mathbf{D}(H)\$\$

This model has no parameter or terms to describe disease-induced
mortality.

- [`Births`](https://dd-harp.github.io/ramp.xds/reference/Births.md) or
  \\\mathbf{B}\\:

  the births function

- [`Deaths`](https://dd-harp.github.io/ramp.xds/reference/Deaths.md) or
  \\\mathbf{D}\\:

  the deaths function

**Setup:**

- The defaults are no births and no deaths

- Use
  [setup_births](https://dd-harp.github.io/ramp.xds/reference/setup_births.md)
  to configure
  [`Births`](https://dd-harp.github.io/ramp.xds/reference/Births.md)

- Use
  [setup_deaths](https://dd-harp.github.io/ramp.xds/reference/setup_deaths.md)
  to configure
  [`Deaths`](https://dd-harp.github.io/ramp.xds/reference/Deaths.md)

Also, see
[xds_info_demographic_change](https://dd-harp.github.io/ramp.xds/reference/xds_info_demographic_change.md).

## Dynamical Term

- \\h\\ or `foi`:

  The force of infection is computed and stored by
  [Exposure](https://dd-harp.github.io/ramp.xds/reference/Exposure.md)
  as `FoI`

In the `SIS` module, there is no effect of pre-erythrocytic immunity, so
a constant fraction of infectious bites causes an infection: the
parameter \\b.\\

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

  test positivity by light microscopy

- `q_rdt`:

  test positivity by RDT

- `q_pcr`:

  test positivity by PCR

## Prevalence

- \\x\\:

  True prevalence is \\x = I/H\\

- \\x\_{lm}\\:

  Expected prevalence by light microscopy is \\q\_{lm}\\ x\\

- \\x\_{rdt}\\:

  Expected prevalence by RDT is \\q\_{rdt}\\ x\\

- \\x\_{pcr}\\:

  Expected prevalence by PCR is \\q\_{pcr}\\ x\\

## Infectiousness

The fraction of mosquitoes that become infected after blood feeding on
someone who is infectious is \\c.\\ The infectious density of the human
(host) population is \\cxI = cH.\\

## Mass Treatment

Mass treatment clears infections. At basic setup \\\Xi(t)=0\\. To set up
mass treatment, see
[setup_mass_treatment](https://dd-harp.github.io/ramp.xds/reference/setup_mass_treatment.md).
Mass treatment is either:

- `mda(t)` or \\\Xi(t)\\:

  mass drug administration

- `msat(t)` or \\\Xi(t)\\:

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
