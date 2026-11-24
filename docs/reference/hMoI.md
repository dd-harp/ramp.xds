# The `hMoI` module for the **XH** component

`hMoI` implements a model with two hybrid variables that track the mean
Multiplicity of Infection (MoI): the first models the true MoI, and the
second is the MoI for "younger" infections that are detected with higher
frequency than older ones.

This model is derived under a restrictive set of assumptions, so it
cannot be extended to model dynamic changes in population size or
treatment with anti-malarial drugs.

## State Variables

- `m1` or \\(m_1\\):

  mean MoI for all infections

- `m2` or \\(m_2\\):

  mean MoI for apparent (detectable) infections

## Demography

Demographic change is inconsistent with the model formulation. Human
population density \\((H)\\) is a parameter, but it should be changed
using
[change_XH_inits](https://dd-harp.github.io/ramp.xds/reference/change_XH_inits.md).

## Dynamical Term

- \\(h\\) or `foi`:

  The force of infection is computed and stored by
  [Exposure](https://dd-harp.github.io/ramp.xds/reference/Exposure.md)
  as `FoI`

In the `hMoI` module, there is no effect of pre-erythrocytic immunity,
so a constant fraction of infectious bites causes an infection: the
parameter \\(b.\\)

## Parameters

- `r1` or \\(r_1\\):

  clearance rate for all infections

- `r2` or \\(r_2\\):

  clearance rate for apparent infections, \\(r_2 \> r_1\\)

- `b`:

  probability of infection per infectious bite

- `c1` or \\(c_1\\):

  probability of transmitting from m1

- `c2` or \\(c_2\\):

  probability of transmitting from m2

- H:

  human or host population density

## Infection Dynamics

\$\$ \begin{array}{rl} dm_1/dt &= h - r_1 m_1 \\\\ dm_2/dt &= h - r_2
m_2 \\\\ \end{array}\$\$

## Prevalence

- \\(x_1\\):

  true prevalence: \\(x_1 = 1-e^{-m_1}\\)

- \\(x_2\\):

  apparent prevalence: \\(x_2 = 1-e^{-m_2}\\)

## Infectiousness

The model assumes that younger infections are more infectious, but that
some transmission does occur from the older infections: \$\$c_2 x_2 +
c_1 (x_1 - x_2)\$\$ so the infectious density is \$\$\left(c_2 x_2 + c_1
(x_1 - x_2)\right) H\$\$
