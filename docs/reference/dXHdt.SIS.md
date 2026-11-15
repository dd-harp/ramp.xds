# Compute derivatives for `SIS` (**XH**)

Computes derivatives for the `SIS` module (**XH**)

## Usage

``` r
# S3 method for class 'SIS'
dXHdt(t, y, xds_obj, i)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

the derivatives, as a vector

## Demography

Demographic changes (**H**) are given by the equation:

\$\$ dH/dt = \mathbf{B}(t,H) - \mathbf{D}(H)\$\$

This model has no parameter or terms to describe disease-induced
mortality.

- [Births](https://dd-harp.github.io/ramp.xds/reference/Births.md) or
  \\(\mathbf{B}\\):

  the births function

- [Deaths](https://dd-harp.github.io/ramp.xds/reference/Deaths.md) or
  \\(\mathbf{D}\\):

  the deaths function

**Setup:**

- The defaults are no births and no deaths

- Use
  [setup_births](https://dd-harp.github.io/ramp.xds/reference/setup_births.md)
  to configure
  [Births](https://dd-harp.github.io/ramp.xds/reference/Births.md)

- Use
  [setup_deaths](https://dd-harp.github.io/ramp.xds/reference/setup_deaths.md)
  to configure
  [Deaths](https://dd-harp.github.io/ramp.xds/reference/Deaths.md)

Also, see
[xds_info_demographic_change](https://dd-harp.github.io/ramp.xds/reference/xds_info_demographic_change.md).

## Infection Dynamics

The model for infection dynamics (**X**) assumes that infections clear
naturally at a constant per-capita rate \\(r\\).

The model also includes two parameters to model treating and curing
malaria infections:

- a fraction \\(\rho\\) of incident infections are treated and cured;

- in addition, prevalent infections are treated and cured at the rate
  \\(\sigma\\)

A function describes an increase in clearance caused by mass treatment,
\\(\Xi(t)\\), a *shock* function.

The total per-capita clearance rate from treatment is thus \\(\rho h +
\sigma + \Xi(t)\\) After clearing, individuals are susceptible to
infection.

Infection dynamics are described by:

\$\$ dI/dt = h(1-\rho)(H-I) - (r + \rho h + \sigma) I - \Xi(t) I -
\mathbf{D}(I) \$\$

The mortality function \\(\mathbf{D}(I)\\) computes total mortality for
\\(I\\) using the same model as for \\(H\\): for example, if \\(D(H)=\mu
H\\), then \\(D(I)= \mu I.\\)

In the compartmental model, individuals are either susceptible
(\\(S\\)), or infected and infectious (\\(I\\)), so \\(S=H-I.\\) The
equation is redundant. After solving, the orbit of the variable
\\(S=H-I\\) is computed and attached.

## See also

[SIS](https://dd-harp.github.io/ramp.xds/reference/SIS.md), [SIS
Dynamics](https://dd-harp.github.io/ramp.xds/articles/human_sis.html)
