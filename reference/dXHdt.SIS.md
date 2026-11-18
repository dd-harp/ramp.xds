# Compute derivatives for `SIS` (**XH**)

The derivatives for the `SIS` module are:

\$\$\begin{array}{rl} dH/dt &= \mathbf{B}(t,H) - \mathbf{D}(H) \\ dI/dt
&= h(1-\rho)(H-I) - (r + \rho h + \sigma) I - \Xi(t) I - \mathbf{D}(I)
\end{array}\$\$

## Usage

``` r
# S3 method for class 'SIS'
dXHdt(t, y, xds_obj, i)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

the derivatives, as a vector

## Infection Dynamics

The model for infection dynamics (**X**) assumes that infections clear
naturally at a constant per-capita rate \\r\\.

The model also includes parameters and functions to model treating and
curing malaria infections:

- a fraction \\\rho\\ of incident infections are treated and cured

- since incident infections are treated, the FoI is \\h (1-\rho)\\

- in addition, prevalent infections are treated and cured at the rate
  \\\sigma\\

- functions describes an increase in clearance caused by mass treatment,
  \\\Xi(t)\\, a *shock* function

The total per-capita clearance rate from treatment is thus \\\rho h +
\sigma + \Xi(t)\\

After clearing, individuals are susceptible to infection.

## Demography

This model has no parameter or terms to describe disease-induced
mortality.

- [`Births`](https://dd-harp.github.io/ramp.xds/reference/Births.md) or
  \\\mathbf{B}\\:

  the births function

- [`Deaths`](https://dd-harp.github.io/ramp.xds/reference/Deaths.md) or
  \\\mathbf{D}\\:

  the deaths function

**Setup:**

The default is a constant population model: birth and death rates are
set to zero

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

## See also

[SIS](https://dd-harp.github.io/ramp.xds/reference/SIS.md), [SIS
Dynamics](https://dd-harp.github.io/ramp.xds/articles/human_sis.html)
