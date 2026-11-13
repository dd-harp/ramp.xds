# Compute derivatives for `SIS` (**XH**)

Computes derivatives for the `SIS` module. For details, see the module
documentation
[SIS](https://dd-harp.github.io/ramp.xds/reference/SIS.md).

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

## See also

[SIS](https://dd-harp.github.io/ramp.xds/reference/SIS.md), [SIS
Dynamics](https://dd-harp.github.io/ramp.xds/articles/human_sis.html)
