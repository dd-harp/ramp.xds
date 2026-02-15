# `SIS` Derivatives Function (an **XH** Module)

Computes the derivatives for SIS compartmental model with variables:

- \\I\\ - the density of infected humans (or hosts)

- \\S\\ - the density of susceptible humans (or hosts)

- \\H\\ - human (or host) population density

The model assumes \\S+I=H.\\ This function computes computes \\dH/dt\\
and \\dI/dt\\. For convenience, \\S\\ is also computed by
[get_XH_vars.SIS](https://dd-harp.github.io/ramp.xds/reference/get_XH_vars.SIS.md)
and
[parse_XH_orbits.SIS](https://dd-harp.github.io/ramp.xds/reference/parse_XH_orbits.SIS.md).

The force of infection or "happenings" rate is \\h\\. It is computed
upstream and used here.

The clearance rate for infections is \\r\\, and by assumption,
individuals are assumed to be susceptible to infection after clearing
infections.

The module includes a port to model mass treatment, \\\xi(t)\\.

**Human Demographic** change is modeled with two functions:

- \\B(t, H)\\ is the time-dependent population birth rate;

- \\D\\ is a linear operator, a matrix describing mortality, migration,
  aging, and dynamical transfers among population strata.

The derivatives computed are:

\$\$ \begin{array}{rl} dH/dt = & B(t,H) + D \cdot H \\ dI/dt = & h
(H-I) - r I - \xi(t) + D \cdot I \end{array} \$\$

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

## See also

[SIS
Dynamics](https://dd-harp.github.io/ramp.xds/articles/human_sis.html)
