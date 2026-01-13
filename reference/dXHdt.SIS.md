# Compute derivatives for `SIS` (**XH** Module)

Compute the derivatives for SIS compartmental model:

- \\S\\ is the density of susceptible humans (or hosts)

- \\I\\ is the density of infected humans (or hosts)

- \\H=S+I\\ is human (or host) population density

The parameters and terms are:

- \\h\\ is the force of infection (from )

- \\r\\ is the natural clearance rate for infections

The clearance rate for infections is \\r\\, and by assumption,
individuals are assumed to be susceptible to infection after clearing
infections.

The **`xds`** implementation computes \\dH/dt\\ rather than \\dS/dt.\\
In the functions
[get_XH_vars](https://dd-harp.github.io/ramp.xds/reference/get_XH_vars.md)
and
[parse_XH_orbits](https://dd-harp.github.io/ramp.xds/reference/parse_XH_orbits.md),
\\S\\ is computed as \\S=H-I\\ and listed as a variable. The derivatives
computed are:

\$\$ \begin{array}{rl} dH/dt = & B(t,H) + D \cdot H \\ dI/dt = & h
(H-I) - r I + D \cdot I \end{array} \$\$ where \\S=H-I\\;

\\B(t, H)\\ is the time-dependent population birth rate; and \\D\\ is a
linear operator, a matrix describing demographic changes, including
mortality, migration, and aging;

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

## Note

The model has a port to model mass treatment. The mass treatment rate,
\\\xi(t),\\ is a function of time. For mass screen and treat, the
treatment rate is lowered by the probability of detection. In this
model, mass treatment has the same effect as \\r.\\ With mass treatment,
infection dynamics are described by:

\$\$ \begin{array}{rl} dI/dt = & h (H-I) - (r + \xi(t)) I + D \cdot I
\end{array} \$\$

The ports for `mda` and `msat` are handled by computing

- `r_t = r + mda(t) + d_rdt*msat(t)`

- `dI <- foi*(H-I) - r_t*I + ...`
