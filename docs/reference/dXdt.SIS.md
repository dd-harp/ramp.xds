# Compute Derivatives for the `SIS` **X**-Model

Compute the derivatives for SIS compartmental model. Here, the model
includes human demographic changes, and it is computed in an equivalent
form: \$\$ \begin{array}{rl} dH/dt = & B(t,H) + D \cdot H \\ dI/dt = & h
(H-I) - r I - \xi(t) + D \cdot I \end{array} \$\$ where \\S=H-I\\ ;
\\\xi(t)\\ is a function to simulate mass treatment; \\B(t, H)\\ is the
time-dependent birth rate; and \\D\\ is a linear operator, a matrix
describing demographic changes, including mortality, migration, and
aging;

## Usage

``` r
# S3 method for SIS
dXdt(t, y, pars, i)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- pars:

  an **`xds`** object

- i:

  the host species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector
