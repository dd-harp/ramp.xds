# Compute Derivatives for **MY** module `SI`

The `SI` model for mosquito infection dynamics has the defined
**variable** classes:

- \\M\\ is the density of mosquitoes in each patch;

- \\Y\\ is the density of infected mosquitoes in each patch.

The density of infectious mosquitoes in each patch is given by a term
(returned by
[F_fqZ.SI](https://dd-harp.github.io/ramp.xds/reference/F_fqZ.SI.md)):
\$\$Z = e^{-\Omega \tau} \cdot Y\$\$

The model blood feeding **parameters** are:

- \\f\\ is the overall blood feeding rate

- \\q\\ is the human fraction for blood feeding

The **parameters** describing egg laying (see
[F_eggs.SI](https://dd-harp.github.io/ramp.xds/reference/F_eggs.SI.md))
are:

- \\\nu\\ is the egg laying rate

- \\\xi\\ is the number of eggs per batch

The model demographic **parameters** are:

- \\g\\ is the mortality rate

- \\\sigma\\ is the emigration rate

- \\\mu\\ is the emigration loss rate

- \\K\\ is the mosquito dispersal matrix

The four parameters describing mortality and migration are used to
construct a demographic matrix: \$\$\Omega = \mbox{diag}\left(g\right) -
\left(\mbox{diag}\left(1-\mu\right) - K \right) \cdot
\mbox{diag}\left(\sigma\right)\$\$

The emergence rate of adult mosquitoes, \\\Lambda\\, is computed by
[F_emerge](https://dd-harp.github.io/ramp.xds/reference/F_emerge.md),
and the **derivatives** are given by the equations: \$\$
\begin{array}{rr} dM/dt = & \Lambda - \Omega \cdot M \\ dY/dt = & f q
\kappa (M-Y) - \Omega \cdot Y \end{array}. \$\$

## Usage

``` r
# S3 method for class 'SI'
dMYdt(t, y, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

a vector with the derivatives

## See also

[F_fqZ.SI](https://dd-harp.github.io/ramp.xds/reference/F_fqZ.SI.md) and
[F_eggs.SI](https://dd-harp.github.io/ramp.xds/reference/F_eggs.SI.md)
