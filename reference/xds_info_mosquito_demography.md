# Mosquito Demography

Mosquito survival and dispersal is described by a *demographic matrix,*
denoted \\\Omega.\\ It is computed using several parameters:

- `g`:

  mortality rate

- `sigma`:

  patch emigration

- `mu`:

  emigration-related loss

- `K`:

  a dispersal matrix

The matrix is computed as:

\$\$ \Omega = \mbox{diag} \left( g + \sigma \mu \right) - K \cdot
\mbox{diag} \left( \sigma \left(1-\mu\right) \right) \$\$

In delay differential equations with a constant EIP (\\\tau\\), survival
and dispersal through the EIP is given by: \$\$ \Upsilon = e^{-\Omega
\tau} \$\$

## See also

[xds_info_mosquito_dispersal](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_dispersal.md)
