# Compute steady states for `macdonald` (**MY**)

Compute steady-state mosquito densities for the `macdonald` module.

## Usage

``` r
# S3 method for class 'macdonald'
steady_state_MY(Lambda, kappa, xds_obj, s = 1)
```

## Arguments

- Lambda:

  the daily emergence rate of adult mosquitoes

- kappa:

  net infectiousness

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [list](https://rdrr.io/r/base/list.html)

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes

- `Y`:

  density of infected adult mosquitoes

- `Z`:

  density of infectious adult mosquitoes

## Dynamics

The system is described by coupled differential equations with a delay.
A subscript \\(\tau\\) denotes a lagged value, for example
\\(M\_\tau=M(t-\tau)\\).

\$\$ \begin{array}{rl} dM/dt &= \Lambda-\Omega\cdot M \\\\\\\\ dP/dt &=
f(M-P)-\Omega\cdot P \\\\\\\\ dY/dt &= fq\kappa(M-Y)-\Omega\cdot Y
\\\\\\\\ dZ/dt &=
\Upsilon\cdot(fq\kappa)\_\tau(M\_\tau-Y\_\tau)-\Omega\cdot Z \end{array}
\$\$

Here, \\(P\\) is the density of mosquitoes that have blood-fed at least
once. The infected mosquito density \\(Y\\) generates infectious
mosquitoes after the EIP, represented by the delayed term in the \\(Z\\)
equation.

## See also

[macdonald](https://dd-harp.github.io/ramp.xds/reference/macdonald.md),
[dMYdt.macdonald](https://dd-harp.github.io/ramp.xds/reference/dMYdt.macdonald.md)
