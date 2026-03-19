# The `GeM` Module for the MY Component

`GeM` is a system of delay differential equation model for mosquito
ecology and infection dynamics. The core dynamics trace back to
Macdonald's mosquito model (1952). Macdonald's model was extended by
Aron & May (1982) to include a variable for mosquito population density
and a term for emergence (\\\Lambda\\). Aron & May's model was extended
by Wu *et al.* (2023) to include spatial dynamics. In this version, a
new method is introduced to non-autonomous dynamics: an accessory
variable computes time-varying survival and dispersal through a time
varying EIP.

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes

- `Y`:

  density of infected adult mosquitoes

- `Z`:

  density of infectious adult mosquitoes

## Parameters

- `f`:

  blood feeding rate

- `q`:

  human blood fraction

- `nu`:

  egg batch laying rate

- `eip`:

  extrinsic incubation period (\\\tau\\)

- `g`:

  mosquito mortality rate

- `sigma`:

  emigration rate (\\\sigma\\)

- `mu`:

  emigration loss rate (\\\mu\\)

- `K`:

  mosquito dispersal matrix

- `eggs_per_patch`:

  \# eggs per batch

## Mosquito Demography

The *demographic matrix* \\\Omega\\ is: \$\$ \Omega(t) = \mbox{diag}
\left( g(t) + \sigma(t) \mu(t) \right) - K \cdot \mbox{diag} \left(
\sigma(t) \left(1-\mu(t)\right) \right) \$\$

## EIP

Let \\\tau(t)\\ denote the EIP for a mosquito, if infected at time
\\t\\: the mosquito would become infectious at time \\t+\tau(t).\\
Similarly, let \\\tau'(t)\\ denote the lag for a mosquito that became
*infectious* at time \\t\\. it was infected at time \\t-\tau'(t).\\ The
two are related by the identities \$\$\tau(t) = \tau'(t+\tau(t))\$\$ and
\$\$\tau'(t) = \tau(t-\tau'(t)).\$\$ In the implementation, a function
`F_eip` returns \\\tau'(t)\\. Implementation of dynamically changing EIP
also requires a function to compute the derivative `dF_eip` (below).

## The Accessory Variables, \\\Upsilon\\

To compute time-varying survival and dispersal through a time-varying
EIP, we a set of accessory variables, called \\\Upsilon\\. To motivate
the algorithm used to compute it, we introduce a new variable, \\U\\
that integrates \\\Omega\\ over time: \$\$dU/dt = \Omega(t).\$\$

Cumulative mortality from time \\t\\ to time \\t+s\\ is: \$\$\gamma(t,s)
= U(t+s) - U(t).\$\$ Survival and dispersal for the mosquitoes that
became infectious at time \\t\\ is: \$\$\Upsilon(t) =
e^{-\gamma\left(t\_\tau, t\right)} = e^{-\left(U\left(t\right) -
U\left(t\_\tau \right)\right)}.\$\$ where \\t\_\tau = t-\tau'(t)\\.

In `GeM`, \\\Upsilon\\ is computed as an accessory variable, with
derivatives: \$\$d\Upsilon/dt =
\left(\Omega\left(t\_\tau\right)\left(1-\frac{d\tau'(t)}{dt}\right)-\Omega(t)\right)
\cdot \Upsilon(t)\$\$ Initial conditions are set to: \$\$\Upsilon(t_0) =
e^{-\Omega(t_0)}\$\$

## Inputs

**Emergence** – `Lambda` or \\\Lambda\\, is computed by the
**ML**-Interface using outputs of the **L** Component

**Net Infectiousness** – `kappa` or \\\kappa\\, is computed by the
**XY**-Interface using outputs of the **XH** Component

## Dynamics

In the following we use the subscript \\\tau\\ to denote the value of a
parameter, term, or variable at time \\t-\tau'(t)\\: \$\$X\_\tau =
X(t-\tau'(t))\$\$.

The derivatives of the state variables are: \$\$ \begin{array}{rl} dM/dt
&= \Lambda - \Omega \cdot M \\ dP/dt &= f(M-P) - \Omega \cdot P \\ dY/dt
&= fq\kappa(M-Y) - \Omega \cdot Y \\ dZ/dt &= \Upsilon \cdot
(fq\kappa)\_\tau(M\_\tau-Y\_\tau) - \Omega \cdot Z \\ \end{array}\$\$

The function
[dMYdt.GeM](https://dd-harp.github.io/ramp.xds/reference/dMYdt.GeM.md)
also computes the variables: \\\Upsilon\\; and an accessory variable
that internalizes computation of \\(fq\kappa)\_\tau\\.

## Egg Laying

The number of egg batches laid, per patch, per day is \\\nu M\\. The
total of eggs laid is \\\nu M \times\\ `eggs_per_batch`

## Infectious Biting

The number of bites on humans, per patch, per day is \\fqM\\, and the
number of infectious bites, per patch, per day is \\fqZ\\.

## References

- Macdonald G (1952) The analysis of the sporozoite rate. Tropical
  Diseases Bulletin 49:569-586.

- Aron JL, May RM (1982) The population dynamics of malaria. Chapter 5
  in *The Population Dynamics of Infectious Diseases: Theory and
  Applications,* Springer, Boston, MA.

- Wu SL, *et al.* (2023) Spatial dynamics of malaria transmission. PLoS
  Computational Biology 19(6): e1010684.
  <https://doi.org/10.1371/journal.pcbi.1010684>
