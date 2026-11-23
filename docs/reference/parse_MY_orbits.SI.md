# Parse outputs for `SI` (**MY**)

Parse the `SI` state outputs and compute infective mosquito density,
infected and infective fractions, and parity.

## Usage

``` r
# S3 method for class 'SI'
parse_MY_orbits(outputs, xds_obj, s)
```

## Arguments

- outputs:

  a [matrix](https://rdrr.io/r/base/matrix.html) of model state outputs

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

## Infective Biting

**Infective biting density** is defined as the number of human
infectious bites by the mosquito population in each patch, each day. In
`SI`, infective biting density is computed if infected mosquitoes at
time \\(t\\), \\(Y(t),\\) had survived and dispersed through an EIP of
\\(\tau\\) days \\(e^{- \Omega \tau}\\).

- \\(Z\\):

  density of infectious mosquitoes, imputed from \\(Y\\)

- `fqZ`:

  Infective biting density: the number of infectious bites, per patch,
  per day \$\$fqZ = e^{-\Omega \tau} \cdot \left( f q Y \right) \$\$

- `fqM`:

  Human biting density: the number of bites on humans, per patch, per
  day

Note that \\(e^{-\Omega \tau}\\) is computed with the matrix exponential
function [expm](https://rdrr.io/pkg/expm/man/expm.html)

**The EIR**

\\(fqZ\\) is the dynamical term computed by
[F_fqZ.SI](https://dd-harp.github.io/ramp.xds/reference/F_fqZ.SI.md). It
is called by
[compute_EIR](https://dd-harp.github.io/ramp.xds/reference/compute_EIR.md)
(in
[Transmission](https://dd-harp.github.io/ramp.xds/reference/Transmission.md))
to compute and store `EIR`, the local daily EIR \\((E)\\). Note that
`fqZ` is a vector of length `nPatches` but the local EIR is a vector of
length `nStrata`. The **XY** interface uses information about humans and
mosquitoes to build the biting distribution matrix, \\(\beta,\\) and if
there are no visitors, then \$\$E = \beta \cdot fqZ\$\$ The orbits of
`fqZ` and `EIR` are computed and stored by
[xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)
by the functions that parse the outputs.

**The HBR**

Since estimates of the EIR are computed by taking the product of the
human biting rate (HBR or \\(B\\)) and the sporozoite rate, there is
interest in knowing \\(fqM\\) and \\(B.\\) The quantity `fqM` is
computed by
[F_fqM.SI](https://dd-harp.github.io/ramp.xds/reference/F_fqM.SI.md).
The orbits of `fqM` and `HBR` are also computed during parsing, as \$\$B
= \beta \cdot fqM\$\$

## See also

[SI](https://dd-harp.github.io/ramp.xds/reference/SI.md)
