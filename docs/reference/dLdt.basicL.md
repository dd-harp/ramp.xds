# Compute the derivatives for `basicL` (**L**)

Compute the derivatives for the `basicL` module: \$\$dL/dt =
\eta-\left(\psi e^{-\xi L}+\phi+\theta L\right)L.\$\$

## Usage

``` r
# S3 method for class 'basicL'
dLdt(t, y, xds_obj, s)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of derivatives
in the module's state-variable order, with length `nHabitats`

## Note

Derivatives for `L` are returned in habitat order. The egg input is read
from `xds_obj$terms$eta[[s]]`.

## State Variables

- `L`:

  density of mosquito larvae in each habitat

## Dynamical Term

- `eta` or \\(\eta\\):

  the number of eggs laid in each habitat, per day

## Parameters

- `psi` or \\(\psi\\):

  maturation rate

- `xi` or \\(\xi\\):

  delayed maturation parameter in response to mean crowding

- `phi` or \\(\phi\\):

  density-independent mortality rate

- `theta` or \\(\theta\\):

  slope of density-dependent mortality

## ODE

The derivatives in dLdt.basicL are: \$\$dL/dt = \eta - (\psi e^{-\xi
L} + \phi + \theta L) L\$\$

## Emergence

The emergence rate of adult female mosquitoes from each habitat is
computed by
[F_emerge.basicL](https://dd-harp.github.io/ramp.xds/reference/F_emerge.basicL.md):
\$\$\alpha = \psi e^{-\xi L} L\$\$

## Regulation

In this model, population is regulated in two ways. First, per-capita
mortality increases with mean crowding; per-capita mortality is
\\(\phi + \theta L.\\) Second, maturation is delayed in response to mean
crowding \\(\psi\\;e^{-\xi L}\\). Depending on the values of \\(\xi,\\)
productivity in some habitats might not be a monotonically increasing
function of egg laying.

The model by Smith DL, *et al.* (2013) did not include delayed
maturation; that model is recovered by setting \\(\xi=0.\\)

## References

Smith DL, Perkins TA, Tusting LS, Scott TW, Lindsay SW (2013). “Mosquito
population regulation and larval source management in heterogeneous
environments.” *PLoS ONE*, **8**(8), e71247.
[doi:10.1371/journal.pone.0071247](https://doi.org/10.1371/journal.pone.0071247)
.

## See also

[basicL](https://dd-harp.github.io/ramp.xds/reference/basicL.md),
[make_L_obj_basicL](https://dd-harp.github.io/ramp.xds/reference/make_L_obj_basicL.md),
[Update_Lt.basicL](https://dd-harp.github.io/ramp.xds/reference/Update_Lt.basicL.md),
[EggLaying](https://dd-harp.github.io/ramp.xds/reference/EggLaying.md)
