# The `basicL` module for the L component

A basic model of aquatic mosquito ecology with density-dependent
mortality and delayed maturation, based on Smith DL, *et al.* (2013).

## State Variables

- `L`:

  density of mosquito larvae in each habitat

## Parameters

- `psi` or \\\psi\\:

  maturation rate

- `xi` or \\\xi\\:

  delayed maturation parameter in response to mean crowding

- `phi` or \\\phi\\:

  density-independent mortality rate

- `theta` or \\\theta\\:

  slope of density-dependent mortality

## Dynamical Term

- `eta` or \\\eta\\:

  the number of eggs laid in each habitat, per day

## ODE

The derivatives in
[dLdt.basicL](https://dd-harp.github.io/ramp.xds/reference/dLdt.basicL.md)
are: \$\$dL/dt = \eta - (\psi e^{-\xi L} + \phi + \theta L) L\$\$

## DTS

The one-day update in
[Update_Lt.basicL](https://dd-harp.github.io/ramp.xds/reference/Update_Lt.basicL.md)
is: \$\$L_t = \eta + e^{-(\psi e^{-\xi L\_{t-1}} + \phi + \theta
L\_{t-1})} L\_{t-1}\$\$

## Emergence

The emergence rate of adult female mosquitoes from each habitat is
computed by
[F_emerge.basicL](https://dd-harp.github.io/ramp.xds/reference/F_emerge.basicL.md):
\$\$\alpha = \psi e^{-\xi L} L\$\$

## Regulation

In this model, population is regulated in two ways. First, per-capita
mortality increases with mean crowding; per-capita mortality is \\\phi +
\theta L.\\ Second, maturation is delayed in response to mean crowding
\\\psi\\e^{-\xi L}\\. Depending on the values of \\\xi,\\ productivity
in some habitats might not be a monotonically increasing function of egg
laying.

The model by Smith DL, *et al.* (2013) did not include delayed
maturation; that model is recovered by setting \\\xi=0.\\

## References

Smith DL, Perkins TA, Tusting LS, Scott TW, Lindsay SW (2013). “Mosquito
population regulation and larval source management in heterogeneous
environments.” *PLoS ONE*, **8**(8), e71247.
[doi:10.1371/journal.pone.0071247](https://doi.org/10.1371/journal.pone.0071247)
.

## See also

[make_L_obj_basicL](https://dd-harp.github.io/ramp.xds/reference/make_L_obj_basicL.md),
[setup_L_obj.basicL](https://dd-harp.github.io/ramp.xds/reference/setup_L_obj.basicL.md)
