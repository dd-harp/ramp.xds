# Mosquito Dispersal

Mosquito dispersal is handled in a standard way across all **MY**
modules.

- A mosquito dispersal matrix, called \\K\\ or `K_matrix`, is set up
  using
  [setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md);

- \\K\\ is used to make a mosquito demographic matrix, called \\\Omega\\
  or `Omega` (see
  [mosquito_demography](https://dd-harp.github.io/ramp.xds/reference/mosquito_demography.md)).

## Setup

- `K_matrix`:

  either a \\K\\ matrix: or options for
  [setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)

There are several ways to configure \\K\\ during basic setup.

By **default:** `K_matrix` is an empty list. When the `MY_obj` is set
up, the \\K\\ matrix is set to `diag(g)`, a model with no dispersal.
That is not changed unless one of the following holds:

## Mosquito Dispersal Matrix, \\K\\

In adult mosquito modules, mosquito dispersal is described by a square
matrix \\K\\ with `nPatches` \\(=n)\\ rows and columns. By convention,
all \\K\\ matrices have the form: \$\$ K = \left\[ \begin{array}{ccccc}
-1 & k\_{1,2} & k\_{1,3} & \cdots & k\_{1, n} \\ k\_{2,1} & -1 &
k\_{2,3} & \cdots & k\_{2, n} \\ k\_{3,1} & k\_{3,2} & -1 & \cdots &
k\_{3, n} \\ \vdots & \vdots & \vdots & \ddots & \vdots \\ k\_{n,1} &
k\_{n,2} & k\_{n,3} & \cdots & -1 \\ \end{array} \right\] \$\$ The
values of the elements \\k\_{i,j}\\ are thus constrained such that there
is no net dispersal loss from the system: \\\forall i,\\ \$\$\sum_j
k\_{i,j} = 1.\$\$ The columns of \\K\\ sum up to zero. \\K\\ thus
describes the destinations of emigrating mosquitoes that survive and
stay in the system.

Emigration rates and emigration-related loss – emigration from the
spatial domain and mortality that is conditioned on emigration – are
handled separately (see
[mosquito_demography](https://dd-harp.github.io/ramp.xds/reference/mosquito_demography.md)).

## See also

[setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md),
[change_K_matrix](https://dd-harp.github.io/ramp.xds/reference/change_K_matrix.md)
&
[mosquito_demography](https://dd-harp.github.io/ramp.xds/reference/mosquito_demography.md)
