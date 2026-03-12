# Mosquito Dispersal

Mosquito Dispersal

## Mosquito Dispersal Matrix, \\K\\

In adult mosquito modules, mosquito dispersal is described by a square
matrix \\K\\ with `nPatches` \\(=n)\\ rows and columns. By convention,
all \\K\\ matrices have the form: \$\$ K = \left\[ \begin{array}{ccccc}
-1 & k\_{1,2} & k\_{1,3} & \cdots & k\_{1, n} \\ k\_{2,1} & -1 &
k\_{2,3} & \cdots & k\_{2, n} \\ k\_{3,1} & k\_{3,2} & -1 & \cdots &
k\_{3, n} \\ \vdots & \vdots & \vdots & \ddots & \vdots \\ k\_{n,1} &
k\_{n,2} & k\_{n,3} & \cdots & -1 \\ \end{array} \right\] \$\$ We
interpret \\K\\ as the destinations of emigrating mosquitoes that
survive and stay in the system. The values of the elements \\k\_{i,j}\\
are thus constrained such that there is no net dispersal loss from the
system: \\\forall i,\\ \$\$\sum_j k\_{i,j} = 1.\$\$ The columns of \\K\\
sum up to zero.

Emigration-related loss (including mortality and emigration from the
spatial domain) is modeled with another parameter (see
[mosquito_demography](https://dd-harp.github.io/ramp.xds/reference/mosquito_demography.md)).

## Basic Setup

- `K_matrix`:

  a matrix or a named list: options that set up \\K\\

There are several ways to configure \\K\\ during basic setup.

By **default:** `K_matrix` is an empty list. When the `MY_obj` is set
up, the \\K\\ matrix is set to `diag(g)`, a model with no dispersal.
That is not changed unless one of the following holds:

- `is.matrix(K_matrix)`: if the user to passes a proper \\K\\ matrix,
  then it is used as-is

- `with(Kmatrix, exists("Kname"))`: If a string in the named list passed
  as `K_matrix` is called `Kname`, then basic setup calls
  [setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)
  with `options=K_matrix`

## See also

[setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)
