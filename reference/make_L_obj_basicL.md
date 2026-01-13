# Make `L_obj` for `basicL` (**L** Component)

The following parameters will be set to the values in `options.` If they
are not found, default values will be used.

- \\\psi\\ or `psi`: maturation rate

- \\\xi\\ or `xi`: delayed maturation response due to mean crowding

- \\\phi\\ or `phi`: density-independent death rate

- \\\theta\\ or `theta`: density dependence in mortality: the slope of
  the response to mean crowding

## Usage

``` r
make_L_obj_basicL(
  nHabitats,
  options = list(),
  psi = 1/8,
  xi = 0,
  phi = 1/8,
  theta = 1/100
)
```

## Arguments

- nHabitats:

  the number of habitats in the model

- options:

  a named [list](https://rdrr.io/r/base/list.html)

- psi:

  maturation rates for each aquatic habitat

- xi:

  delayed maturation in response to mean crowding

- phi:

  density-independent mortality rates for each aquatic habitat

- theta:

  density-dependent mortality terms for each aquatic habitat

## Value

**`L_obj`** an **L** component object

## See also

Called by:
[setup_L_obj.basicL](https://dd-harp.github.io/ramp.xds/reference/setup_L_obj.basicL.md).
Related:
[dLdt.basicL](https://dd-harp.github.io/ramp.xds/reference/dLdt.basicL.md)
&
[Update_Lt.basicL](https://dd-harp.github.io/ramp.xds/reference/Update_Lt.basicL.md)
