# Build a Model of Immature Mosquito Ecology

A modified version of
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
that streamlines setup for an **L** Component when the **MY** Component
is set to `trivial.` The model also sets **X** Component to the
`trivial` module.

The **`xds`** object defines `frame = class(frame) = 'aquatic'` to
dispatch
[xde_derivatives.aquatic](https://dd-harp.github.io/ramp.xds/reference/xde_derivatives.aquatic.md)
or
[dts_update.aquatic](https://dd-harp.github.io/ramp.xds/reference/dts_update.aquatic.md)

## Usage

``` r
xds_setup_aquatic(
  xds = "ode",
  nHabitats = 1,
  Lname = "basicL",
  Loptions = list(),
  MYoptions = list(),
  model_name = "unnamed"
)
```

## Arguments

- xds:

  is `ode` or `dde` or `dts` for ordinary OR delay differential OR
  difference equations

- nHabitats:

  is the number of habitats

- Lname:

  is a character string defining a **L** Component module

- Loptions:

  a list to configure the **L** Component module

- MYoptions:

  a list to configure
  [F_eggs.trivial](https://dd-harp.github.io/ramp.xds/reference/F_eggs.trivial.md)

- model_name:

  is a name for the model (arbitrary)

## Value

an **`xds`** object

## See also

[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
