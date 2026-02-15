# Resources

Computes availability of resources potentially affecting adult mosquito
bionomics: blood hosts, aquatic habitats, and sugar. There are ports to
compute variables that are part of the blood feeding, egg laying, or
exposure interfaces.

- [BloodHosts](https://dd-harp.github.io/ramp.xds/reference/BloodHosts.md)
  availability of alternative blood hosts

- [HabitatDynamics](https://dd-harp.github.io/ramp.xds/reference/HabitatDynamics.md)
  to modify habitat search weights

- [Travel](https://dd-harp.github.io/ramp.xds/reference/Travel.md) to
  compute variables associated with exposure while traveling

- [Visitors](https://dd-harp.github.io/ramp.xds/reference/Visitors.md)
  to compute parasite/pathogen blood feeding on visitors

- [Traps](https://dd-harp.github.io/ramp.xds/reference/Traps.md) to
  compute availability of oviposition traps

- [Sugar](https://dd-harp.github.io/ramp.xds/reference/Sugar.md)
  availability of sugar

## Usage

``` r
Resources(t, y, xds_obj)
```

## Arguments

- t:

  the time

- y:

  the state variables

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** model object
