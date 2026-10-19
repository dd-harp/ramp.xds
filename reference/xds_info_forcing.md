# Forcing in **`ramp.xds`**

In **`ramp.xds`**, forcing is implemented through *ports* and
*junctions,* or through exogenous forcing of dynamical terms by
`trivial` modules (see
[xds_info_trivial_forcing](https://dd-harp.github.io/ramp.xds/reference/xds_info_trivial_forcing.md)).

- Parameter or terms that could be time-varying are implemented as
  *ports.* There are several kinds of ports:

  - mosquito bionomic ports set a baseline that can be modified by
    vector control;

  - pharmaceutical ports are used to model mass treatment;

  - terms and parameters affecting blood feeding, transmission, exposure
    or habitat dynamics are implemented as ports; and

  - other exogenous variables are implemented as ports.

- Junctions are functions that activate ports (see
  [xds_info_port](https://dd-harp.github.io/ramp.xds/reference/xds_info_port.md)):

  - [`Forcing`](https://dd-harp.github.io/ramp.xds/reference/Forcing.md)
    handles weather, hydrology, system shocks, and development;

  - [`Importation`](https://dd-harp.github.io/ramp.xds/reference/Importation.md)
    sets up time away, the travel EIR, visitors, and their
    infectiousness;

  - [`Health`](https://dd-harp.github.io/ramp.xds/reference/Health.md)
    sets up variables associated with health systems;

  - Vector control is implemented in two parts;

  - [`Resources`](https://dd-harp.github.io/ramp.xds/reference/Resources.md)
    sets up variables associated with mosquito ecology that are not part
    of blood feeding or egg laying;

  - Four junctions implement mosquito bionomics: two set the baseline
    (for adult and immature mosquitoes) and two modify the baseline with
    vector control *effect sizes.*

## See also

[xds_info_trivial_forcing](https://dd-harp.github.io/ramp.xds/reference/xds_info_trivial_forcing.md);
[xds_info_vector_control](https://dd-harp.github.io/ramp.xds/reference/xds_info_vector_control.md);
[xds_info_health](https://dd-harp.github.io/ramp.xds/reference/xds_info_health.md)
