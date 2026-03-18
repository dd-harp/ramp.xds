# Parsing

After solving a system of equations using
[xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md),
the outputs are parsed and attached to `xds_object$outputs`.

Use `get_*_orbits` to inspect the orbits.

**Note:** `xds_object$outputs` are overwritten each time
[xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)
is called.

## Internals

1.  Each module defines a method `parse_*_orbits`

2.  [xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)
    calls
    [parse_outputs](https://dd-harp.github.io/ramp.xds/reference/parse_outputs.md)
    which calls `parse_*_orbits` for each component and for each species

3.  Outputs are parsed and stored on the xds object as `xds_obj$outputs`

    - `time` is stored as `xds_obj$outputs$time`

    - the full output matrix is stored as `xds_obj$outputs$deout`

    - the last state of the system is stored as `mod$outputs$last_y`

    - several dynamical terms (`EIR`, `PR`, `fqZ`, `kappa`) are computed
      and attached as orbits

    - the state variables are extracted from the output matrix by name

    - the terms and variables are attached as named lists to
      `xds_obj$outputs$orbits.`

      - `XH` holds parsed outputs for the **XH** component

      - `MY` holds parsed outputs for the **MY** component

      - `L` holds parsed outputs for the **L** component
