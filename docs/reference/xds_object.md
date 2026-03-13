# The **`xds`** Model Object

A dynamical system is encoded and stored on an **`xds`** model object, a
compound [list](https://rdrr.io/r/base/list.html) with a specific set of
features.

A properly constructed xds object is returned by
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
or one of its variants (see
[xds_object_frame](https://dd-harp.github.io/ramp.xds/reference/xds_object_frame.md)).
The object is created in several steps:

- a basic template for an xds object is returned by
  [make_xds_object_template](https://dd-harp.github.io/ramp.xds/reference/make_xds_object_template.md),
  which

  - sets the value of structural parameters;

  - creates empty lists to hold the module objects: `XH_obj` `MY_obj,`
    and `L_obj`;

  - sets up empty
    [port](https://dd-harp.github.io/ramp.xds/reference/port.md)s and
    [junction](https://dd-harp.github.io/ramp.xds/reference/junction.md)s;

- the core dynamical components are setup; and

- the values of a few modifiable parameters affecting transmission are
  set.

Use functions to set up (`setup_*`), inspect (`get_*`), and modify
(\`change\_\*) various aspects of xds objects.
