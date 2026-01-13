# Make an **`xds`** model object template

This function is called by
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
to create a structured object, called an **`xds`** model object
template. The template has configured interfaces, created stubs for the
model objects, created junctions, and set up empty ports. A template has
set up all the basic structural elements, but it has not configured any
of the component model objects.

## Usage

``` r
make_xds_object_template(
  xds = "ode",
  frame = "full",
  nPatches = 1,
  membership = 1,
  residency = 1
)
```

## Arguments

- xds:

  is used to dispatch various functions to set up and solve systems of
  differential equations. 'xde' for ordinary or delay differential
  equations; 'dts' for "discrete time systems"

- frame:

  model component subset

- nPatches:

  is the number of patches

- membership:

  is the habitat membership vector

- residency:

  is the strata residency vector

## Value

an `xds` model object

## Details

This function sets up the interfaces and the core objects used by
**`ramp.xds.`**

First, the function sets up some short text strings (assigned to the
same `S3` class) to dispatch various **cases** of of various `S3`
functions:

- **`xds`** is either "xde" for differential equations, or "dts" for
  discrete time systems

- **`xde`** is either "ode" for ordinary differential equations; "dde"
  for delay differential equations; or "dts" for discrete time systems

- **`frame`** is one of several cases:

  - "full" includes all three dynamical components: a human/host
    dynamical component, \\\cal XH\\; and adult mosquito dynamical
    component, \\\cal MYZ\\; and an aquatic mosquito dynamical
    component, \\\cal L\\. in some form (possibly the trivial case) (see
    [`xds_setup()`](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md))

  - "mozy" is for mosquito ecology models (see
    [`xds_setup_mosy()`](https://dd-harp.github.io/ramp.xds/reference/xds_setup_mosy.md)),
    including models without pathogen infection dynamics in mosquitoes

  - "aquatic" is for aquatic mosquito ecology models (see
    [`xds_setup_aquatic()`](https://dd-harp.github.io/ramp.xds/reference/xds_setup_aquatic.md)),
    forced by a function describing egg laying

  - "human" is for human/host infection dynamics(see
    [`xds_setup_human()`](https://dd-harp.github.io/ramp.xds/reference/xds_setup_human.md)),
    forced by the infective density of adult mosquitoes, \\fqZ\\

  - "eir" is for human/host cohort infection dynamics (see
    [`xds_setup_eir()`](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md)),
    forced by a function `F_eir`

- **`forcing`** is set to "static"

Second, the function sets the values of the **structural parameters**:

- **`nPatches`** or \\N_p\\ is the number of patches

- **`nHabitats`** or \\N_q\\, the number of aquatic habitats, is set to
  `length(membership)`

- **`nStrata`** or \\N_h\\, the number of human/ host population strata,
  is set to `length(HPop)`

- **`nVectorSpecies`** or \\N_s\\, the number of vector species is set
  to 1;

- **`nHostSpecies`** or \\N_i\\, the number of host species is set to 1;

Next, the function sets up empty lists to hold the model objects that
define components:

- `XH_obj`

- 

- **Transmission** calls
  [`setup_transmission()`](https://dd-harp.github.io/ramp.xds/reference/setup_TRANSMISSION.md)

model for the availability of visitors; by default, there are no
visitors Next, the function sets up egg laying, blood feeding, and
transmission:

- **Egg Laying** calls
  [`make_habitat_matrix()`](https://dd-harp.github.io/ramp.xds/reference/make_habitat_matrix.md),
  then
  [`setup_ML_interface()`](https://dd-harp.github.io/ramp.xds/reference/setup_ML_interface.md)

- **Blood Feeding** calls
  [`make_residency_matrix()`](https://dd-harp.github.io/ramp.xds/reference/make_residency_matrix.md),
  then
  [`setup_XY_interface()`](https://dd-harp.github.io/ramp.xds/reference/setup_XY_interface.md)

- **Transmission** calls
  [`setup_transmission()`](https://dd-harp.github.io/ramp.xds/reference/setup_TRANSMISSION.md)
  sets up a static model for the availability of visitors; by default,
  there are no visitors

Finally, the function sets up a few other miscellaneous options:

- [Exposure](https://dd-harp.github.io/ramp.xds/reference/Exposure.md)
  is called *after*
  [Transmission](https://dd-harp.github.io/ramp.xds/reference/Transmission.md)
  to compute environmentally heterogeneous exposure and malaria
  importation through travel:

  - [setup_exposure](https://dd-harp.github.io/ramp.xds/reference/setup_exposure.md)
    sets up a Poisson model for environmental heterogeneity

  - [setup_travel_object](https://dd-harp.github.io/ramp.xds/reference/setup_travel_object.md)
    sets up a model with no exposure through travel

## Note

`xds` stands for extensible differential equation

## See also

Related:
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
and
[setup_forcing_object](https://dd-harp.github.io/ramp.xds/reference/setup_forcing_object.md).
Illustrated in a vignette: [5-3-4
Example](https://dd-harp.github.io/ramp.xds/articles/ex_534.html)
