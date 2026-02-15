# Make an **`xds`** model object Template

Creates and returns structured template for an **`xds`** model object.
The returned model object has set up generic features and placeholders
for options that must be configured to fully define an **`xds`** model
object

## Usage

``` r
make_xds_template(
  xds = "ode",
  frame = "full",
  nPatches = 1,
  membership = 1,
  residence = 1
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

- residence:

  is the strata residence vector

## Value

an `xds` model object

## Details

This function sets up the basic structures required to configure and run
a basic model. The returned model object is a list with various required
elements attached and configured, but without specifying the dynamical
components or any advanced features.

First, the function sets up some short text strings (assigned to the
same `S3` class) to dispatch various **cases** of of various `S3`
functions:

- **`xds`** is either "ode" or "dde" for ordinary / delay differential
  equations; or "dts" for difference equations;

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

  - "cohort" is for human/host cohort infection dynamics (see
    [`xds_setup_cohort()`](https://dd-harp.github.io/ramp.xds/reference/xds_setup_cohort.md)),
    forced by a function `F_eir`

- **`forcing`** is set to "static"

Second, the function sets the values of the **structural parameters**:

- **`nVectorSpecies`** or \\n_s\\, the number of vector species is set
  to 1;

- **`nHostSpecies`** or \\n_i\\, the number of host species is set to 1;

- **`nPatches`** or \\n_p\\ is the number of patches

- **`nHabitats`** or \\n_q\\, the number of aquatic habitats, is set to
  `length(membership)`

- **`nStrata`** or \\n_h\\, the number of human/ host population strata,
  is set to `length(HPop)`

Next, the function sets up empty lists to hold the model objects that
define components:

- `Xpar`

- 

- **Transmission** calls
  [`setup_TRANSMISSION()`](https://dd-harp.github.io/ramp.xds/reference/setup_TRANSMISSION.md)

model for the availability of visitors; by default, there are no
visitors Next, the function sets up egg laying, blood feeding, and
transmission:

- **Egg Laying** calls
  [`create_habitat_matrix()`](https://dd-harp.github.io/ramp.xds/reference/create_habitat_matrix.md),
  then
  [`setup_EGG_LAYING()`](https://dd-harp.github.io/ramp.xds/reference/setup_EGG_LAYING.md)

- **Blood Feeding** calls
  [`create_residence_matrix()`](https://dd-harp.github.io/ramp.xds/reference/create_residence_matrix.md),
  then
  [`setup_BLOOD_FEEDING()`](https://dd-harp.github.io/ramp.xds/reference/setup_BLOOD_FEEDING.md)

- **Transmission** calls
  [`setup_TRANSMISSION()`](https://dd-harp.github.io/ramp.xds/reference/setup_TRANSMISSION.md)
  sets up a static model for the availability of visitors; by default,
  there are no visitors

Finally, the function sets up a few other miscellaneous options:

- [Exposure](https://dd-harp.github.io/ramp.xds/reference/Exposure.md)
  is called *after*
  [Transmission](https://dd-harp.github.io/ramp.xds/reference/Transmission.md)
  to compute environmentally heterogeneous exposure and malaria
  importation through travel:

  - [setup_exposure_pois](https://dd-harp.github.io/ramp.xds/reference/setup_exposure_pois.md)
    sets up a Poisson model for environmental heterogeneity

  - [setup_no_travel](https://dd-harp.github.io/ramp.xds/reference/setup_no_travel.md)
    sets up a model with no exposure through travel

## See also

Related:
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
and
[setup_no_forcing](https://dd-harp.github.io/ramp.xds/reference/setup_no_forcing.md).
Illustrated in a vignette: [5-3-4
Example](https://dd-harp.github.io/ramp.xds/articles/ex_534.html)
