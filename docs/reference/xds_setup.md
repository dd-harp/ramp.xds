# Build a Model and Configure All Components

Make an **`xds`** *model object*:

- Define the dynamical components:

  - **XH** - Component modules for human / host infection dynamics of
    class **`Xname`** with trivial demographics

  - **MY** - Component modules for adult mosquito ecology and infection
    dynamics of class **`MYname`**

  - **L** Component modules for aquatic mosquito ecology of class
    **`Lname`**

- Define basic structural parameters for a single host and vector
  population:

  - \\n_p\\ or `nPatches` - the number of patches

  - \\n_q\\ or `nHabitats = length(membership)` - the numer and
    locations of aquatic habitats

  - \\n_h\\ or `nStrata = length(residence)` - the number of human /
    host population strata and basic demographic information

- Configure some of the basic elements

  - Search weights for human population strata

  - Search weights for aquatic habitats

  - The mosquito dispersal matrix, \\K\\

  - The time spent matrix \\\Theta\\

Advanced options can be configured after basic setup.

## Usage

``` r
xds_setup(
  xds = "ode",
  Xname = "SIS",
  XHoptions = list(),
  MYname = "macdonald",
  MYoptions = list(),
  Lname = "trivial",
  Loptions = list(),
  nPatches = 1,
  HPop = 1000,
  residence = 1,
  membership = 1,
  searchB = 1,
  TimeSpent = list(),
  K_matrix = list(),
  searchQ = 1,
  BFopts = list(),
  model_name = "unnamed"
)
```

## Arguments

- xds:

  is `ode` or `dde` or `dts` for ordinary OR delay differential OR
  difference equations

- Xname:

  a character string defining a **X** Component module

- XHoptions:

  a list to configure the **X** Component module

- MYname:

  a character string defining an **MY** module

- MYoptions:

  options to set up the **MY** component

- Lname:

  a character string defining a **L** Component module

- Loptions:

  a list to configure the **L** Component module

- nPatches:

  is the number of patches

- HPop:

  is the number of humans in each patch

- residence:

  is a vector that describes the patch where each human stratum lives

- membership:

  is a vector that describes the patch where each aquatic habitat is
  found

- searchB:

  is a vector of search weights for blood feeding

- TimeSpent:

  is either a TimeSpent matrix or a string to call a function that sets
  it up

- K_matrix:

  is either a K_matrix matrix or a string that defines how to set it up

- searchQ:

  is a vector of search weights for egg laying

- BFopts:

  a list to configure the blood feeding model

- model_name:

  is a name for the model (arbitrary)

## Value

an **`xds`** object

## Details

1.  Using the basic structural parameters, a basic template is created
    by
    [make_xds_object_template](https://dd-harp.github.io/ramp.xds/reference/make_xds_object_template.md)
    with a properly configured interface for blood feeding and egg
    laying, and `xds_obj$frame = class(xds_obj$frame) = 'full'` .

    - `nPatches` is passed as a parameter

    - `nHabitats` is configured by passing the habitat `membership`
      vector, and `nHabitats = length(membership)`

    - `nStrata` is configured by passing a vector of human population
      densities and a residence vector, and
      `nStrata = length(residence) = length(HPop)`

    - `nHostSpecies=1` (basic setup handles only the first host species)

    - `nVectorSpecies=1` (basic setup handles only the first vector
      species)

. 2. Each one of the dynamical components is configured.

- **`xds_obj$XH_obj[[1]]`** defines a model for human / host infection
  dynamics of class `Xname` (the **X** component). The parameter values
  passed in a named list, `XHoptions.`

- **`xds_obj$MY_obj[[1]]`** defines a model for adult mosquito ecology &
  infection dynamics of class `MYname` (the **MY** Component). The
  parameter values are passed in a named list, `MYoptions.`

- **`xds_obj$L_obj[[1]]`** defines a model for aquatic mosquito ecology
  of class `Lname` (The **L** Component). The parameter values are
  passed in a named list, `Loptions.`

1.  After configuring the dynamical components, several structural
    parameters can be configured at the command line:

    - Habitat search weights can be set

    - Host population search weights can be set

    - A mosquito dispersal matrix, \\\cal K\\, can be set

    - A time spent matrix, \\\Theta\\, can be set

Advanced features must be configured later, including:

- multiple-host species or multiple-vector species

- exogenous forcing by weather, resources, or other factors

- vector control, vaccines, or other mass

## Note

If the **MY** Component is the `trivial` module, consider using
[xds_setup_aquatic](https://dd-harp.github.io/ramp.xds/reference/xds_setup_aquatic.md)
or
[xds_setup_human](https://dd-harp.github.io/ramp.xds/reference/xds_setup_human.md)
or
[xds_setup_eir](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md).
Models for mosquito ecology need not include states describing infection
status (see
[xds_setup_mosy](https://dd-harp.github.io/ramp.xds/reference/xds_setup_mosy.md)).

## See also

[make_xds_object_template](https://dd-harp.github.io/ramp.xds/reference/make_xds_object_template.md)
