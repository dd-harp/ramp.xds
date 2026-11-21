# Dynamical Interfaces

In the modular design, the dynamical processes interact through two
interfaces:

## XY Interface

The **XY** interface coordinates parasite **Transmission Dynamics**
through mosquito biting and blood feeding, and computes the dynamical
terms that pass information between the **X** and **Y** processes. The
XY interface has three parts:

- [BloodFeeding](https://dd-harp.github.io/ramp.xds/reference/BloodFeeding.md)
  (see the [Blood Feeding
  vignette](https://dd-harp.github.io/ramp.xds/articles/BloodFeeding.html))
  – mosquitoes *search* for blood hosts, so the algorithms that model
  blood feeding were developed around the concept of blood host
  availability. Models can set up constant blood feeding rates, but the
  parameter values should satisfy some constraints:

  - the blood feeding rate should be 0 if there are no hosts available;

  - the human fraction should be 0 if there are no humans available;

  - the human fraction should be 1 if only humans are available;

- [Transmission](https://dd-harp.github.io/ramp.xds/reference/Transmission.md)
  (see the [Transmission
  vignette](https://dd-harp.github.io/ramp.xds/articles/Transmission.html))
  – under the blood-feeding model, the biting distribution matrix
  (\\\beta\\) ensures that every bite and blood meal is accounted for:

  - it updates the local fraction (if there are visitors)

  - it computes the local daily entomological inoculation rate (EIR or
    \\E\\) for all the strata

  - it computes net infectiousness (NI or \\\kappa\\) in each patch.

- [Exposure](https://dd-harp.github.io/ramp.xds/reference/Exposure.md)
  (see the [Exposure
  vignette](https://dd-harp.github.io/ramp.xds/articles/Exposure.html))
  – handles mosquito blood feeding from the human or host perspective.
  The function computes the force of infection (FoI) for all the
  population strata from:

  - the local daily EIR (\\E\\)

  - the travel EIR (\\\delta\\)

  - an assumption about the distribution of the expectation

  - a model of partial immunity (\\b\\) from the **X** component

## ML Interface

**Mosquito Ecology:** the **L** and **M** processes communicate through
the habitat interface. Aquatic habitats are located in the patches and
configured during basic setup by passing the habitat membership vector.
See the [Habitats and Mosquito Ecology
vignette](https://dd-harp.github.io/ramp.xds/articles/EggLaying.html).

- **Patch Dynamics** – adult mosquito populations are distributed in
  patches (see
  [xds_info_patch_dynamics](https://dd-harp.github.io/ramp.xds/reference/xds_info_patch_dynamics.md))

- **Aquatic Habitats** – aquatic habitats are located in patches (see
  [xds_info_aquatic_habitats](https://dd-harp.github.io/ramp.xds/reference/xds_info_aquatic_habitats.md))

  - [Emergence](https://dd-harp.github.io/ramp.xds/reference/Emergence.md)
    — a function that uses the habitat matrix to aggregate adults
    emerging from aquatic habitats to patches

  - [EggLaying](https://dd-harp.github.io/ramp.xds/reference/EggLaying.md)
    — a function that uses the habitat matrix and habitat search weights
    to distribute eggs laid by adults in a patch among the aquatic
    habitats in that patch.

## See also

- XY Interface

  - [xds_info_blood_feeding](https://dd-harp.github.io/ramp.xds/reference/xds_info_blood_feeding.md)

  - [xds_info_transmission](https://dd-harp.github.io/ramp.xds/reference/xds_info_transmission.md)

  - [xds_info_exposure](https://dd-harp.github.io/ramp.xds/reference/xds_info_exposure.md)

- ML Interface

  - [xds_info_emergence](https://dd-harp.github.io/ramp.xds/reference/xds_info_emergence.md)

  - [xds_info_egg_laying](https://dd-harp.github.io/ramp.xds/reference/egg_laying.md)
