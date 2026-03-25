# Dynamical Interfaces

In the modular design, the
[xds_info_dynamical_components](https://dd-harp.github.io/ramp.xds/reference/xds_info_dynamical_components.md)
interact through two well-designed interfaces:

- **Infection Dynamics:** the **X** and **Y** components communicate
  through an interface with three parts:

  - **Blood Feeding** – a coherent framework for modeling blood feeding
    based on the concept of blood host availability. Models can set
    blood feeding rates without using it, but the parameter values
    should satisfy some constraints:

    - the blood feeding rate is 0 (\\f=0\\) if there are no hosts
      available;

    - the human fraction is 0 (\\q=0\\) if there are no humans
      available;

    - the human fraction is 1 (\\q=1\\) if only humans are available;

  - **Transmission** – under the blood feeding model, compute the biting
    distribution matrix (\\\beta\\) that guarantees every bite and blood
    meal is accounted for: it updates the local fraction (if there are
    visitors), and then outputs the local daily entomological
    inoculation rate (EIR or \\E\\) for all the strata; and the net
    infectiousness (NI or \\\kappa\\) in each patch.

  - **Exposure** – compute the force of infection (FoI) from:

    - the local daily EIR (\\E\\)

    - the travel EIR (\\\delta\\)

    - a model of partial immunity (\\b\\) from the **X** component

- **Mosquito Ecology:** the **L** and **M** components communicate
  through an interface with three parts:

  - **Patch Dynamics** – adult mosquito populations are distributed in
    patches (see
    [xds_info_patch_dynamics](https://dd-harp.github.io/ramp.xds/reference/xds_info_patch_dynamics.md))

  - **Aquatic Habitats** – aquatic habitats are located in patches (see
    [xds_info_aquatic_habitats](https://dd-harp.github.io/ramp.xds/reference/xds_info_aquatic_habitats.md))

  - **Egg Laying** – The egg laying interface uses search weights to
    distribute eggs laid by adults in a patch to the aquatic habitats in
    that patch
