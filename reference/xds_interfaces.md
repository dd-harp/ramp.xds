# Dynamical Interfaces

In the modular design, the
[dynamical_components](https://dd-harp.github.io/ramp.xds/reference/dynamical_components.md)
interact through two well-designed interfaces:

- **Infection Dynamics:** the **X** and **Y** components communicate
  through an interface with three parts:

  - **Blood Feeding** –

  - **Transmission** –

  - **Exposure** –

- **Mosquito Ecology:** the **L** and **M** components communicate
  through an interface with three parts:

  - **Patch Dynamics** – adult mosquito populations are distributed in
    patches (see
    [patch_dynamics](https://dd-harp.github.io/ramp.xds/reference/patch_dynamics.md))

  - **Aquatic Habitats** – aquatic habitats are located in patches (see
    [aquatic_habitats](https://dd-harp.github.io/ramp.xds/reference/aquatic_habitats.md))

  - **Egg Laying** –
