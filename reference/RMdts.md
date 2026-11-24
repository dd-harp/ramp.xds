# The `RMdts` module for the MY component

Implements the **MY** component using a discrete-time Macdonald model of
adult mosquito ecology and infection dynamics.

## State Variables

- `M`:

  density of adult mosquitoes

- `Y`:

  density of infected adult mosquitoes

- `Z`:

  density of infectious adult mosquitoes

## Dynamical Terms

- `Lambda` or \\\Lambda\\:

  adult mosquito emergence, supplied by
  [Emergence](https://dd-harp.github.io/ramp.xds/reference/Emergence.md)

- `kappa` or \\\kappa\\:

  net infectiousness, supplied by
  [Transmission](https://dd-harp.github.io/ramp.xds/reference/Transmission.md)

## Parameters

- `f`:

  blood feeding rate

- `q`:

  human blood fraction

- `eip`:

  extrinsic incubation period (time steps)

- `g`:

  mosquito mortality rate

- `sigma`:

  emigration rate

- `mu`:

  emigration loss rate

- `K`:

  mosquito dispersal matrix

- `Omega`:

  adult mosquito demographic matrix (mortality + migration)

- `Upsilon`:

  survival and dispersal through the eip

## Dynamics

State is updated each time step via
[Update_MYt.RMdts](https://dd-harp.github.io/ramp.xds/reference/Update_MYt.RMdts.md).

## DTS

[Update_MYt.RMdts](https://dd-harp.github.io/ramp.xds/reference/Update_MYt.RMdts.md)
updates `M`, `P`, `U`, the cohort matrix `Y`, and `Z`. It uses the
stored `Omega` as a one-step matrix and `G` to weight progression out of
the infected cohorts. Cohort columns are merged and replaced after
computing survival and progression.

## Egg Laying

The daily number of eggs laid per patch is `M*nu*eggsPerBatch`, where
`M` is total adult mosquito density, `nu` is the oviposition rate, and
`eggsPerBatch` is the number of eggs per batch.
[F_eggs.RMdts](https://dd-harp.github.io/ramp.xds/reference/F_eggs.RMdts.md)
returns this quantity before distribution among aquatic habitats by
[EggLaying](https://dd-harp.github.io/ramp.xds/reference/EggLaying.md).
