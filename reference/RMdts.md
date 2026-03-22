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
