# The `hMoI` Module for the XH Component

Implements the **XH** component using a hybrid model that tracks the
mean Multiplicity of Infection (MoI) for total and apparent infections.
This model is not extensible to dynamic human population density or mass
treatment.

## State Variables

- `m1`:

  mean MoI for all infections

- `m2`:

  mean MoI for apparent (detectable) infections

## Parameters

- `r1`:

  clearance rate for all infections

- `r2`:

  clearance rate for apparent infections

- `b`:

  probability of infection per infectious bite

## Dynamics

\$\$ \begin{array}{rl} dm_1/dt &= h - r_1 m_1 \\ dm_2/dt &= h e^{-m_1}
(e^{m_2} - 1) - r_2 m_2 \\ \end{array}\$\$ where \\h\\ is the force of
infection computed upstream.
