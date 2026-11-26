# Blood Feeding in **`ramp.xds`**

- \\(f\\):

  the blood-feeding rate per mosquito

- \\(q\\):

  the fraction of blood meals on the modeled hosts and visitors

- \\(W_H\\):

  local host availability

- \\(B\\):

  total availability to blood-seeking mosquitoes

- \\(\beta\\):

  the mixing matrix

## Blood Hosts

**Hosts in Patches** — The population densities in strata are computed
by the **XH** component function
[F_H](https://dd-harp.github.io/ramp.xds/reference/F_H.md). Search
weights and time at risk determine local host availability (see
[available_blood_hosts](https://dd-harp.github.io/ramp.xds/reference/available_blood_hosts.md)).

**Other Hosts and Traps** — Local hosts compete with visitors, other
blood hosts, and blood traps for mosquito blood-seeking activity:

- \\(W_H\\) — local host availability

- \\(W_V\\) — visitor availability

- \\(W_O\\) — other blood host availability

- \\(W_T\\) — blood trap availability

\$\$B = W_H + W_V + W_O + W_T\$\$

**Mosquito Bionomics** — Blood-feeding rates and other mosquito bionomic
parameters can be modeled as functional responses to availability. For
example: \$\$f = F_f(B, \ldots)\$\$ For positive total availability, an
availability-based host fraction is: \$\$q = \frac{W_H+W_V}{B}\$\$ This
is a modeling relationship, not a constraint automatically imposed on
every **MY** module. Directly assigned bionomic parameters need not
satisfy it.

**Bites on Local Hosts** — The local fraction separates bites on local
hosts from bites on visitors: \$\$\ell = \frac{W_H}{W_H+W_V}\$\$
[F_local_frac](https://dd-harp.github.io/ramp.xds/reference/F_local_frac.md)
sets this fraction to one when \\(W_H+W_V=0\\). For a single vector
species, the human/host biting rate and daily local EIR are computed
using the
[mixing_matrix](https://dd-harp.github.io/ramp.xds/reference/mixing_matrix.md):
\$\$\mbox{HBR} = \beta \cdot (\ell f q M)\$\$ \$\$\mbox{EIR} = \beta
\cdot (\ell f q Z)\$\$ Products within the parentheses are element-wise.
With multiple vector species, their contributions are summed for each
host stratum.

The transmission interface also computes net infectiousness to
mosquitoes. Conversion of EIR to infection rates is handled separately
by [Exposure](https://dd-harp.github.io/ramp.xds/reference/Exposure.md).

## References

Wu SL, Henry JM, Citron DT, Ssebuliba DM, Nsumba JN, C HMS, Brady OJ,
Guerra CA, García GA, Carter AR, Ferguson HM, Afolabi BE, Hay SI, Jr
RCR, Kiware S, Smith DL (2023). “Spatial dynamics of malaria
transmission.” *PLoS Computational Biology*, **19**(6), e1010684.
[doi:10.1371/journal.pcbi.1010684](https://doi.org/10.1371/journal.pcbi.1010684)
, [2023-06-27](https://dd-harp.github.io/ramp.xds/reference/2023-06-27).

## See also

[BloodFeeding](https://dd-harp.github.io/ramp.xds/reference/BloodFeeding.md),
[available_blood_hosts](https://dd-harp.github.io/ramp.xds/reference/available_blood_hosts.md),
[mixing_matrix](https://dd-harp.github.io/ramp.xds/reference/mixing_matrix.md),
[xds_info_mosquito_bionomics](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_bionomics.md),
[xds_info_transmission](https://dd-harp.github.io/ramp.xds/reference/xds_info_transmission.md),
[Exposure](https://dd-harp.github.io/ramp.xds/reference/Exposure.md)
