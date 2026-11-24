# Available Blood Hosts

- \\(W\\):

  local human/host availability in patches

- \\(B\\):

  total availability to blood-seeking mosquitoes in patches

## Note

For one host species, \\(\left\|w\right\|=\left\|H\right\|=\\)`nStrata`
and \\(\Psi\\) has dimensions `nPatches` \\(\times\\) `nStrata`. Both
\\(W\\) and \\(B\\) have length `nPatches`.

## Total Blood Host Availability

Local human/host availability \\((W)\\) is computed as described in
[human_availability](https://dd-harp.github.io/ramp.xds/reference/human_availability.md).
Visitors, other blood hosts, and blood traps also attract blood-seeking
mosquitoes. Total availability in each patch is: \$\$B = W + V + O +
T\$\$ where \\(V\\) is visitor availability, \\(O\\) is other blood host
availability, and \\(T\\) is blood trap availability.

## See also

[human_availability](https://dd-harp.github.io/ramp.xds/reference/human_availability.md),
[time_spent_matrix](https://dd-harp.github.io/ramp.xds/reference/time_spent_matrix.md),
[time_at_risk](https://dd-harp.github.io/ramp.xds/reference/time_at_risk.md),
[xds_info_search_weights_blood](https://dd-harp.github.io/ramp.xds/reference/xds_info_search_weights_blood.md),
[mixing_matrix](https://dd-harp.github.io/ramp.xds/reference/mixing_matrix.md)
