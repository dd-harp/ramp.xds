# Human Availability

- \\(W_H\\):

  local human/host availability in patches

## Human Availability

Blood feeding is modeled as an interaction between hosts that spend time
in patches and mosquitoes that search for blood hosts. Each host
population stratum has a per-capita search weight \\((w)\\). The
fraction of time spent outside the patches is time away \\((\delta)\\)
(see
[time_away](https://dd-harp.github.io/ramp.xds/reference/time_away.md)),
so the population density present in the spatial domain is
\\((1-\delta)H\\).

The time-spent matrix \\((\Theta)\\) describes how hosts allocate their
time among patches. Ambient host population density by patch is \$\$A =
\Theta \cdot ((1-\delta)H),\$\$ where vector multiplication is
element-wise.

The time-at-risk matrix \\((\Psi)\\) weights time spent by relative
mosquito activity over the day (see
[time_at_risk](https://dd-harp.github.io/ramp.xds/reference/time_at_risk.md)).
Human availability in patches is \$\$W_H = \Psi \cdot
(w(1-\delta)H),\$\$ where the product inside the parentheses is
element-wise. Search weights are specific to host species and shared
across vector species. Availability can differ by vector species because
each can have a different time-at-risk matrix.

## See also

[available_blood_hosts](https://dd-harp.github.io/ramp.xds/reference/available_blood_hosts.md),
[F_W_available](https://dd-harp.github.io/ramp.xds/reference/F_W_available.md),
[time_spent_matrix](https://dd-harp.github.io/ramp.xds/reference/time_spent_matrix.md),
[time_at_risk](https://dd-harp.github.io/ramp.xds/reference/time_at_risk.md)
