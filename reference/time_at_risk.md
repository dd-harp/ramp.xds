# Time at Risk

- \\\Psi\\:

  time-at-risk matrix

## Circadian Pattern Function

A circadian pattern function is a model for relative mosquito activity
by time of day, \\F_c(t)\\. On average, it should be true that
\$\$\int\_{t}^{t+1} F_c(s) ds \approx 1.\$\$ A circadian function is
used to weight time spent by time of day to compute time at risk.

## Time at Risk

The time at risk matrix describes time spent in the patches weighted by
relative mosquito activity rates.

The time at risk matrix \\(\Psi)\\ is computed from the time spent
matrix \\(\Theta)\\ and the circadian mosquito activity function
\\F_c(t)\\: \$\$\Psi = F_c(t) \Theta\$\$

Both \\\Theta\\ and \\\Psi\\ have dimensions `nPatches` \\\times\\
`nStrata` for a host species. Time at risk is computed for each
vector-host species pair. By default, \\F_c(t)=1\\, so \\\Psi=\Theta\\.

The time-at-risk matrix is used to compute human availability \\(W_H)\\
and the biting distribution matrix \\(\beta)\\.

## See also

[xds_info_time_spent](https://dd-harp.github.io/ramp.xds/reference/time_spent_matrix.md),
[F_TaR](https://dd-harp.github.io/ramp.xds/reference/F_TaR.md),
[setup_F_circadian](https://dd-harp.github.io/ramp.xds/reference/setup_F_circadian.md),
[time_away](https://dd-harp.github.io/ramp.xds/reference/time_away.md),
[available_blood_hosts](https://dd-harp.github.io/ramp.xds/reference/available_blood_hosts.md)
