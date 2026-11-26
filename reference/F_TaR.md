# Compute time at risk

Computes time at risk from time spent \\\Theta\\ and a circadian pattern
function evaluated at simulation time \\t\\.

## Usage

``` r
F_TaR(t, TiSp, F_circadian)
```

## Arguments

- t:

  simulation time in days, passed directly to the circadian function

- TiSp:

  a time spent matrix

- F_circadian:

  a function that returns relative mosquito activity at time \\t\\

## Value

a time-at-risk matrix

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

## References

Wu SL, Henry JM, Citron DT, Ssebuliba DM, Nsumba JN, C HMS, Brady OJ,
Guerra CA, García GA, Carter AR, Ferguson HM, Afolabi BE, Hay SI, Jr
RCR, Kiware S, Smith DL (2023). “Spatial dynamics of malaria
transmission.” *PLoS Computational Biology*, **19**(6), e1010684.
[doi:10.1371/journal.pcbi.1010684](https://doi.org/10.1371/journal.pcbi.1010684)
. [2023-06-27](https://dd-harp.github.io/ramp.xds/reference/2023-06-27).

## See also

[time_spent_matrix](https://dd-harp.github.io/ramp.xds/reference/time_spent_matrix.md),
[setup_timespent](https://dd-harp.github.io/ramp.xds/reference/setup_TimeSpent.md),
[setup_F_circadian](https://dd-harp.github.io/ramp.xds/reference/setup_F_circadian.md),
[human_availability](https://dd-harp.github.io/ramp.xds/reference/human_availability.md)
