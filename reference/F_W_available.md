# Compute Host available for Blood Feeding

Compute local human/host availability in the patches.

## Usage

``` r
F_W_available(search_weights, H, TaR)
```

## Arguments

- search_weights:

  blood feeding search weights for the host strata

- H:

  host density, adjusted for the fraction of time not traveling

- TaR:

  the time-at-risk matrix for this vector-host species pair

## Value

a numeric vector of local availability, one value per patch

## Human Availability

Blood feeding is modeled as an interaction between hosts that spend time
in patches and mosquitoes that search for blood hosts. Each host
population stratum has a per-capita search weight \\(w)\\. The fraction
of time spent outside the patches is time away \\(\delta)\\ (see
[time_away](https://dd-harp.github.io/ramp.xds/reference/time_away.md)),
so the population density present in the spatial domain is
\\(1-\delta)H\\.

The time-spent matrix \\(\Theta)\\ describes how hosts allocate their
time among patches. Ambient host population density by patch is \$\$A =
\Theta \cdot ((1-\delta)H),\$\$ where vector multiplication is
element-wise.

The time-at-risk matrix \\(\Psi)\\ weights time spent by relative
mosquito activity over the day (see
[time_at_risk](https://dd-harp.github.io/ramp.xds/reference/time_at_risk.md)).
Human availability in patches is \$\$W = \Psi \cdot (w(1-\delta)H),\$\$
where the product inside the parentheses is element-wise. Search weights
are specific to host species and shared across vector species.
Availability can differ by vector species because each can have a
different time-at-risk matrix.

## References

Wu SL, Henry JM, Citron DT, Ssebuliba DM, Nsumba JN, C HMS, Brady OJ,
Guerra CA, García GA, Carter AR, Ferguson HM, Afolabi BE, Hay SI, Jr
RCR, Kiware S, Smith DL (2023). “Spatial dynamics of malaria
transmission.” *PLoS Computational Biology*, **19**(6), e1010684.
[doi:10.1371/journal.pcbi.1010684](https://doi.org/10.1371/journal.pcbi.1010684)
. [2023-06-27](https://dd-harp.github.io/ramp.xds/reference/2023-06-27).

## See also

[available_blood_hosts](https://dd-harp.github.io/ramp.xds/reference/available_blood_hosts.md),
[F_B_available](https://dd-harp.github.io/ramp.xds/reference/F_B_available.md)
