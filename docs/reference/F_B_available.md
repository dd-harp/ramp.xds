# Compute total blood host availability

Compute total availability to blood-seeking mosquitoes.

## Usage

``` r
F_B_available(W, visitors, other_blood, traps)
```

## Arguments

- W:

  local human/host availability

- visitors:

  visitor availability

- other_blood:

  availability of other vertebrate hosts

- traps:

  blood trap availability

## Value

a numeric vector of total availability, one value per patch

## Details

Total availability sums local human/host availability, visitors, other
blood hosts, and blood traps: \$\$B = W + V + O + T.\$\$

## References

Wu SL, Henry JM, Citron DT, Ssebuliba DM, Nsumba JN, C HMS, Brady OJ,
Guerra CA, García GA, Carter AR, Ferguson HM, Afolabi BE, Hay SI, Jr
RCR, Kiware S, Smith DL (2023). “Spatial dynamics of malaria
transmission.” *PLoS Computational Biology*, **19**(6), e1010684.
[doi:10.1371/journal.pcbi.1010684](https://doi.org/10.1371/journal.pcbi.1010684)
, [2023-06-27](https://dd-harp.github.io/ramp.xds/reference/2023-06-27).
