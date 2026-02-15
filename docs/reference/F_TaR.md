# Compute blood feeding available of all vertebrate hosts

Computes available all vertebrate hosts to blood feeding mosquitoes

## Usage

``` r
F_TaR(t, TiSp, F_circadian, time_at_home)
```

## Arguments

- t:

  the time

- TiSp:

  a time spent matrix

- F_circadian:

  a function to compute relative activity rates by time of day

- time_at_home:

  the fraction of time spent at home

## Value

a TaR [matrix](https://rdrr.io/r/base/matrix.html)

## Details

The available of other vertebrate hosts is a sum of available local
hosts \\W\\, and visitors \\W\_\delta\\, and other available vertebrate
hosts, \\O\\. Total available is a simple sum: \$\$B = W + W\_\delta +
O\$\$

## References

Wu SL, Henry JM, Citron DT, Ssebuliba DM, Nsumba JN, C HMS, Brady OJ,
Guerra CA, García GA, Carter AR, Ferguson HM, Afolabi BE, Hay SI, Jr
RCR, Kiware S, Smith DL (2023). “Spatial dynamics of malaria
transmission.” *PLoS Computational Biology*, **19**(6), e1010684.
[doi:10.1371/journal.pcbi.1010684](https://doi.org/10.1371/journal.pcbi.1010684)
, [2023-06-27](https://dd-harp.github.io/ramp.xds/reference/2023-06-27).
