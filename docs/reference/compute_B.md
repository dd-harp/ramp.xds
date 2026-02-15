# Compute Vertebrate Host Availability for Blood Feeding

Compute the availability all vertebrate hosts for blood feeding by
mosquitoes

## Usage

``` r
compute_B(W, visitors, other_blood)
```

## Arguments

- W:

  availability of the parasite's / pathogen' local hosts

- visitors:

  availability of *visitors,* or non-resident host populations

- other_blood:

  availability of other vertebrate hosts

## Value

host availability, a [vector](https://rdrr.io/r/base/vector.html)

## Details

The availability of other vertebrate hosts is a sum of available local
hosts \\W\\, and visitors \\W\_\delta\\, and other available vertebrate
hosts, \\O\\. Total availability is a simple sum: \$\$B = W +
W\_\delta + O\$\$

## References

Wu SL, Henry JM, Citron DT, Ssebuliba DM, Nsumba JN, C HMS, Brady OJ,
Guerra CA, García GA, Carter AR, Ferguson HM, Afolabi BE, Hay SI, Jr
RCR, Kiware S, Smith DL (2023). “Spatial dynamics of malaria
transmission.” *PLoS Computational Biology*, **19**(6), e1010684.
[doi:10.1371/journal.pcbi.1010684](https://doi.org/10.1371/journal.pcbi.1010684)
, [2023-06-27](https://dd-harp.github.io/ramp.xds/reference/2023-06-27).
