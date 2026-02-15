# Compute Host Availability for Blood Feeding

Compute the availability of the population strata defined in the model
as hosts for blood feeding by mosquitoes

## Usage

``` r
compute_W(search_weights, H, TaR)
```

## Arguments

- search_weights:

  blood feeding search weights for the host strata

- H:

  host density

- TaR:

  the host species index

## Value

host availability, a [vector](https://rdrr.io/r/base/vector.html)

## Details

Host availability to blood searching mosquitoes in patches is the sum of
search weights of the human strata, a vector \\\omega\\, weighted by
time at risk, defined by a matrix \\\Psi\\ that is \\n_p \times n_h\\.
The search weight is a *per-capita* measure so we weight it by human
population density, \\H\\. Availability, \\W\\ is computed as \$\$\Psi
\cdot (\omega H).\$\$

## References

Wu SL, Henry JM, Citron DT, Ssebuliba DM, Nsumba JN, C HMS, Brady OJ,
Guerra CA, García GA, Carter AR, Ferguson HM, Afolabi BE, Hay SI, Jr
RCR, Kiware S, Smith DL (2023). “Spatial dynamics of malaria
transmission.” *PLoS Computational Biology*, **19**(6), e1010684.
[doi:10.1371/journal.pcbi.1010684](https://doi.org/10.1371/journal.pcbi.1010684)
, [2023-06-27](https://dd-harp.github.io/ramp.xds/reference/2023-06-27).

## See also

Availability of all vertebrate hosts for blood feeding is computed by
[`compute_B()`](https://dd-harp.github.io/ramp.xds/reference/compute_B.md)
