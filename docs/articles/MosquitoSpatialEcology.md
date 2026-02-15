# Mosquito Spatial Ecology with \`ramp.xds\`

``` r

library(ramp.xds)
```

In **`ramp.xds`,** mosquito spatial ecology is implemented in a
patch-based simulation model: a spatial domain is subdivided into
patches; and each patch contains an arbitrary number of habitats.

Suppose there are 5 patches and 15 habitats: 5 habitats in patch 1; no
habitats in patch 2; 3 habitats in patch 3; and 4 habitats in patch 4;
and 2 habitats in patch 5.

``` r

membership = c(rep(1,5), rep(3, 3), rep(4,5), rep(5,2))
membership
```

    ##  [1] 1 1 1 1 1 3 3 3 4 4 4 4 4 5 5

``` r

calJ = make_residency_matrix(5, membership)
```

The model for egg-laying is motivated by the notion of mosquito
searching. Each habitat is assigned a *search weight* that is related to
how easy it is for the mosquito to find it. Functions can be configured
to compute mosquito *emigration* rates, \\\sigma,\\ as a function of
habitat availability in the patch, the sum of the patch’s habitat search
weights.

``` r

set.seed(23)
searchQ = rlnorm(15, log(0.5), 1.2)
searchQ
```

    ##  [1] 0.6304683 0.2967795 1.4959678 4.3013053 1.6533094 1.8886106 0.3581330
    ##  [8] 1.6987614 0.5280192 3.3127820 0.6497283 0.1424179 0.3536054 0.8911105
    ## [15] 0.1161586

``` r

Q <- F_Q(calJ, searchQ)
```

``` r

sigma <- exp(-Q/2)
```

``` r

x = runif(5, -1, 1)
y = runif(5, -1, 1)
ker = function(d){exp(-d)}
K_matrix = make_K_matrix_xy(cbind(x,y), ker=ker)
K_matrix
```

    ##        [,1]      [,2]      [,3]      [,4]      [,5]
    ## 1 0.0000000 0.1718465 0.2682655 0.2862690 0.1655605
    ## 2 0.1754472 0.0000000 0.1413541 0.1436189 0.5330968
    ## 3 0.3151158 0.1626328 0.0000000 0.4509289 0.1450906
    ## 4 0.3534971 0.1737070 0.4740391 0.0000000 0.1562521
    ## 5 0.1559399 0.4918137 0.1163413 0.1191831 0.0000000

``` r

model5 <- xds_setup(nPatches=5, membership=membership, searchQ=searchQ, K_matrix=K_matrix)
```

``` r

round(model5$ML_interface$habitat_matrix[[1]]*1000)/1000
```

    ## [1] 1
