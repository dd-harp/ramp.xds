# Mosquito Spatial Ecology with \`ramp.xds\`

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`ramp.xds`](https://dd-harp.github.io/ramp.xds/)`)`

In **`ramp.xds`,** mosquito spatial ecology is implemented in a
patch-based simulation model: a spatial domain is subdivided into
patches; and each patch contains an arbitrary number of habitats.

Suppose there are 5 patches and 15 habitats: 5 habitats in patch 1; no
habitats in patch 2; 3 habitats in patch 3; and 4 habitats in patch 4;
and 2 habitats in patch 5.

\
`membership`` ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`[`rep`](https://rdrr.io/r/base/rep.html)`(``1``,``5``)``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``3``, ``3``)``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``4``,``5``)``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``5``,``2``)``)`\
`membership`

    ##  [1] 1 1 1 1 1 3 3 3 4 4 4 4 4 5 5

\
`N`` ``=`` `[`make_habitat_matrix`](https://dd-harp.github.io/ramp.xds/reference/make_habitat_matrix.md)`(``5``, ``membership``)`

The model for egg-laying is motivated by the notion of mosquito
searching. Each habitat is assigned a *search weight* that is related to
how easy it is for the mosquito to find it. Functions can be configured
to compute mosquito *emigration* rates, \\(\sigma,\\) as a function of
habitat availability in the patch, the sum of the patch’s habitat search
weights.

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``23``)`\
`searchQ`` ``=`` `[`rlnorm`](https://rdrr.io/r/stats/Lognormal.html)`(``15``, `[`log`](https://rdrr.io/r/base/Log.html)`(``0.5``)``, ``1.2``)`\
`searchQ`

    ##  [1] 0.6304683 0.2967795 1.4959678 4.3013053 1.6533094 1.8886106 0.3581330
    ##  [8] 1.6987614 0.5280192 3.3127820 0.6497283 0.1424179 0.3536054 0.8911105
    ## [15] 0.1161586

\
`Q`` ``<-`` `[`F_available_habitat`](https://dd-harp.github.io/ramp.xds/reference/F_available_habitat.md)`(``N``, ``searchQ``)`

\
`sigma`` ``<-`` `[`exp`](https://rdrr.io/r/base/Log.html)`(``-``Q``/``2``)`

\
`x`` ``=`` `[`runif`](https://rdrr.io/r/stats/Uniform.html)`(``5``, ``-``1``, ``1``)`\
`y`` ``=`` `[`runif`](https://rdrr.io/r/stats/Uniform.html)`(``5``, ``-``1``, ``1``)`\
`FkV`` ``=`` ``function``(``d``, ``V``=`[`list`](https://rdrr.io/r/base/list.html)`(``)``)``{`[`exp`](https://rdrr.io/r/base/Log.html)`(``-``d``)``}`\
`K_matrix`` ``=`` `[`make_K_matrix_xy`](https://dd-harp.github.io/ramp.xds/reference/make_K_matrix_xy.md)`(`[`cbind`](https://rdrr.io/r/base/cbind.html)`(``x``,``y``)``, F_K``=``FkV``)`\
`K_matrix`

    ##         [,1]       [,2]       [,3]       [,4]       [,5]
    ## 1 -1.0000000  0.1718465  0.2682655  0.2862690  0.1655605
    ## 2  0.1754472 -1.0000000  0.1413541  0.1436189  0.5330968
    ## 3  0.3151158  0.1626328 -1.0000000  0.4509289  0.1450906
    ## 4  0.3534971  0.1737070  0.4740391 -1.0000000  0.1562521
    ## 5  0.1559399  0.4918137  0.1163413  0.1191831 -1.0000000

\
`model5`` ``<-`` `[`xds_setup`](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)`(``Lname ``=`` ``"basicL"``, nPatches``=``5``, membership``=``membership``, searchQ``=``searchQ``, Koptions``=``K_matrix``)`

\
[`round`](https://rdrr.io/r/base/Round.html)`(``model5``$``habitats``$``laying_matrix``[[``1``]``]``*``1000``)``/``1000`

    ##        [,1] [,2]  [,3]  [,4]  [,5]
    ##  [1,] 0.075    0 0.000 0.000 0.000
    ##  [2,] 0.035    0 0.000 0.000 0.000
    ##  [3,] 0.179    0 0.000 0.000 0.000
    ##  [4,] 0.513    0 0.000 0.000 0.000
    ##  [5,] 0.197    0 0.000 0.000 0.000
    ##  [6,] 0.000    0 0.479 0.000 0.000
    ##  [7,] 0.000    0 0.091 0.000 0.000
    ##  [8,] 0.000    0 0.431 0.000 0.000
    ##  [9,] 0.000    0 0.000 0.106 0.000
    ## [10,] 0.000    0 0.000 0.664 0.000
    ## [11,] 0.000    0 0.000 0.130 0.000
    ## [12,] 0.000    0 0.000 0.029 0.000
    ## [13,] 0.000    0 0.000 0.071 0.000
    ## [14,] 0.000    0 0.000 0.000 0.885
    ## [15,] 0.000    0 0.000 0.000 0.115
