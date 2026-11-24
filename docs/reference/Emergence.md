# Compute emerging adults

Compute emerging adults at time \\(t\\).

## Usage

``` r
Emergence(t, y, xds_obj)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** model object

## Emergence

During setup, the habitat matrix \\((\mathbf{N})\\) is built from the
habitat membership vector by
[make_habitat_matrix](https://dd-harp.github.io/ramp.xds/reference/make_habitat_matrix.md).
The function Emergence calls
[F_emerge](https://dd-harp.github.io/ramp.xds/reference/F_emerge.md) to
get the adult mosquitoes emerging from each habitat \\((\alpha)\\) and
computes and stores emergence by patch (`Lambda` or \\(\Lambda\\)):
\$\$\Lambda = \mathbf{N} \cdot \alpha\$\$

## See also

[xds_info_interfaces](https://dd-harp.github.io/ramp.xds/reference/xds_info_interfaces.md)
