# Compute the derivatives for `hMoI` (**XH**)

The derivatives for the hybrid MoI (`hMoI`) module are:
\$\$\begin{array}{rl} dm_1/dt &= h-r_1m_1 \\ dm_2/dt &= h-r_2m_2
\end{array}\$\$ Here \\h\\ is the force of infection (`foi`).

## Usage

``` r
# S3 method for class 'hMoI'
dXHdt(t, y, xds_obj, i)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector

## See also

[hMoI](https://dd-harp.github.io/ramp.xds/reference/hMoI.md)
