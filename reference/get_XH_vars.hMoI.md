# Get the state variables for `hMoI` (**XH**)

Extract the state variables for the `hMoI` module from `y` and return
them by name in a list for use during model solving. Also return `H`
from the module object rather than from `y`.

## Usage

``` r
# S3 method for class 'hMoI'
get_XH_vars(y, xds_obj, i)
```

## Arguments

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

a named [list](https://rdrr.io/r/base/list.html) of state variables and
module-specific derived quantities

## Note

Variable indices are stored at `xds_obj$XH_obj[[i]]$ix`.

## See also

[hMoI](https://dd-harp.github.io/ramp.xds/reference/hMoI.md)
