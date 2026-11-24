# Set up EIP survival-and-dispersal matrix port (**MY**)

Set up the EIP survival-and-dispersal matrix port.

## Usage

``` r
setup_Upsilon_obj(xds_obj, s = 1, dede = FALSE)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

- dede:

  is it a delay differential equation that needs \\\Upsilon\\

## Value

an **`xds`** model object

## Note

Set up the port object `Upsilon_obj` with class `dde` when `dede` is
TRUE, or `na` otherwise. The class of `Upsilon_obj` controls dispatch
for
[change_Upsilon](https://dd-harp.github.io/ramp.xds/reference/change_Upsilon.md).
