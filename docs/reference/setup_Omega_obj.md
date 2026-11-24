# Set up mosquito demographic matrix port (**MY**)

Set up the mosquito demographic matrix port.

Options for `change_Omega` are:

- `xde` — for most differential equation systems

- `dts` — for most discrete time systems

Options for behavioral state models are in `ramp.library`

Options for `update_Omega` are:

- `static` — for autonomous systems

- `dynamic` — for non-autonomous systems

## Usage

``` r
setup_Omega_obj(ch, up, xds_obj, s = 1)
```

## Arguments

- ch:

  is `xde`, `dts`

- up:

  is `static` or `dynamic`

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

an **`xds`** model object

## Note

The Omega object stores the change and update options. The Omega object
controls dispatch for
[update_Omega](https://dd-harp.github.io/ramp.xds/reference/update_Omega.md).
The
[change_Omega](https://dd-harp.github.io/ramp.xds/reference/change_Omega.md)
generic dispatches on `xds_obj$xds`.
