# Setup the Omega obj

The Omega object handles dispatching for `change_Omega` and
`update_Omega`

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

an **xds** model object
