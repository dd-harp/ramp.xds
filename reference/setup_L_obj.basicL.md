# Set up the module object for `basicL` (**L**)

Set up a `basicL` module object for the **L** component and the
\\s^{th}\\ vector species.

- Create the module object with
  [make_L_obj_basicL](https://dd-harp.github.io/ramp.xds/reference/make_L_obj_basicL.md),
  passing `nHabitats` and `options`.

- Set the skill set using
  [get_skillset_L](https://dd-harp.github.io/ramp.xds/reference/get_skillset_L.md).

- Set initial values using
  [setup_L_inits](https://dd-harp.github.io/ramp.xds/reference/setup_L_inits.md),
  passing `options` to override defaults.

- Initialize habitat search weights using
  [setup_habitat_search_weights.setup](https://dd-harp.github.io/ramp.xds/reference/setup_habitat_search_weights.setup.md).

- Return the updated **`xds`** model object.

## Usage

``` r
# S3 method for class 'basicL'
setup_L_obj(Lname, membership, xds_obj, options = list(), s = 1)
```

## Arguments

- Lname:

  the class name of the **L** module

- membership:

  the habitats' patch membership vector

- xds_obj:

  an **`xds`** model object

- options:

  a named list to configure **`L_obj`**

- s:

  the species index

## Value

an **`xds`** model object

## Note

The module object is stored at `xds_obj$L_obj[[s]]` Its classes are
`basicL` and `basicL_` followed by `xds_obj$xds`.

## See also

[basicL](https://dd-harp.github.io/ramp.xds/reference/basicL.md),
[make_L_obj_basicL](https://dd-harp.github.io/ramp.xds/reference/make_L_obj_basicL.md)
