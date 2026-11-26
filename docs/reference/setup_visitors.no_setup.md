# Set up no visitors

- `visitors` or \\(W_V\\):

  the availability of non-resident humans in each patch

Visitor availability contributes to total availability for searching
mosquitoes, alongside resident hosts, other blood hosts, and blood
traps. In transmission, it is used with resident-host availability to
compute the local fraction.

## Usage

``` r
# S3 method for class 'no_setup'
setup_visitors(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  a numeric vector, a setup function name, or an options list

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- s:

  the vector species index

## Value

an **`xds`** model object

## Details

Don't change anything.

## See also

[visitor_availability](https://dd-harp.github.io/ramp.xds/reference/visitor_availability.md),
[get_visitors](https://dd-harp.github.io/ramp.xds/reference/get_visitors.md),
[change_visitors](https://dd-harp.github.io/ramp.xds/reference/change_visitors.md),
[malaria_importation](https://dd-harp.github.io/ramp.xds/reference/malaria_importation.md),
[update_visitors](https://dd-harp.github.io/ramp.xds/reference/update_visitors.md)
