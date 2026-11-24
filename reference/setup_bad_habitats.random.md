# Set up bad habitats

Generate random bad habitat availability using
[make_bad_habitats_random](https://dd-harp.github.io/ramp.xds/reference/make_bad_habitats_random.md)
and apply it using
[change_bad_habitats](https://dd-harp.github.io/ramp.xds/reference/change_bad_habitats.md).

## Usage

``` r
# S3 method for class 'random'
setup_bad_habitats(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  a setup function name

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- s:

  the vector species index

## Value

an **`xds`** model object
