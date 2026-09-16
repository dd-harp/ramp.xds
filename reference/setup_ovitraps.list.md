# Set up ovitraps

If an options list is passed as the first argument, then set

- `OTname = name$name`

- `options = name` and call
  `setup_ovitraps(OTname, xds_obj, options, s)`

## Usage

``` r
# S3 method for class 'list'
setup_ovitraps(name, xds_obj, options = list(), s = 1)
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

an **`xds`** object
