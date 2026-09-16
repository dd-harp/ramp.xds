# Set up a travel eir

Pass a pre-configured travel eir . If it passes the checks, it replaces
the current travel_eir .

If called with `name = "as_is"`, the travel eir must be at
`options$travel_eir`

## Usage

``` r
# S3 method for class 'as_is'
setup_travel_eir(name, xds_obj, options = list(), i = 1)
```

## Arguments

- name:

  a or setup function name

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- i:

  the host species index

## Value

an **`xds`** object
