# Initialize exposure objects and infection-rate terms

Initialize a Poisson exposure object for the first host species and
empty FoI and AR storage. Called by
[make_xds_object_template](https://dd-harp.github.io/ramp.xds/reference/make_xds_object_template.md).

## Usage

``` r
setup_exposure_obj(xds_obj)
```

## Arguments

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** model object

## See also

Also, see
[F_foi.pois](https://dd-harp.github.io/ramp.xds/reference/F_foi.pois.md)
and
[F_ar.pois](https://dd-harp.github.io/ramp.xds/reference/F_ar.pois.md)
