# Set up the module object for `hMoI` (**XH**)

Set up an `hMoI` module object for the **XH** component and the
\\i^{th}\\ host species.

- Set the module name to `hMoI`.

- Create the module object with
  [make_XH_obj_hMoI](https://dd-harp.github.io/ramp.xds/reference/make_XH_obj_hMoI.md),
  passing `nStrata[i]` and `options`.

- Set initial values using
  [setup_XH_inits](https://dd-harp.github.io/ramp.xds/reference/setup_XH_inits.md),
  passing `options` to override defaults.

- Initialize blood search weights using
  [setup_blood_search_weights.setup](https://dd-harp.github.io/ramp.xds/reference/setup_blood_search_weights.setup.md).

- Initialize the time-spent matrix using
  [setup_timespent.setup](https://dd-harp.github.io/ramp.xds/reference/setup_timespent.setup.md).

- Initialize time away using
  [setup_time_away.setup](https://dd-harp.github.io/ramp.xds/reference/setup_time_away.setup.md).

- Initialize travel EIR using
  [setup_travel_eir.no_travel](https://dd-harp.github.io/ramp.xds/reference/setup_travel_eir.no_travel.md).

- Return the updated **`xds`** model object.

## Usage

``` r
# S3 method for class 'hMoI'
setup_XH_obj(XHname, residence, xds_obj, options = list(), i = 1)
```

## Arguments

- XHname:

  the model name

- residence:

  the residence vector

- xds_obj:

  an **`xds`** model object

- options:

  model options as a named list

- i:

  the host species index

## Value

an **`xds`** model object

## Note

The module object is stored at `xds_obj$XH_obj[[i]]`

## State Variables

- `m1` or \\m_1\\:

  mean MoI for all infections

- `m2` or \\m_2\\:

  mean MoI for apparent (detectable) infections

## Parameters

- `r1` or \\r_1\\:

  clearance rate for all infections

- `r2` or \\r_2\\:

  clearance rate for apparent infections, \\r_2 \> r_1\\

- `b`:

  probability of infection per infectious bite

- `c1` or \\c_1\\:

  probability of transmitting from m1

- `c2` or \\c_2\\:

  probability of transmitting from m2

- H:

  human or host population density

## See also

[hMoI](https://dd-harp.github.io/ramp.xds/reference/hMoI.md),
[make_XH_obj_hMoI](https://dd-harp.github.io/ramp.xds/reference/make_XH_obj_hMoI.md)
