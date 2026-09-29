# Make type 2 blood feeding rate configuration

Build a configuration object for the type 2 blood feeding response in
response to the availability of blood hosts: \$\$F_f(B)= f_x \frac{s_f
B}{1+s_f B}\$\$.

## Usage

``` r
make_F_f_type2(nPatches, options = list(), fx = 0.3, sf = 0.1)
```

## Arguments

- nPatches:

  is the number of patches

- options:

  is a set of options that overwrites the defaults

- fx:

  the maximum blood feeding rate, \\f_x\\

- sf:

  the shape parameter, \\s_f\\

## Value

a configuration object for
[F_f.type2](https://dd-harp.github.io/ramp.xds/reference/F_f.type2.md)
