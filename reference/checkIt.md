# Check the length of an input value

Check the length of an input value

## Usage

``` r
checkIt(x, lng, type = "numeric", fixit = TRUE)
```

## Arguments

- x:

  a [numeric](https://rdrr.io/r/base/numeric.html) input

- lng:

  the required length

- type:

  a [character](https://rdrr.io/r/base/character.html) string specifying
  whether to convert the input to integer

- fixit:

  a [logical](https://rdrr.io/r/base/logical.html) value; if `TRUE`,
  replicate a length-one input to length `lng`

## Value

the checked [numeric](https://rdrr.io/r/base/numeric.html) input
