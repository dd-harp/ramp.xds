# Compute derivatives for the **MY** module `macdonald`

This implements a delay differential equation model for adult mosquito
ecology and infection dynamics that is consistent with the model
published by George Macdonald in 1952. A generalized version of this
model, the **MY** module `GeRM`, was developed to handle exogenous
forcing by weather and vector control. This model should be used only
for educational purposes.

**Variables:**

- \\M\\: the density of adult mosquitoes

- \\Y\\: the density of infected adult mosquitoes

- \\Z\\: the density of infectious adult mosquitoes

**Parameters and Terms:**

- \\\Lambda\\ or `Lambda`: the emergence rate of adult mosquitoes (from
  `F_emerge`)

- \\f\\ or `f`: the blood feeding rate

- \\q\\ or `q`: maturation rate

- \\\tau\\ or `eip`: the extrinsic incubation period

- \\\Omega\\ or `Omega`: an adult mosquito demographic matrix, including
  mortality and migration

- \\\Upsilon\\ or `Upsilon`: survival and dispersal through the eip,
  \\\Upsilon= e^{-\Omega \tau}\\

**Dynamics:** In the delay equation, we use the subscript to denote the
lagged value of a variable or term: *e.g.*, \\M\_\tau = M(t-\tau)\\.

\$\$ \begin{array}{rl} dM/dt &= \Lambda - \Omega \cdot M \\ dY/dt &=
fq\kappa(M-Y) - \Omega \cdot Y \\ dZ/dt &= \Upsilon \cdot
(fq\kappa)\_\tau(M\_\tau-Y\_\tau) - \Omega \cdot Y \\ \end{array}\$\$

This model was included mainly for the historical interest. It has been
updated to handle exogenous forcing by weather and vector control in the
module `GeRM`

## Usage

``` r
# S3 method for class 'macdonald'
dMYdt(t, y, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector

## Note

This model is not capable of being extended to handle exogenous forcing
by weather or vector control. Please use the `GeRM` model.
