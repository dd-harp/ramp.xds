# Delay Equations

------------------------------------------------------------------------

In this short vignette, we explain how **`ramp.xds`** computes lagged
parameters and terms while solving delay differential equations.

------------------------------------------------------------------------

This software is designed to solve delay differential equations using
[`deSolve::dede`](https://rdrr.io/pkg/deSolve/man/dede.html). Solving
delay differential equations requires computing lagged values of
variables, parameters or terms. The lagged values of variables can be
computed by
[`deSolve::lagvalue;`](https://rdrr.io/pkg/deSolve/man/timelags.html)
but there was a need to compute lagged values of parameters and terms
that had been computed outside of the module. This presented a design
challenge for modular computing.

The solution was to create *accessory variables* and use
[`deSolve::lagderiv`](https://rdrr.io/pkg/deSolve/man/timelags.html) to
internalize computation of any lagged terms. For any lagged term
\\x(t-\tau)\\, we create a new variable \\X\\ and compute
\\\frac{dX}{dt} = x.\\ The variable \\X\\ is indexed and initialized
like other dependent variables. Lagged values of \\x\\ are a simple call
to [`deSolve::lagderiv`](https://rdrr.io/pkg/deSolve/man/timelags.html)
for \\X.\\ We call \\X\\ an accessory variable, because it is not
describe the state of the system.

## Example

A model published by Aron and May in 1982 includes terms to describe the
dynamics of infected (\\Y\\) and infectious (\\Z\\) mosquitoes. It is
system of delay differential equations. To write it down, we use a
subscript to denote the lagged value of a quantity: example, \\Y\_\tau =
Y(t-\tau).\\ The model assumes mosquitoes become infected exactly
\\\tau\\ days after becoming infected: the mosquitoes that became
infected \\\tau\\ time units ago are \\f\_\tau q\_\tau \kappa\_\tau
(M\_\tau - Y\_\tau)\\ and a fraction has survived \\\tau\\ days (with
probability \\e^{-g\tau}\\). With time-dependent forcing terms, the
dynamics are:

\\ \begin{array}{rl} \frac{dY}{dt} &= fq\kappa (M-Y) - g Y \\
\frac{dZ}{dt} &= e^{-g\tau} f\_\tau q\_\tau \kappa\_\tau
(M\_\tau-Y\_\tau) - g Z \\ \end{array} \\

To implement this, we would need the value of \\f\_\tau\\, \\q\_\tau\\,
\\\kappa\_\tau\\ and \\M\_\tau\\ and \\Y\_\tau.\\ In the software
implementation \\M\\ and \\Y\\ are variables, so we could use
[`deSolve::lagvalue`](https://rdrr.io/pkg/deSolve/man/timelags.html),
but the value of \\\kappa\\ is computed and stored in `Transmission`.
The values of \\f\_\tau\\ and \\q\_\tau\\ are computed by the adult
mosquito module.

One way to compute the parameters and terms (let’s call it the external
design solution for lagged values) is to compute and store the value of
lagged terms in the module where they are computed, so `Transmission`
would compute and store \\\kappa\_\tau\\ and the adult mosquito module
would need to compute and store the values of \\f\_\tau\\ and
\\q\_\tau.\\ The need to compute these lagged values would need to be
conveyed to dependent modules.

The internal design solution for lagged values creates an accessory
variable \\K\\ where \\dK/dt = fq\kappa (M-Y).\\ The variable is indexed
in the same way as the other variables, and it is computed in dMYdt just
like the other variables. In the function call,
[`deSolve::lagderiv`](https://rdrr.io/pkg/deSolve/man/timelags.html) is
called to retrieve the lagged value which is the lagged derivative of
the variable \\K\\ at time \\t-\tau\\, or \\f\_\tau q\_\tau
\kappa\_\tau(M\_\tau - Y\_\tau).\\
