# Delay Equations

This software is designed to handle delay differential equations.
Solving delay differential equations requires computing lagged values of
variables, parameters or terms.

The values of variables can be computed by `deSolve::lagvalue,` but in a
modular framework, the need to compute lagged values of parameters
computed by other modules presented a design challenge.

A decision was made to internalize computation of any lagged terms
through the creation of *accessory variables.* For every lagged term
\\x(t-\tau)\\, an accessory variable \\X\\ was created such that
\\\frac{dX}{dt} = x.\\ The variable \\X\\ is indexed and initialized
like other dependent variables. Lagged values of \\x\\ are a simple call
to [`deSolve::lagderiv`](https://rdrr.io/pkg/deSolve/man/timelags.html)
for \\X.\\
