# Mosquito Bionomics

The values of some mosquito bionomic parameters (generically, \\x\\) are
computed in two stages, with vector control effect sizes computed
between them:

- [MosquitoBionomics](https://dd-harp.github.io/ramp.xds/reference/MosquitoBionomics.md)
  computes a *baseline* value, \\x_b\\

- Vector control functions compute and store the *net effect size,*
  \\e_x\\

- [VectorControlEffectSizes](https://dd-harp.github.io/ramp.xds/reference/VectorControlEffectSizes.md)
  computes the realized value \$\$x = x_b \times e_x\$\$

The parameter values can be modified by vector control through one of
two distinct approaches, called *integrated effects* or *independent
effect sizes*.

In models with integrated effects, \\x_b\\ could include some effects of
vector control, so it may not be correct to call it a baseline, strictly
speaking.

## See also

[xds_info_port](https://dd-harp.github.io/ramp.xds/reference/xds_info_port.md)
