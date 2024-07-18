# **`ramp.xds`**  

## **RAMP** - e**X**tensible **D**ynamical **S**ystems

<!-- badges: start -->
[![PLoS Computational Biology](https://img.shields.io/badge/doi%3A10.1371%2Fjournal.pcbi.1010684-cornflowerblue?style=for-the-badge&logoColor=cornflowerblue&label=PLoS%20Comp%20Bio&labelColor=slategrey&link=https%3A%2F%2Fjournals.plos.org%2Fploscompbiol%2Farticle%3Fid%3D10.1371%2Fjournal.pcbi.1010684)](https://doi.org/10.1371/journal.pcbi.1010684)

<!-- badges: end -->

## What is RAMP?

RAMP, or **R**obust **A**nalytics for **M**alaria **P**olicy, is a bespoke inferential system for malaria decision support and adaptive malaria control. A core goal for RAMP is to characterize, quantify, and propagate uncertainty in analysis using conventional and simulation-based analytics.  

## What is **`ramp.xds`**?

**`ramp.xds`** is an R software package that supports RAMP. It's function is to set up, analyze, and solve dynamical systems describing the epidemiology, transmission dynamics, and control of malaria and other mosquito-borne pathogens. The software was designed around a rigorous mathematical framework for modular model building, described in [Spatial Dynamics of Malaria Transmission](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1010684) (Wu SL, *et al.* 2023. PLoS Computational Biology)^[Wu SL, Henry JM, Citron DT, Mbabazi Ssebuliba D, Nakakawa Nsumba J, Sánchez C. HM, et al. (2023) Spatial dynamics of malaria transmission. PLoS Comput Biol 19(6): e1010684. https://doi.org/10.1371/journal.pcbi.1010684]. 

**`ramp.xds`** is part of a suite of R packages to support RAMP simulation-based analytics. Supporting packages include: 

+ An extended library of stable, tested, and reusable code implementing previously published model families for *ramp.xds* is maintained in [**`ramp.library`**](https://dd-harp.github.io/ramp.library/).

+ Algorithms to apply the framework, include code to fit models to data, is found in [**`ramp.work`**](https://dd-harp.github.io/ramp.work/). 

+ A large set of models illustrating capabilities are found in [**`ramp.malaria`**](https://dd-harp.github.io/ramp.malaria/). 

**`ramp.xds`**  is under active development. It is the continuation of two other software packages, now deprecated: [**`exDE`**](https://dd-harp.github.io/exDE/) and [**`MicroMoB`**](https://dd-harp.github.io/MicroMoB/). 

**NOTE:** The repository retains the name `ramp.xde` as a legacy from the past: a name change is planned from `ramp.xde` to `ramp.xds.`


## Installation

To install the latest version from GitHub, run the following lines of code in an R session.

```
library(devtools)
devtools::install_github("dd-harp/ramp.xde")
```

## Why use **`ramp.xds`**?

There are several advantages to using **`ramp.xds`**:

+ **`ramp.xds`** makes it easy to set up, solve, and apply dynamical systems models for malaria & other mosquito-borne pathogens.

+ **`ramp.library`** includes a large set of published models. 

+ All the code has been verified and tested. If you're publishing a model family that is not already in *ramp.library,* you can write us and we'll help you add it to the growing collection. 

+ It should be comparatively easy to replicate your study in *ramp.xds.* It’s also easy for others to replicate your study.

+ Any model family from one domain can be combined with models from other domains and take advantage of the other features of *ramp.xds.*  Software capabilities include: 

    + support for systems of ordinary and delay, differential and difference equations; 

    + support for autonomous and non-autonomous, including exogenous forcing by weather and vector control; 

    + structural flexibility in stratifying host population to handle heterogeneity in epidemiological traits, such as vaccination, drug taking, net ownership, travel and mobility, and host demograpy;

    + support for modeling realistic human / host demography with cohort dynamics and aging to model infection, immunity, disease, and infectiousness in host populations over by age and time; 

    + support for modeling mass vaccination, mass drug administration, and other health interventions.  

    + environmental heterogeneity in exposure;  

    + malaria importation through travel and visitors; 

    + multiple host species (or types) and multiple mosquito vector species (or types);

    + support for modeling observational processes (in *ramp.xde*) and (through *ramp.work*) model fitting.  
    
    + support for analysis, including computation of reproductive numbers 


## Modularity 

This software supports nimble model building with functions to set up, solve, and analyze dynamical systems models for mosquito ecology and mosquito-borne pathogen transmission formulated as dynamical systems in continuous or discrete time. 

The mathematical framework and software have a modular design developed around an algorithm to model mosquito blood feeding on vertebrate hosts and parasite / pathogen transmission, including patch-based spatial dynamics within a defined spatial domain. The blood feeding algorithm provides a rigorous computational interface linking dynamical components describing parasite/pathogen linking infection dynamics in mosquito and vertebrate host populations. It guarantees mathematical consistency in computing blood feeding rates and habits and in allocating bites and blood meals with heterogeneous vertebrate host population densities, including models with dynamically changing availability of host populations or other vertebrate animals. 

The framework and software also support spatial models of mosquito ecology through an algorithm that describes egg laying by adult mosquitoes in aquatic habitats and emergence of adult mosquitoes. The egg laying algorithm provides a rigorous interface linking dynamical components describing adult mosquito populations in a set of spatial patches and immature aquatic mosquito populations in a structured set of aquatic habitats. 

The framework and software were also designed to support: 

+ support for systems of ordinary and delay, differential and difference equations; 

+ support for autonomous and non-autonomous, including exogenous forcing by weather and vector control; 

+ structural flexibility in stratifying host population to handle heterogeneity in epidemiological traits, such as vaccination, drug taking, net ownership, travel and mobility, and host demograpy;

+ realistic demography with cohort dynamics and aging to model infection, immunity, disease, and infectiousness in host populations over by age and time; 

+ mass vaccination, mass drug administration, and other health interventions.  

+ environmental heterogeneity in exposure;  

+ malaria importation through travel and visitors; 

+ multiple host species (or types) and multiple mosquito vector species (or types);

+ observational processes.  

The software aims to be fully modular: it implements plug-and-play computation for a broad class of simulation models describing the dynamics and control of mosquito-transmitted pathogens. Modularity has been achieved through a combination of methods: the functions that compute derivatives (for differential equations) or that update state variables (for difference equations) use a method that dispatches on parameter lists for each component which is used to compute the full set of differential equations; the interfaces for blood feeding and egg laying are biologically realistic but rigidly defined to guarantee mathematical consistency; and a large set of additional features have been implemented with careful attention to detail to guarantee consistency and avoid conflicts. Each dynamical component also includes a null model, but where its outputs can be computed by any function. The function `ramp.xde::xDE_diffeqn` computes the gradient of all state variables from those modular components and can be used with the excellent solvers in [deSolve](http://desolve.r-forge.r-project.org/). The functions `ramp.xde::DTS_step` updates the state variables in discrete-time systems with support for mismatched runtime time steps. 

To get started, please consider reading some of the articles in the drop down panels above, at our [website](https://dd-harp.github.io/ramp.xde/). The 3 sections ending in "Component" describe particular models implementing
the interface for each of those components (adult mosquitoes, aquatic mosquitoes, and humans), and show a simulation at their equilibrium values. 

The section "Articles" has more in-depth examples, including an extended walk through of how to set up and run a model in `vignette("ex_534")`, a guide on how to contribute, and an example of running a model in ramp.xde with external forcing under a model of ITN (insecticide treated nets) based vector control in `vignette("vc_lemenach")`. 

The section "Functions" documents each function exported by the package.



## Model building in ramp.xde

Models for mosquito borne pathogen transmission systems are naturally modular, structured by
vector life stage, host population strata, and by the spatial locations (patches) at
which transmission occurs (see figure below).

<p align="center">
  <img src="vignettes/modularity.png"/>
</p>

Models in the ramp.xde framework are constructed from 3 dynamical components:

  * $\mathcal M$: adult mosquitoes, whose dynamics are described by $d\mathcal{M}/dt$
  * $\mathcal{L}$: aquatic (immature) mosquitoes, whose dynamics are described by $d\mathcal{L}/dt$
  * $\mathcal{X}$: human population, whose dynamics are described by $d\mathcal{X}/dt$
  
The combined state from these 3 components is the entire state of the dynamical model, and their combined dynamics described by their differential equations represents the full endogenous dynamics of the system. In addition there are 2 more components which do not directly contribute to the state of the model, but instead modify parameters and compute intermediate quantities to represent external influences on the system. These are:

  * Exogenous forcing: weather, climate, unmodeled populations
  * Vector control: public health and mosquito control interventions which affect the dynamical components

There are also functions which handle the exchange of information (flows) between the dynamical components and which couple their dynamics. Bloodfeeding is the process by which adult mosquitoes seek out and feed on blood hosts, and results in the quantities $EIR$ (entomological inoculation rate) and $\kappa$, the net infectiousness of humans to mosquitoes, which couple the dynamics of $\mathcal{M}$ and $\mathcal{X}$. Likewise emergence of new adults from aquatic habitats and egg laying by adults into habitats couples $\mathcal{M}$ and $\mathcal{L}$. 

The function `ramp.xde::xDE_diffeqn` compute the necessary quantities and returns a vector of derivatives of all state variables which can be used to solve trajectories from a model in ramp.xde. The program flow within this function is summarized by this diagram:

<p align="center">
  <img src="vignettes/xDEdiffeqn.png"/>
</p>

For more information, please read our [research article](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1010684) describing the theory behind the model.

## Contributing

**ramp.xds** aims to provide stable, reliable, reusable code for the study of mosquito-borne pathogen dynamics and control using dynamical systems.   

For information about how to contribute to the development of ramp.xde, please read our article on how to contribute at [Contributing](articles/Contributing.html). 

If you have any questions, comments, bug reports, or suggestions, the first point of contact with the development team is through [GitHub Issues](https://github.com/dd-harp/ramp.xde/issues). If you are specifically submitting a bug report, please check out our [bug reporting guide](https://dd-harp.github.io/ramp.xde/articles/Contributing.html#sec8). If you are interested in collaborating in extensive model development (e.g. new mosquito model), please do not hesitate to contact the authors, whose email addresses can be found in the `DESCRIPTION` file.

We welcome issues and encourage contribution regardless of experience; the length of the contributing guide is not intended to be intimidating, merely complete. It is the responsibility of the package maintainers to help new contributors understand our conventions and guide contributions to a successful conclusion.

## History

This software was designed to replace exDE and MicroMoB, which were originally designed by Sean L. Wu and Professor David L. Smith. David L Smith developed the prototypes. Sean L. Wu wrote the first versions of the R-packages. Development of exDE continued. The name exDE was changed to ramp.xde where development continued. A stable version of exDE was restored. Functionality for discrete time systems was added to ramp.xde in the spring of 2024. A name change to ramp.xds is planned. 

## Acknowledgements

The idea of developing modular software to simulate the dynamics and control of mosquito-borne pathogens originated sometime around 2009 at the Emerging Pathogens Institute, University of Florida. It took much longer than anticipated to finish. Some concepts have appeared in various publications. Some of the algorithms trace back to other software packages that were never launched, but that someday might yet be completed (*e.g.*, MASH). In retrospect, a key challenge was blood feeding, but it was not the only one. 

From its inception, this project has been supported by grants from the Bill and Melinda Gates Foundation, including (most recently) **Modeling for Adaptive Malaria Control** (INV 030600, PI = David L Smith, University of Washington). The software is part of RAMP (Robust Analytics for Malaria Policy), the core analytical methodology being developed to support adaptive malaria control. The RAMP project includes a RAMP-UW team at the University of Washington based in Seattle Washington, and the RAMP-Uganda team based in Kampala, Uganda, which provides analytics support to the National Malaria Control Division, Ministry of Health, Uganda. 

Some model features were inspired by discussions with the mosquito working groups (led by Professor Thomas Scott) of RAPIDD (Research and Policy for Infectious Disease Dynamics). Over that time, the project benefited from the unwavering support and inspiration of the late F. Ellis McKenzie, Fogarty International Center.

Development of `ramp.xde` was supported, in part, by a grant from the US National Institute of Allergies and Infectious Diseases (NIAID) to fund a project **Spatial Targeting and Adaptive Vector Control for Residual Transmission and Malaria Elimination in Urban African Settings** (R01 AI163398, PI = David L Smith), which has supported collaboration with the Bioko Island Malaria Elimination Program. Development of this software benefited from funding and collaboration with the NIAID grant **Program for Resistance, Immunology, Surveillance & Modeling of Malaria in Uganda (PRISM)** (2U19AI089674, PIs = Grant Dorsey, University of California San Francisco; and Moses Kamya, Infectious Diseases Research Collaboration), which was part of the International Centers of Excellence in Malaria Research (ICEMR) program. 

Funding to develop models of West Nile Virus to support Harris County Public Health was funded by the NSF as part of a project, **Computing the Biome** (PI= Janos Sztipanovits). The project was part of the Convergence Accelerator program of the National Science Foundation, Directorate for Technology, Innovation, and Partnerships (TIP) ([NSF 2040688 ](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2040688 ) and [NSF 2040688](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2134862), PI=Janos Sztipanovits, Vanderbilt University).


