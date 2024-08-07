# **`ramp.xds`** - e**X**tensible **D**ynamical **S**ystems 
## *Nimble model building for malaria epidemiology, spatial transmission dynamics, and control*

<!-- badges: start -->
[![PLoS Computational Biology](https://img.shields.io/badge/doi%3A10.1371%2Fjournal.pcbi.1010684-cornflowerblue?style=for-the-badge&logoColor=cornflowerblue&label=PLoS%20Comp%20Bio&labelColor=slategrey&link=https%3A%2F%2Fjournals.plos.org%2Fploscompbiol%2Farticle%3Fid%3D10.1371%2Fjournal.pcbi.1010684)](https://doi.org/10.1371/journal.pcbi.1010684)

<!-- badges: end -->

## Install **`ramp.xds`** 

To install the latest version from GitHub, run the following lines of code in an R session.

```
library(devtools)
devtools::install_github("dd-harp/ramp.xds")
```

## What is RAMP?

RAMP -- **R**obust **A**nalytics for **M**alaria **P**olicy -- is a bespoke inferential system for malaria decision support and adaptive malaria control. A core goal for RAMP is to characterize, quantify, and propagate uncertainty in analysis using conventional and simulation-based analytics.

## What is **`ramp.xds`**?

**`ramp.xds`** is an R software package that supports nimble model building for simulation-based analytics and malaria research. It was designed to set up, analyze, and solve dynamical systems describing  malaria epidemiology, spatial transmission dynamics, and control including mosquito spatial ecology. 

The software has been designed to serve the needs of malaria programs, where mathematical models are developed for decision support and adaptive malaria control. In this context, models describe malaria in a defined geographical area. Mechanistic model of malaria in a place, fitted to available data, are a synthesis of malaria intelligence in a form that can facilitate complex analysis, extending our innate mental capabilities. As the needs of a malaria program changes, the models can be modified -- simplified or extended -- to address the tasks at hand. The software was also developed to apply malaria theory: it outputs standard, observable malaria metrics, and it easily solves equations, computes steady states and stable orbits, and computes spatio-temporal vectorial capacity and the basic and adjusted reproductive numbers for malaria parasites. 

The software was designed around a rigorous mathematical framework for modular model building, described in [Spatial Dynamics of Malaria Transmission](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1010684) (Wu SL, *et al.* 2023. PLoS Computational Biology)^[Wu SL, Henry JM, Citron DT, Mbabazi Ssebuliba D, Nakakawa Nsumba J, Sánchez C. HM, et al. (2023) Spatial dynamics of malaria transmission. PLoS Comput Biol 19(6): e1010684. https://doi.org/10.1371/journal.pcbi.1010684]. The mathematical framework has now been extended to cover *exogenous forcing* by weather and vector control. 

**`ramp.xds`** is the core computational engine for simulation-based analytics. It is part of a suite of R packages. Other packages are: 

+  [**`ramp.library`**](https://dd-harp.github.io/ramp.library/) is an extended library of stable code that has been tested and verified. It includes a large set of model families published in peer review that are not included in **`ramp.xds`** The ability to reuse code reduces the costs of replicating studies. Through this library, **`ramp.xds`** also supports nimble model building and analytics for other mosquito-borne pathogens. 

+ [**`ramp.work`**](https://dd-harp.github.io/ramp.work/) includes algorithms to apply the framework, include code to fit models to data and to do constrained optimization 

+ [**`ramp.malaria`**](https://dd-harp.github.io/ramp.malaria/) includes a large set of models illustrating capabilities of **`ramp.xds`** 

**`ramp.xds`**  is under active development. It supersedes two other software packages, no longer under active development: [**`exDE`**](https://dd-harp.github.io/exDE/) and [**`MicroMoB`**](https://dd-harp.github.io/MicroMoB/). If you're interested, we've written a short [history of development of RAMP simulation software](https://dd-harp.github.io/ramp.xds/articles/History.html).

## Why use **`ramp.xds`**?

**`ramp.xds`** makes it easy to set up, solve, and apply dynamical systems models for malaria & other mosquito-borne pathogens. A large code library, in [**`ramp.library`**](https://dd-harp.github.io/ramp.library/), includes a large set of published models with code that has been verified and tested. If you're working on a model family that is not already in *ramp.library,* you can write us and we'll help you add it to the growing collection. 

It should be comparatively easy to replicate your study, or any study, using *ramp.xds.* It’s also easy for others to replicate your study.

Any model family from one domain can be combined with models from other domains and take advantage of the other features of *ramp.xds.*  Software capabilities include: 

+ support for systems of ordinary and delay, differential and difference equations; 

+ support for autonomous and non-autonomous, including exogenous forcing by weather and vector control; 

+ structural flexibility in stratifying host population to handle heterogeneity in epidemiological traits, such as vaccination, drug taking, net ownership, travel and mobility, and host demography;

+ support for modeling realistic human / host demography with cohort dynamics and aging to model infection, immunity, disease, and infectiousness in host populations over by age and time; 

+ support for modeling mass vaccination, mass drug administration, and other health interventions.  

+ environmental heterogeneity in exposure;  

+ malaria importation through travel and visitors; 

+ multiple host species (or types) and multiple mosquito vector species (or types);

+ support for modeling observational processes (in *ramp.xds*) and (through *ramp.work*) model fitting.  
    
+ support for analysis, including computation of reproductive numbers 


## Modularity 

This software supports nimble model building with functions to set up, solve, and analyze dynamical systems models for mosquito ecology and mosquito-borne pathogen transmission formulated as dynamical systems in continuous or discrete time. 

The mathematical framework and software have a modular design developed around an algorithm to model mosquito blood feeding on vertebrate hosts and parasite / pathogen transmission, including patch-based spatial dynamics within a defined spatial domain. The blood feeding algorithm provides a rigorous computational interface linking dynamical components describing parasite/pathogen linking infection dynamics in mosquito and vertebrate host populations. It guarantees mathematical consistency in computing blood feeding rates and habits and in allocating bites and blood meals with heterogeneous vertebrate host population densities, including models with dynamically changing availability of host populations or other vertebrate animals. 

The framework and software also support spatial models of mosquito ecology through an algorithm that describes egg laying by adult mosquitoes in aquatic habitats and emergence of adult mosquitoes. The egg laying algorithm provides a rigorous interface linking dynamical components describing adult mosquito populations in a set of spatial patches and immature aquatic mosquito populations in a structured set of aquatic habitats. 

The software aims to be fully modular: it implements plug-and-play computation for a broad class of dynamical systems models for describing the dynamics and control of malaria and other mosquito-transmitted pathogens. Modularity has been achieved through a combination of methods: the functions that compute derivatives (for differential equations) or that update state variables (for difference equations) use a generic method (using R's `S3` system) that dispatches on parameter lists for each component which is used to compute the full set of differential equations; the interfaces for blood feeding and egg laying are biologically realistic but rigidly defined to guarantee mathematical consistency; and a large set of additional features have been implemented with careful attention to detail to guarantee consistency and avoid conflicts. Each dynamical component also includes a null model, but where its outputs can be computed by any function. The function `ramp.xds::xde_derivatives` computes the derivatives of all state variables from those modular components and can be used with the excellent solvers in [deSolve](http://desolve.r-forge.r-project.org/). The functions `ramp.xds::dts_update` updates the state variables in discrete-time systems with support for mismatched run-time time steps. 

To get started, please consider reading some of the articles in the drop down panels above, at our [website](https://dd-harp.github.io/ramp.xds/). The 3 sections ending in "Component" describe particular models implementing
the interface for each of those components (adult mosquitoes, aquatic mosquitoes, and humans), and show a simulation at their equilibrium values. 

The section "Articles" has more in-depth examples, including an extended walk through of how to set up and run a model in `vignette("ex_534")`, a guide on how to contribute, and an example of running a model in ramp.xds with external forcing under a model of ITN (insecticide treated nets) based vector control in `vignette("vc_lemenach")`. 

The section "Functions" documents each function exported by the package.

## Contributing

**ramp.xds** aims to provide stable, reliable, reusable code for the study of mosquito-borne pathogen dynamics and control using dynamical systems.   

For information about how to contribute to the development of ramp.xds, please read our article on  [Contributing](https://dd-harp.github.io/ramp.xds/articles/Contributing.html). 

If you have any questions, comments, bug reports, or suggestions, the first point of contact with the development team is through [GitHub Issues](https://github.com/dd-harp/ramp.xds/issues). If you are specifically submitting a bug report, please check out our [bug reporting guide](https://dd-harp.github.io/ramp.xds/articles/Contributing.html#sec8). If you are interested in collaborating in extensive model development (e.g. new mosquito model), please do not hesitate to contact the authors, whose email addresses can be found in the `DESCRIPTION` file.

We welcome issues and encourage contribution regardless of experience; the length of the contributing guide is not intended to be intimidating, merely complete. It is the responsibility of the package maintainers to help new contributors understand our conventions and guide contributions to a successful conclusion.

## Acknowledgements

+ This project has been supported by a grant from the Bill and Melinda Gates Foundation, **Modeling for Adaptive Malaria Control** (INV 030600, PI = David L Smith, University of Washington).

+ Adaptive Vector Control is funded by grant **Spatial Targeting and Adaptive Vector Control for Residual Transmission and Malaria Elimination in Urban African Settings** (R01 AI163398, PI = David L Smith), from US National Institute of Allergies and Infectious Diseases (NIAID).

+ Development of the software was supported through a collaboration with the [**Bioko Island Malaria Elimination Program**](https://www.mcd.org/focus-areas/featured-projects/bioko-island-malaria-elimination-project) 

+ Development of the software was supported through a collaboration with Uganda's **National Malaria Control Division** and **Department of Health Information** in the [Uganda Ministry of Health](https://www.health.go.ug/)

+ Development of this software benefited from funding and collaboration with the NIAID grant **Program for Resistance, Immunology, Surveillance & Modeling of Malaria in Uganda** (PRISM) (2U19AI089674, PIs = Grant Dorsey, University of California San Francisco; and Moses Kamya, Infectious Diseases Research Collaboration), which was part of the **International Centers of Excellence in Malaria Research** (ICEMR) program.

+ Funding to develop models of West Nile Virus to support Harris County Public Health was funded by the NSF as part of a project, Computing the Biome (PI = Janos Sztipanovits). The project was part of the Convergence Accelerator program of the National Science Foundation, Directorate for Technology, Innovation, and Partnerships (TIP) ([NSF 2040688](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2040688) and [NSF 2040688](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2134862), PI=Janos Sztipanovits, Vanderbilt University).

+ We acknowledge the important formative role played by the mosquito working groups of **RAPIDD** (Research and Policy for Infectious Disease Dynamics), which was sponsored by the Fogarty International Center, NIH, and the Department of Homeland Security. The mosquito working groups were led by Professor Thomas Scott. RAPIDD was led by F. Ellis McKenzie. 




