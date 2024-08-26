# **`ramp.xds`** - e**X**tensible **D**ynamical **S**ystems 
## *Nimble model building for the epidemiology, spatial transmission dynamics, and control of malaria and other mosquito-borne pathogens*

<!-- badges: start -->
[![PLoS Computational Biology](https://img.shields.io/badge/doi%3A10.1371%2Fjournal.pcbi.1010684-cornflowerblue?style=for-the-badge&logoColor=cornflowerblue&label=PLoS%20Comp%20Bio&labelColor=slategrey&link=https%3A%2F%2Fjournals.plos.org%2Fploscompbiol%2Farticle%3Fid%3D10.1371%2Fjournal.pcbi.1010684)](https://doi.org/10.1371/journal.pcbi.1010684)

<!-- badges: end -->

## Install **`ramp.xds`** 

To install the latest version from GitHub, run the following lines of code in an R session.

```
library(devtools)
devtools::install_github("dd-harp/ramp.xds")
```

Also see the related vignette [*Getting Started*](https://dd-harp.github.io/ramp.xds/articles/GettingStarted.html).

## What is RAMP?

RAMP -- **R**obust **A**nalytics for **M**alaria **P**olicy -- is a bespoke inferential system for malaria decision support and adaptive malaria control. A core goal for RAMP is to characterize, quantify, and propagate uncertainty in conventional analysis and through simulation-based analytics.

## What is **`ramp.xds`**?

**`ramp.xds`** is an R software package that supports nimble model building for simulation-based analytics and malaria research. It was designed to set up, analyze, and solve dynamical systems describing the epidemiology, spatial transmission dynamics, and control of malaria and other mosquito-transmitted pathogens. The software also supports nimble model building and analysis for mosquito ecology, with the capability to handle forcing by weather and other exogenous variables. 

**`ramp.xds`** has been designed to serve the needs of malaria programs. In that context, mathematical models are developed for decision support and adaptive malaria control in a defined geographical area. Mechanistic models describing malaria transmission in a place, fitted to available data while acknowledging uncertainty, are a synthesis of malaria intelligence in a form that can facilitate complex analysis, extending our innate mental capabilities. As the needs of a malaria program changes, the models can be modified -- simplified or extended -- to serve the tasks at hand. 

**`ramp.xds`** was also developed to apply malaria theory: it outputs standard, observable malaria metrics, and it easily solves equations, computes steady states and stable orbits, and computes the spatio-temporal vectorial capacity and the basic and adjusted reproductive numbers for malaria parasites. 

The software was designed around a rigorous mathematical framework for modular model building, described in [Spatial Dynamics of Malaria Transmission](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1010684) (Wu SL, *et al.* 2023. PLoS Computational Biology)^[Wu SL, Henry JM, Citron DT, Mbabazi Ssebuliba D, Nakakawa Nsumba J, Sánchez C. HM, et al. (2023) Spatial dynamics of malaria transmission. PLoS Comput Biol 19(6): e1010684. https://doi.org/10.1371/journal.pcbi.1010684]. The mathematical framework has now been extended to cover *exogenous forcing* by weather and vector control. 

**`ramp.xds`** is part of a suite of R packages developed to support RAMP: 

+ **`ramp.xds`** is the core computational engine for simulation-based analytics. It includes a basic set of models -- enough to design, verify, and demonstrate the basic features of modular software. 

+  [**`ramp.library`**](https://dd-harp.github.io/ramp.library/) is an extended library of stable code that has been tested and verified. It includes a large set of model families published in peer review that are not included in **`ramp.xds`** The ability to reuse code reduces the costs of replicating studies. Through this library, **`ramp.xds`** also supports nimble model building and analytics for other mosquito-borne pathogens. 

+ [**`ramp.work`**](https://dd-harp.github.io/ramp.work/) includes algorithms to apply the framework, include code to fit models to data and to do constrained optimization 

+ [**`ramp.malaria`**](https://dd-harp.github.io/ramp.malaria/) includes a large set of models illustrating capabilities of **`ramp.xds`** 

**`ramp.xds`**  is under active development. It supersedes two other software packages, no longer under active development: [**`exDE`**](https://dd-harp.github.io/exDE/) and [**`MicroMoB`**](https://dd-harp.github.io/MicroMoB/). The history of development of RAMP simulation software has been memorialized in a [vignette](https://dd-harp.github.io/ramp.xds/articles/History.html).

## **`ramp.xds`** Capabilities

**`ramp.xds`** includes functions that make it easy to set up, solve, and apply dynamical systems models for malaria & other mosquito-borne pathogens. A code library, in [**`ramp.library`**](https://dd-harp.github.io/ramp.library/), aims to become a comprehensive set of published models with code that has been verified and tested. It should be comparatively easy to replicate your study, or any study, using **`ramp.xds.`** If you're working on a new model family that is not already in **`ramp.library,`** you can write us and we'll help you add it to the growing collection. After your model has been added to the **`ramp.library`**, it should be easy for others to replicate your study.

**`ramp.xds`** makes it easy to extend the capability of your models. Any model family from one dynamical component can be combined with any model from another dynamical component, and the rigid interface developed around blood feeding and egg laying make it possible to add structure and spatial dynamics. 

Some built in features that make **`ramp.xds`** easy to use (see [*Getting Started*](https://dd-harp.github.io/ramp.xds/articles/GettingStarted.html)): 

+ after solving, the outputs are parsed to make it easy to examine, plot, and visualize;

+ the software automatically computes dynamical terms.  

Additional capabilities of **`ramp.xds`** include support for: 

+ systems of ordinary and delay, differential and difference equations; 

+ autonomous and non-autonomous systems, including exogenous forcing by weather; 

+ mosquito spatial ecology and spatial transmission dynamics with algorithms to model mosquito dispersal and human mobility and travel;

+ structural flexibility for host populations, making it easy to stratify by epidemiological traits, such as heterogeneous exposure, vaccination, drug taking, net ownership, travel and mobility, and host demography;

+ realistic human / host demography with aging and cohort dynamics to model infection, immunity, disease, and infectiousness in host populations over by age and time; 

+ vector control;  

+ mass vaccination, mass drug administration, and other health interventions;  

+ heterogeneous biting and environmental heterogeneity in exposure;  

+ malaria connectivity, including importation through travel and visitors; 

+ multiple host species (or types) and multiple mosquito vector species (or types);

+ observational processes;

+ model fitting (see [**`ramp.work`**](dd-harp.github.io/ramp.work/)).
    
+ support for analysis, including computation of reproductive numbers 

## Modularity 

**`ramp.xds`** supports nimble model building through functions that make it easy to set up, solve, analyze, and apply dynamical systems models for mosquito ecology and mosquito-borne pathogen transmission formulated as dynamical systems in continuous or discrete time. To accomplish this, 
**`ramp.xds`** was designed to be modular, flexible, and extensible (see [Modularity](https://dd-harp.github.io/ramp.xds/articles/Modularity.html)). Since the software has been designed around a the mathematical framework in [Wu SL, *et al.* 2023](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1010684), the software documentation and vignettes tend to follow the same notation. 

The modularity is a key design feature of **`ramp.xds`.** There are five core dynamical components in three chunks connected by two rigid interfaces:

***
<center>
![**Caligraphic Roman Letters Describe Major Components**](vignettes/notation1.png)
</center> 
***

Mosquito ecology and parasite infection dynamics are inextricably linked, as are the models describing human demography and parasite infection dynamics and immunity in human populations. 

The modularity has been achieved through a combination of methods: the functions that compute derivatives (for differential equations) or that update state variables (for difference equations) use a generic method (using R's `S3` system) that dispatches on parameter lists for each component which is used to compute the full set of differential equations; the interfaces for blood feeding and egg laying are biologically realistic but rigidly defined to guarantee mathematical consistency; and a large set of additional features have been implemented with careful attention to detail to guarantee consistency and avoid conflicts. Each dynamical component also includes a null model, but where its outputs can be computed by any function. The function `ramp.xds::xde_derivatives` computes the derivatives of all state variables from those modular components and can be used with the excellent solvers in [deSolve](http://desolve.r-forge.r-project.org/). The functions `ramp.xds::dts_update` updates the state variables in discrete-time systems with support for mismatched run-time time steps. Extensibility has been implemented through a system for adding exogenous forcing by weather and mosquito resource dynamics, vector control and health systems. 
  
A good place to start is the vignette [Getting Started](https://dd-harp.github.io/ramp.xds/articles/GettingStarted.html). A vignette that walks through `xds_setup` and sets up a simple system with 5 habitats, 3 patches, and 4 population strata (see the [5-3-4 Model Vignette](https://dd-harp.github.io/ramp.xds/articles/ex_534.html)). Please browse the "Articles" or explore the well-documented "Functions."

***

<center>
![Modularity, as implemented in **`ramp.xds`**](vignettes/ModularGeneralized.png)
</center> 

***

## Contributing

**ramp.xds** aims to provide stable, reliable, reusable code for the study of mosquito-borne pathogen dynamics and control using dynamical systems. For information about how to contribute to the development of ramp.xds, please read our article on [Contributing](https://dd-harp.github.io/ramp.xds/articles/Contributing.html). 

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




