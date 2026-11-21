# X

#' @title Basic Setup Functions
#'
#' @description
#'
#' **Basic Setup** is defined by the use of [xds_setup] or one of its variants.
#' It returns an **xds** model object that can be solved by [xds_solve] or
#' modified through advanced setup. The functions do three things:
#'
#' + **Dynamics** --- set up the dynamical components
#'
#' + **Model Structure** --- set up the basic model structure
#'
#' + **Basic Spatial Dynamics** --- set up the basic spatial parameters
#'
#' @section Basic Setup Functions:
#'
#' Depending on the purpose of the study,
#' **Basic Setup** is done by one of five functions:
#'
#' + [xds_setup] can be used for most studies. It configures all three dynamical components.
#'
#' + [xds_setup_mosy] sets a model for mosquito ecology that ignores infection dynamics.
#'
#' + [xds_setup_aquatic] sets up a model for aquatic dynamics forced by egg laying.
#'
#' + [xds_setup_human] sets up an **XH** module to study disease dynamics forced by infective biting in the patches.
#'
#' + [xds_setup_eir] sets up an **XH** module to study disease dynamics forced by the daily EIR.
#'
#'
#' Each function sets `class(xds_obj$frame)` to dispatch functions that are used to solve dynamical systems.
#'
#' @section [xds_setup]:
#'
#' [xds_setup] sets `class(xds_obj$frame)="full"` and requires arguments to set up all three dynamical components.
#'
#' @section [xds_setup_mosy]:
#'
#' [xds_setup_mosy] sets `class(xds_obj$frame)="mosy"` for models of mosquito ecology:
#' - sets up an **L** module
#' - sets up a **MY** module, but the **Y** processes are ignored
#' - sets up a trivial **XH** module to pass human population density
#'
#' @section [xds_setup_aquatic]:
#'
#' [xds_setup_aquatic] sets `class(xds_obj$frame)="aquatic"` for models of aquatic mosquito ecology:
#' - sets up an **L** module
#' - sets up a trivial **MY** module and configures a forcing function for egg laying
#' - sets up an **XH** component to be ignored
#'
#' @section [xds_setup_human]:
#'
#' [xds_setup_human] sets `class(xds_obj$frame)="human"` for models of malaria epidemiology or other disease dynamics forced by infective biting in the patches:
#' - sets up the **XH** module
#' - sets up a trivial **MY** component and configures a forcing function for infective biting in patches
#' - sets up an **L** component to be ignored
#'
#' @section [xds_setup_eir]:
#'
#' [xds_setup_eir] sets `class(xds_obj$frame)="eir"` for models of malaria epidemiology or other disease dynamics forced by the EIR:
#' - sets up the **XH** module
#' - sets up a composed time series function for the EIR
#' - (optional) configures relative biting rates by age for cohort dynamics; defaults to `F_one`
#'
#' These models do not have active **MY** or **L** dynamical components.

#' Use:
#' + [xds_solve] when the independent variable is **time**
#' + [xds_solve_cohort] for cohort dynamics, when the independent variable is **age**
#'
#'
#' @seealso [xds_info_dynamical_components], [xds_info_trivial_forcing] and [xds_object]
#'
#' @name xds_basic_setup
#' @aliases xds_info_basic_setup
NULL
