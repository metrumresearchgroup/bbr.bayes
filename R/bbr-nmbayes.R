#' @title NONMEM Bayes models in `bbr`
#'
#' @description
#'
#' This page provides an overview of the basic structure of NONMEM Bayes models
#' in `bbr`. The main entry point for interacting with NONMEM Bayes models is
#' the `bbi_nmbayes_model` object.
#'
#' @details
#'
#' ## Model Structure
#'
#' NONMEM models in `bbr` have one control stream. Executing the model produces
#' an output directory. NONMEM Bayes models, on the other hand, have a more
#' complicated model structure. The user still defines only one control stream.
#' Underneath, this top-level control stream is then used to derive the control
#' stream of multiple submodels, one for the initial estimates and one for each
#' chain.
#'
#' For a control stream with the base name `run`,
#' [submit_model()][nmbayes_submit_model] will create the following submodels:
#'
#' **`<run>/init/`** - A model that generates the initial estimates.
#'
#' **`<run>/<run>-<chain>/`** - A model for each chain that generates samples
#' using `BAYES` or `NUTS` estimation methods.
#'
#' ## Defining a NONMEM Bayes model
#'
#' The first step to defining a NONMEM Bayes model will often be to call
#' [copy_model_as_nmbayes()] on an existing non-Bayes NONMEM model. This creates
#' the new model with [bbr::copy_model_from()] and then sets the model type to
#' by "nmbayes". Alternatively you can also create a fresh NONMEM Bayes model
#' with `new_model(..., .model_type = "nmbayes")`. (As with regular NONMEM
#' models, a control stream must already exist.)
#'
#' The control stream must define the following two records:
#'
#'  * a `METHOD=CHAIN` estimation record that defines how to generate the
#'    initial estimates for each chain. `ISAMPLE` should be set to 0. The
#'    `NITER` option controls the number of chains.
#'
#'  * a `METHOD=BAYES` or `METHOD=NUTS` estimation record that defines how to
#'    generate the initial estimates for each chain.
#'
#' Here's an example that defines four chains of 500 samples:
#'
#'     $EST METHOD=CHAIN FILE=1100.chn NSAMPLE=4 ISAMPLE=0 SEED=1
#'          CTYPE=0 IACCEPT=0.3 DF=10 DFS=0
#'
#'     $EST METHOD=NUTS AUTO=2 CTYPE=0 OLKJDF=2 OVARF=1 SEED=1 NBURN=250 NITER=500
#'          NUTS_DELTA=0.95 PRINT=10 MSFO=./1100.msf RANMETHOD=P PARAFPRINT=10000
#'          BAYES_PHI_STORE=1
#'
#' @name bbr_nmbayes
#' @aliases bbi_nmbayes_model
NULL
