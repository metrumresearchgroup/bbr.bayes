#' @title Stan models in `bbr`
#'
#' @description
#'
#' This page provides an overview of the basic structure of Stan models in
#' `bbr`. The main entry point for interacting with Stan models is the
#' `bbi_stan_model` object. With it, you can create a new model on disk from
#' "scaffold" files, copy a new model from an existing one, jump to model files
#' of interest, and submit models. The Details section contains information
#' about the model structure and the necessary files that will exist on disk for
#' any `bbi_stan_model`.
#'
#' @details
#'
#' ## Model Structure
#'
#' **`<run>`** - The "run" is, in some sense, the "name" of a given model.
#' Practically, it will correspond to the model directory name, the base name of
#' the bbr-created YAML (`<run>.yaml`), as well as the base name for some files
#' in that directory. Calling [bbr::get_model_id()] on a model object will
#' return `<run>` as a string. The `bbi_log_df` tibbles also all contain a run
#' column which is populated by calling `basename(.mod$absolute_model_path)` for
#' each model. Note: this is _not_ actually stored in the model object because
#' it can be unequivocally extracted as just described.
#'
#' **`absolute_model_path`** - Like the `bbi_nonmem_model` object, the
#' `bbi_stan_model` object will carry around only an absolute path to the model
#' directory. This will point to the model directory (named `<run>`) containing
#' all of the files described below, as well as a `<run>.yaml` file that `bbr`
#' uses to persist model metadata. A model is loaded or created by passing a
#' relative path to this directory to either [bbr::read_model()] or
#' [bbr::new_model()], both of which return the `bbi_stan_model` object. When
#' this object is created, it checks the model directory for the relevant files
#' and populates `absolute_model_path`.
#'
#' ## Necessary Files
#'
#' All of the files described below will exist inside the model directory named
#' `<run>`. If you call `new_model(..., .model_type = "stan")` without any of
#' these files, template "scaffold" files for all of them will be created in the
#' newly created model directory.
#'
#' **`<run>.stan`** - The Stan file.
#'
#' **`<run>-stanargs.R`** - Contains a named list with all of the arguments that
#' will be passed through to the [$sample()][cmdstanr::model-method-sample]
#' method of [cmdstanr::CmdStanModel]. See [set_stanargs()] for details on
#' modifying.
#'
#' **`<run>-standata.R`** - Contains all necessary R code to read in
#' any source data and transform them to a Stan-ready data object (list).
#'
#'   * Contains only one function, called `make_standata(.dir)`, that takes a
#'     single argument and returns the data list to pass to the
#'     [CmdStanModel$sample()][cmdstanr::model-method-sample].
#'
#'   * The `.dir` argument will be the directory containing the script. This is
#'     used to find data files for loading, for example
#'     `read_csv(file.path(.dir, "..", "..", "data", "derived", "my_data.csv"))`
#'
#'   * Can be called (by [build_data()]) to generate the data for model
#'     submission or to compare the resulting data to previously saved data on
#'     disk.
#'
#'   * Note that `make_standata` will be evaluated in the parent environment of
#'     your global session, giving it access to all other environments on your
#'     search path. This means that you don't _need_ to prefix function calls
#'     with the package name (e.g., `here::here()`), but doing so is recommended
#'     so that `make_standata` doesn't depend on your search path state. As an
#'     exception, you may be comfortable leaving base packages unqualified
#'     (e.g., `rnorm()` rather than `stats::rnorm()`) because users are unlikely
#'     to remove `package:stats` from their search path or to attach a package
#'     that overrides `rnorm()`.
#'
#' **`<run>-init.R`** - This file contains all necessary R code to create the
#' initial values passed to the cmdstanr's
#' [$sample()][cmdstanr::model-method-sample] method. This file is a lot like
#' `<run>-standata.R` (discussed above) and a scaffold can be created with
#' [add_staninit_file()].
#'
#'   * Contains only one function, called `make_init(.data)`, that takes a
#'     single argument and returns something that can be passed to the `init`
#'     argument of `$sample()`. There are several options; see the
#'     [$sample()][cmdstanr::model-method-sample] documentation for details.
#'
#'   * The object returned from `make_standata()` will be passed to the `.data`
#'     argument of `make_init()`.
#'
#'   * Will be called internally by `bbr` and the result passed as the `init`
#'     argument to `$sample()`.
#'
#'   * See the `make_standata` entry above for details on the evaluation
#'     environment.
#'
#'   * Note that `$sample()` supports passing
#'     _"A function that returns a single list..."_. If you intend to use this
#'     option, your `make_init()` function must return _the function_ described,
#'     _not_ the "single list...".
#'
#'   * Note that this file will not be included when you're defining a model for
#'     _standalone_ generated quantities. See "Standalone Generated Quantities"
#'     section below for more information.
#'
#' ## Other Files and Directories
#'
#' There will be several other things created in the model directory, as the
#' model is run or as it prepares to run.
#'
#' **`<run>`** - This is the binary file created when the `<run>.stan` file is
#' compiled by `cmdstan`. We `.gitignore` this automatically.
#'
#' **`<run>-output`** - This directory is created by `bbr`. It is where the
#' posteriors will be saved (currently as CSV’s) and also where the
#' `bbi_config.json` is saved when the model run finishes successfully. Note
#' that we _don’t_ call this `<run>` (as is done in NONMEM) for two primary
#' reasons:
#'
#'   * It is more informative to call it `<run>-output` to distinguish it from
#'     all the other files and directories that start with `<run>`.
#'
#'   * There is also the binary called `<run>` (previously mentioned) that could
#'     cause confusion. In fact, there was a bug in `cmdstanr` in February 2021
#'     involving exactly this scenario.
#'
#' `<run>-output/bbi_config.json` - This file is created by `bbr` when a model
#' run finishes successfully. It stores some configuration information about the
#' run, as well as the md5 hashes of the necessary files. These hashes are later
#' used (by [bbr::check_up_to_date()] to check whether the files have changed
#' since the model was run, primarily for reproducibility purposes.
#'
#' ## Some Helper Functions
#'
#' * **[check_stan_model()]** (mentioned above) - Checks for the necessary files
#'   before running or copying the model. By default, it also checks the syntax
#'   of the `<run>.stan` file.
#'
#' * **[bbr::build_path_from_model()]** - Builds the absolute path to a file in
#'   the model folder from a model object and a suffix.
#'
#' * **[add_stanmod_file()], [add_standata_file()], [add_staninit_file()],
#'   [add_stan_fitted_params_file()]** - Helpers for adding one of the necessary
#'   files to the model folder.
#'
#' * **[open_stanmod_file()], [open_standata_file()], [open_staninit_file()],
#'   [open_stan_fitted_params_file()]** - Helpers for opening files within the
#'   model directory.
#'
#' * **[bbr::model_diff()]** - Compare necessary files between two models.
#'   Defaults to comparing `<run>.stan` files.
#'
#' * Also has many of the same helpers as `bbi_nonmem_model` objects:
#'   [bbr::tags_diff()], [bbr::add_tags()], [bbr::add_notes()],
#'   [bbr::get_model_path()], [bbr::get_output_dir()], [bbr::get_model_id()]
#'
#' ## Standalone Generated Quantities
#'
#' Stan supports generating quantities of interest from existing posterior
#' samples (see [Stan user's
#' guide](https://mc-stan.org/docs/stan-users-guide/stand-alone-generated-quantities-and-ongoing-prediction.html)).
#' \pkg{cmdstanr} exposes this through the
#' [$generate_quantities()][cmdstanr::model-method-generate-quantities] of
#' [cmdstanr::CmdStanModel].
#'
#' *Note:* The information below applies to _standalone_ generated quantities.
#' If the model defines generated quantities that are produced at the same time
#' as the MCMC samples, the model will have the structure defined above.
#'
#' In `bbr`, models for standalone generated quantities are defined via the
#' `bbi_stan_gq_model` object, a subclass of `bbi_stan_model`. On the file
#' system, these models look very similar to regular Stan models, with the
#' following differences:
#'
#'  * the "model_type" value in the model YAML is "stan_gq" instead of "stan"
#'
#'  * there is no `<run>-init.R` file;
#'    [$generate_quantities()][cmdstanr::model-method-generate-quantities] does
#'    not have an `init` argument.
#'
#'  * there is a `<run>-fitted-params.R` file. This file must define a function,
#'    `make_fitted_params`, that takes a single argument, the model object. The
#'    function can return any value accepted for the `fitted_params` argument of
#'    [$generate_quantities()][cmdstanr::model-method-generate-quantities].
#'
#'    See the `make_standata` entry above for details on the evaluation
#'    environment.
#'
#' "stan_gq" models can be created fresh with `new_model(..., .model_type =
#' "stan_gq")`. However, for the more common case where the "stan_gq" model is
#' derived from an existing "stan" model, you can use the
#' [copy_model_as_stan_gq()] helper, which takes care of copying over the
#' relevant files, adding a "gq_parent" field to the model's YAML file that
#' points back to the parent model, and setting up a default
#' `<run>-fitted-params.R` that returns the paths to the parent model's
#' posteriors.
#'
#' The "gq_parent" field of "stan_gq" models links to the "stan" model whose
#' samples are used as input. The default `<run>-fitted-params.R` uses this
#' value to retrieve the previous fit, and
#' [check_up_to_date][check_up_to_date_stan_model] considers it when deciding
#' whether a model is up to date.
#'
#' In the most common case, the "gq_parent" value will be automatically set up
#' by [copy_model_as_stan_gq()]. However, you may want to manually set this to
#' multiple values (e.g., with [add_stan_gq_parent()]) for cases where the
#' fitted parameters are coming from multiple models. The field may also be
#' absent, which is appropriate for cases where the fitted parameters are not
#' coming from a previous model.
#'
#' To run a "stan_gq" model, pass the `bbi_stan_gq_model` object to
#' [submit_model()][stan_submit_model], which will use the model files to
#' construct a call to
#' [CmdStanModel$generate_quantities()][cmdstanr::model-method-generate-quantities].
#'
#' @name bbr_stan
#' @aliases bbi_stan_model bbi_stan_gq_model
NULL
