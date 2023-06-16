
#' @importFrom glue glue
#' @importFrom rlang := %||% .data
#' @importFrom tidyselect all_of everything starts_with
NULL

### NONMEM Bayes

NMBAYES_MOD_CLASS <- "bbi_nmbayes_model"
NMBAYES_SUM_CLASS <- "bbi_nmbayes_summary"

IPH_ID_NAMES <- c("SUBJECT_NO", "ID", "SUBPOP")
IPH_NONPARAM_NAMES <- c("ITERATION", IPH_ID_NAMES)

NMBAYES_HELP <- "
; TODO: This model was copied by bbr.bayes::copy_model_as_nmbayes().
;       nmbayes models require a METHOD=CHAIN estimation record and a
;       METHOD=BAYES or METHOD=NUTS estimation record. The records
;       below are meant as a starting point.  At the very least, you
;       need to adjust the number of iterations (see NITER option in
;       the second $EST block), but please review all options
;       carefully.
;
;       See ?bbr.bayes::bbr_nmbayes and the NONMEM docs for details.
$EST METHOD=CHAIN FILE={model_id}.chn NSAMPLE=4 ISAMPLE=0 SEED=1
     CTYPE=0 IACCEPT=0.3 DF=10 DFS=0

$EST METHOD=NUTS SEED=1 NBURN=250 NITER=NNNN
     AUTO=2 CTYPE=0 OLKJDF=2 OVARF=1
     NUTS_DELTA=0.95 PRINT=10 MSFO={model_id}.msf RANMETHOD=P PARAFPRINT=10000
     BAYES_PHI_STORE=1

"

### Stan

STAN_MOD_CLASS <- "bbi_stan_model"
STAN_SUM_CLASS <- "bbi_stan_summary"

STAN_GQ_MOD_CLASS <- "bbi_stan_gq_model"
STAN_GQ_SUM_CLASS <- "bbi_stan_gq_summary"

STAN_FIT_CLASS <- "CmdStanMCMC"
STAN_GQ_FIT_CLASS <- "CmdStanGQ"

STANMOD_SUFFIX <- ".stan"
STANDATA_R_SUFFIX <- "-standata.R"
STANDATA_JSON_SUFFIX <- "-standata.json"
STANINIT_SUFFIX <- "-init.R"
STANARGS_SUFFIX <- "-stanargs.R"
STAN_FITTED_PARAMS_SUFFIX <- "-fitted-params.R"
STAN_OUTDIR_SUFFIX <- "-output"
STAN_MODEL_FIT_RDS <- file.path(STAN_OUTDIR_SUFFIX, "fit.RDS")

STAN_MODEL_REQ_FILES <- c(
  STANMOD_SUFFIX,
  STANDATA_R_SUFFIX,
  STANINIT_SUFFIX,
  STANARGS_SUFFIX
)

STAN_GQ_MODEL_REQ_FILES <- c(
  STANMOD_SUFFIX,
  STANDATA_R_SUFFIX,
  STANARGS_SUFFIX,
  STAN_FITTED_PARAMS_SUFFIX
)

STAN_RESERVED_ARGS <- c(
  "data",
  "output_dir"
)

STANCFG_DATA_MD5 <- "standata_script_md5"
STANCFG_INIT_MD5 <- "init_script_md5"
STANCFG_ARGS_MD5 <- "stanargs_md5"
STANCFG_FITTED_PARAMS_MD5 <- "stan_fitted_params_md5"
STANCFG_GQ_PARENT_MD5 <- "gq_parent_md5"

STAN_BBI_VERSION_STRING <- "STAN"

YAML_GQ_PARENT <- "gq_parent"

############
# SCAFFOLDS
############

STANMOD_SCAFFOLD <- "mod.stan"
STANMOD_SCAFFOLD_MD5 <- "cb1c31e0f34cd0c196b64b6cd5492669"

STANDATA_SCAFFOLD <- "data.R"
STANDATA_SCAFFOLD_MD5 <- "44721f8445919647cc59ecc3ecc44072"

STANINIT_SCAFFOLD <- "init.R"

STANARGS_SCAFFOLD_MD5 <- "8b11f83c3293f3b89a30807b3b69b58e"

STAN_FITTED_PARAMS_SCAFFOLD <- "fitted-params.R"

STAN_SCAFFOLD_MD5_VEC <- c(
  STANMOD_SCAFFOLD_MD5,
  STANDATA_SCAFFOLD_MD5,
  STANARGS_SCAFFOLD_MD5
  # NOTE: we don't check -init.R or -fitted-params.R because they are _not_
  # scaffolds; they return working default values.
)

MISSING_STAN_FILES_ERR_MSG <- "The following files, which are necessary to run a `%s`, are missing"
STAN_SCAFFOLD_ERR_MSG <- "The following files, which are necessary to run a `%s`, are only scaffolds:"

utils::globalVariables("make_standata")
