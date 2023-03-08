# The purpose of this script is to for manually verifying that the
# output of run_sims() looks correct (from a scientific point of view)
#
# The script will call run_sims() on our test model, and then create
# plots and tables that let the scientific reviewer manually compare
# these simulated outputs against the same plots and tables created
# from raw NONMEM outputs. We expect these outputs to look _reasonably_
# similar, but not similar enough that we can compare them programmatically,
# hence the reliance on manual human inspection.

# load bbr.bayes
devtools::load_all()
library(bbr)
library(mrgsolve)
#anything else we need to load?

################################
# generate run_sims() output
################################

NMBAYES_MOD_ID <- "1100"
NMBAYES_ABS_MODEL_DIR <- system.file("model", "nonmem", "bayes",
                                     package = "bbr.bayes", mustWork = TRUE)

mod_nm <- read_model(file.path(NMBAYES_ABS_MODEL_DIR, NMBAYES_MOD_ID))

mod_ms <- mrgsolve::mread(
  system.file("model", "mrgsolve", "1100.mod",
              package = "bbr.bayes", mustWork = TRUE))

withr::with_seed(3012, {
  res <- run_sims(mod_nm, mod_ms, n_post = 500)
})


################################
# get outputs from single chain,
#   for comparison
################################

# NOTE: will this even work?
# I'm trying to treat this single chain as a regular NM bbr model...
mod_nm_comp <- read_model(file.path(
  NMBAYES_ABS_MODEL_DIR,
  paste0(NMBAYES_MOD_ID, "_1")
))

res_comp <- nm_join(mod_nm_comp)

#### maybe we need this code?#### ####
# filter to observation rows only
res_comp <- res_comp %>%
  filter(EVID == 0)

### is our test model log_dv?
# res_comp <- mutate(res_comp,
#                    LNPRED = PRED,
#                    PRED = exp(PRED),
# )

# filter to single row per ID and include factors
id <- distinct(data, ID, .keep_all = TRUE)
#### #### #### #### #### ####



####### then do things like this...

### from sim output
  dvp <- dv_pred_ci(res)

  # DV vs population prediction by renal function
  dvp_rf <- dv_pred_ci(res, scales = "free") +
    facet_wrap(~RF, scales = "free")

  # DV vs population prediction by hepatic function
  dvp_cp <- dv_pred_ci(res, scales = "free") +
    facet_wrap(~CP, scales = "free")

### for comparison
dvp_comp <- dv_pred(res_comp, x = "EPRED//Population predicted {xname}")
dvp_rf_comp <- dv_pred(res_comp, x = "EPRED//Population predicted {xname}",
                  scales = "free") +
  facet_wrap(~RF, scales = "free")
dvp_cp_comp <- dv_pred(res_comp, x = "EPRED//Population predicted {xname}",
                  scales = "free") +
  facet_wrap(~CP, scales = "free")

##### tell reviewer to look at dvp vs. dvp_comp, etc. etc.
