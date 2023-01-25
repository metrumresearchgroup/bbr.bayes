
.onLoad <- function(libname, pkgname) {
  register_model_type("nmbayes",
                      c(NMBAYES_MOD_CLASS, NM_MOD_CLASS),
                      "model")
  register_model_type("nmbayes",
                      c(NMBAYES_SUM_CLASS, NM_SUM_CLASS),
                      "summary")

  register_model_type("stan_gq",
                      c(STAN_GQ_MOD_CLASS, STAN_MOD_CLASS),
                      "model")
  register_model_type("stan_gq",
                      c(STAN_GQ_SUM_CLASS, STAN_SUM_CLASS),
                      "summary")
}
