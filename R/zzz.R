
.onLoad <- function(libname, pkgname) {
  register_model_type("nmbayes",
                      c(NMBAYES_MOD_CLASS, NM_MOD_CLASS),
                      "model")
  register_model_type("nmbayes",
                      c(NMBAYES_SUM_CLASS, NM_SUM_CLASS),
                      "summary")
}
