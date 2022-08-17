
#' @export
config_log_make_entry.bbi_stan_model <- function(.mod, config, fields) {
  fields <- fields %||% c("model_md5", CONFIG_DATA_PATH, "data_md5")
  if (!all(fields %in% names(config))) {
    warning(get_config_path(.mod, .check_exists = FALSE),
            "is missing required keys: ",
            paste(fields[!(fields %in% names(config))], collapse = ', '))

    return(NULL)
  }
  # TODO: This is a kludge to meet bbr expectations. Consider reworking things
  # on bbr's side so that it isn't necessary.
  config[["bbi_version"]] <- STAN_BBI_VERSION_STRING

  return(list(config = config, fields = c(fields, "bbi_version")))
}
