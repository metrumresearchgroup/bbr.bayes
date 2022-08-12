
# Generics

#' @importFrom bbr check_up_to_date
#' @importFrom bbr copy_model_from
#' @importFrom bbr get_model_path
#' @importFrom bbr get_output_dir
#' @importFrom bbr model_diff
#' @importFrom bbr model_summary
#' @importFrom bbr submit_model
NULL

# Functions exported by bbr

#' @importFrom bbr build_path_from_model
#' @importFrom bbr config_log
#' @importFrom bbr get_data_path
#' @importFrom bbr get_model_id
#' @importFrom bbr get_yaml_path
#' @importFrom bbr new_model
#' @importFrom bbr read_model
#' @importFrom bbr run_log
#' @importFrom bbr summary_log
NULL

# Private things from bbr

build_new_model_path <- import_bbr("build_new_model_path")
check_yaml_in_sync <- import_bbr("check_yaml_in_sync")
copy_model_from_impl <- import_bbr("copy_model_from_impl")
file_matches_string <- import_bbr("file_matches_string")
model_diff_get_comp <- import_bbr("model_diff_get_comp")
model_diff_impl <- import_bbr("model_diff_impl")

ABS_MOD_PATH <- import_bbr("ABS_MOD_PATH", mode = "character")
BBI_PARENT_CLASS <- import_bbr("BBI_PARENT_CLASS", mode = "character")
CONFIG_DATA_MD5 <- import_bbr("CONFIG_DATA_MD5", mode = "character")
CONFIG_DATA_PATH <- import_bbr("CONFIG_DATA_PATH", mode = "character")
CONFIG_MODEL_MD5 <- import_bbr("CONFIG_MODEL_MD5", mode = "character")
RUN_ID_COL <- import_bbr("RUN_ID_COL", mode = "character")
SL_SUMMARY <- import_bbr("SL_SUMMARY", mode = "character")
YAML_BASED_ON <- import_bbr("YAML_BASED_ON", mode = "character")
YAML_DESCRIPTION <- import_bbr("YAML_DESCRIPTION", mode = "character")
YAML_MOD_TYPE <- import_bbr("YAML_MOD_TYPE", mode = "character")
YAML_TAGS <- import_bbr("YAML_TAGS", mode = "character")