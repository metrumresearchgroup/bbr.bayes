
#' Convert a non-Bayes NONMEM model into a nmbayes model
#'
#' NONMEM Bayes models have a different type than other Bayes models because
#' they have a more complicated structure underneath that requires special
#' handling. This method converts the type of an existing model from "nonmem" to
#' "nmbayes" by modifying the underlying YAML file. A call to
#' [bbr::copy_model_from()] should usually precede a call to this function.
#'
#' @param .mod A `bbi_nonmem_model` object.
#' @return The converted model. If `.mod` is already a `bbi_nmbayes_model`, it
#'   is returned as is.
#' @export
nmbayes_convert <- function(.mod) {
  nmbayes_warn()
  checkmate::assert_class(.mod, NM_MOD_CLASS)
  if (inherits(.mod, NMBAYES_MOD_CLASS)) {
    return(.mod)
  }

  .mod[[YAML_MOD_TYPE]] <- "nmbayes"
  save_model_yaml(.mod)

  mid <- get_model_id(.mod)
  glue_id <- function(x) {
    glue::glue_data_safe(list(model_id = mid), x)
  }

  ctl_path <- get_model_path(.mod)
  ctl_lines <- c(ctl_delete_section(readr::read_lines(ctl_path),
                                    c("EST", "TABLE")),
                 purrr::map_chr(nmbayes_est_stub, glue_id))
  readr::write_lines(ctl_lines, ctl_path)

  return(read_model(.mod[[ABS_MOD_PATH]]))
}

#' Delete specified model sections
#'
#' @param lines Character vector with control stream content.
#' @param prefix Character vector specifying sections to delete. These names
#'   correspond to the text immediately following a "$" that is the first
#'   non-whitespace character on a line. This should specify that shortest
#'   leading substring that deletes the target method (e.g., "EST" for
#'   "ESTIMATION").
#' @return Returns `lines` with `prefix` sections removed.
#' @noRd
ctl_delete_section <- function(lines, prefix) {
  pat_del <- sprintf("^\\s*\\$(%s)",
                     paste0("\\Q", prefix, "\\E", collapse = "|"))
  beg_idx_del <- grep(pat_del, lines)
  if (length(beg_idx_del) == 0) {
    return(lines)
  }

  beg_idx <- grep("^\\s*\\$.+", lines)
  if (identical(beg_idx, beg_idx_del)) {
    if (beg_idx[1] > 1) {
      return(lines[1:(beg_idx[1] - 1)])
    }
    return(character(0))
  }
  if (length(beg_idx) < 2) {
    stop("bug: invalid state, beg_idx should be above 1: ", beg_idx)
  }

  end_idx <- c(beg_idx[2:length(beg_idx)] - 1,
               length(lines))
  end_idx_del <- end_idx[beg_idx %in% beg_idx_del]
  del_pos <- unlist(purrr::map2(beg_idx_del, end_idx_del, `:`))

  return(lines[-del_pos])
}

# TODO: Embed instructions or otherwise point to documentation about
# requirements for this section.
#
# TODO: Update run_chains to parse these markers and to inject
# {model_id}_{chain} for {id} placeholder.
#
# TODO: Update run_chains to do simple validation of this block.
nmbayes_est_stub <- c(
  NMBAYES_CTL_START,
  "$EST METHOD=CHAIN FILE={model_id}.chn NSAMPLE=4 ISAMPLE=0 SEED=1 CTYPE=0 IACCEPT=0.3 DF=10 DFS=0",
  ";$EST METHOD=BAYES SEED=1 NBURN=NNN NITER=NNNN PRINT=10 MSFO=./{{id}}.msf RANMETHOD=P PARAFPRINT=10000 BAYES_PHI_STORE=0",
  "",
  ";$TABLE NUM CL V2 Q V3 KA ETAS(1:LAST) EPRED IPRED NPDE EWRES NOPRINT ONEHEADER FILE={{id}}.tab RANMETHOD=P",
  NMBAYES_CTL_END
)
