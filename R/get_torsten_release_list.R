get_torsten_release_list <- function() {
  require(cmdstanr)
  dest_file <- tempfile(pattern = "releases-", fileext = ".json")
  download_url <- "https://api.github.com/repos/metrumresearchgroup/Torsten/releases"
  release_list_downloaded <- cmdstanr:::download_with_retries(download_url, dest_file)
  if (!release_list_downloaded) {
    stop("GitHub download of release list failed.", call. = FALSE)
  }
  release <- jsonlite::read_json(dest_file)
  sapply(release,
         function(x){
           x$tag_name
         })
}
