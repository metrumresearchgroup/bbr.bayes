# Create Stan data
#
# This function must return the list that will be passed to `data` argument
#   of `cmdstanr::sample()`
#
# The `.dir` argument represents the absolute path to the directory containing
#   this file. This is useful for building file paths to the input files you will
#   load. Note: you _don't_ need to pass anything to this argument, you only use
#   it within the function. `bbr` will pass in the correct path when it calls
#   `make_standata()` under the hood.
make_standata <- function(.dir) {
  # read in any input data
  in_data <- readr::read_csv(file.path(.dir, '..', 'my_data.csv'))
  # do any transformations
  # return the list that can be passed to cmdstanr
}
