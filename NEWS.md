# bbr.bayes 0.3.1

## Changes

* `find_stan_gq_children()` and `stan_summary_log()` have been
   adjusted to prevent a change in stringr 1.6.0 from affecting
   details of their return values.  (#169)


# bbr.bayes 0.3.0

## Changes

* `CmdStanModel$sample` writes samples to
  `{model}-{timestamp}-{chain}-{rand}.csv` by default.  The
  `submit_model()` method for `stan` models (`?stan_submit_model`) now
  overrides that default to drop the timestamp and random characters
  so that the file name is consistent across runs.  (#167)


# bbr.bayes 0.2.2

## Bug fixes

* For `nmbayes` models, the `posterior::as_draws()` method
  (`?bbr_as_draws`) and `read_fit_model()` rename variables for
  compatibility with `?posterior::draws_rvars`.  This logic mishandled
  `THETA` names with multiple digits (e.g., `THETA10`).  (#163)

* The `submit_model()` method for `nmbayes` (`?nmbayes_submit_model`)
  did not generate initial values synchronously when a caller passed
  `FALSE` for `.wait`, leading to a race between creating the initial
  values and the subsequent model runs reading those values.  (#162)

* The documentation of the `submit_model()` methods for `nmbayes`
  (`?nmbayes_submit_model`) and `stan` (`?stan_submit_model`) models
  inherited their `.overwrite` description from `bbr::submit_model()`,
  which inaccurately described their handling.  (#161)

* `install_torsten()` aborts if the caller specifies a `version`
  argument that doesn't uniquely identify an available version.  It
  incorrectly aborted when the specified version was valid but a
  sub-string of another available version (e.g., "v0.91.0" could not
  be selected because a "v0.91.0rc2" release exists).  (#159)


# bbr.bayes 0.2.1

## Changes

* `check_nonmem_finished.bbi_nmbayes_model()` now returns `FALSE` if
  the output directory doesn't exist, following a `bbr` v1.11.0 change
  in behavior. (#150)

* The nmbayes `get_data_path()` method has been updated to follow
  changes in `bbr` v1.10.0.

  * The first argument has been renamed from `.mod` to `.bbi_object`.
    (#138)

  * The `.check_exists` argument is now supported.  (#152)

  * A data path is now extracted from the control stream if the model
    has not yet finished.  (#152)


# bbr.bayes 0.2.0

## New features and changes

* `bbr.bayes` now requires `bbr` 1.10.0 or later.  (#134)

### NONMEM Bayes

* Initial support has been added for NONMEM Bayes models via the
  `bbi_nmbayes_model` model type.  **Compatibility note:** the 0.1.0
  release included a `bbi_nmbayes_model` model type that was marked as
  experimental and is not compatible with the new design.  (#84, #108,
  #133)

* New `copy_model_as_nmbayes()` function for creating a
  `bbi_nmbayes_model` model from a parent `bbi_nonmem_model` object.
  (#84)

* New `chain_paths()` helper for constructing paths to files within
  chain sub-models.  (#104)

* New `nm_join_bayes()` and `nm_join_bayes_quick()` functions provide
  an interface for joining model output summaries to input data.
  (#106, #116, #118, #120, #122)

* Various tailored methods for `bbr` generics have been added.  (#84,
  #123, #126, #127, #128).

### Stan

* New `install_torsten()` function for downloading and installing
  [Torsten](https://github.com/metrumresearchgroup/Torsten).  (#95)

* The bundled `bern-gq.stan` model has been updated to use syntax
  compatible with Stan 2.33.  (#99)

* The signature of the `submit_model()` method for Stan models did not
  match the positional arguments of the generic.  `.bbi_args` (unused
  for Stan models) has been added as the second argument.  (#132)


# bbr.bayes 0.1.1

## Changes

`submit_model` no longer sets up a `.gitignore` rule for posterior
CSVs of Stan models.  (#92)


# bbr.bayes 0.1.0

This initial release adds support for working with Stan models. (#81)
