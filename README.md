
# bbr.bayes <a href='https://metrumresearchgroup.github.io/bbr.bayes'><img src = "man/figures/logo.png" align="right" /></a>

<!-- badges: start -->

[![Build Status](https://github-drone.metrumrg.com/api/badges/metrumresearchgroup/bbr.bayes/status.svg)](https://github-drone.metrumrg.com/metrumresearchgroup/bbr.bayes)
<!-- badges: end -->


`bbr.bayes` is an extension to the [bbr] package for Bayesian
modeling.  The package is in the early stages of development.  Initial
support has been added for [Stan] models (powered by [cmdstanr]) and
for NONMEM Bayes models.

## Installation

You can install the latest release of `bbr.bayes` from [MPN].

To install the latest development version from GitHub, you can use
`remotes`:

```R
# install.packages("remotes")
remotes::install_github("metrumresearchgroup/bbr.bayes")
```

Note that a few `bbr.bayes` dependencies are not on CRAN but are
available from other CRAN-like repos:

 * [bbr] is available on [MPN].  Use snapshot 2024-03-01 or later to
   get the minimum version required by `bbr.bayes`.

 * [nmrec] is available on [MPN] as of the 2023-09-19 snapshot.

 * [cmdstanr] is available from [MPN] and
   <https://mc-stan.org/r-packages/>.


## Documentation

Documentation, including a ["Getting Started with bbr.bayes and Stan"][gss]
and ["Getting Started with bbr.bayes and NONMEM Bayes"][gsn]
vignettes, is available at
<https://metrumresearchgroup.github.io/bbr.bayes/>.

### Cheat Sheets

<a href="https://metrumresearchgroup.github.io/cheatsheets/bbr_bayes_stan_cheat_sheet.pdf"><img src="https://metrumresearchgroup.github.io/cheatsheets/thumbnails/bbr_bayes_stan_cheat_sheet_thumbnail.png" width="700" height="395"/></a>

<a href="https://metrumresearchgroup.github.io/cheatsheets/bbr_bayes_nonmem_cheat_sheet.pdf"><img src="https://metrumresearchgroup.github.io/cheatsheets/thumbnails/bbr_bayes_nonmem_cheat_sheet_thumbnail.png" width="700" height="395"/></a>

## Development

`bbr.bayes` uses [pkgr] to manage development dependencies and [renv]
to provide isolation. To set up an environment with pkgr and renv:

 1. clone the repo
 2. install `pkgr`, if not already installed
 3. open package in an R session and run `renv::init(bare = TRUE)`
 4. run `pkgr install` in terminal within package directory
 5. restart session

[bbr]: https://metrumresearchgroup.github.io/bbr
[cmdstanr]: https://mc-stan.org/cmdstanr/
[gsn]: https://metrumresearchgroup.github.io/bbr.bayes/articles/getting-started-nmbayes.html
[gss]: https://metrumresearchgroup.github.io/bbr.bayes/articles/getting-started-stan.html
[MPN]: https://mpn.metworx.com/docs/snapshots
[nmrec]: https://metrumresearchgroup.github.io/nmrec
[pkgr]: https://github.com/metrumresearchgroup/pkgr
[renv]: https://rstudio.github.io/renv/
[Stan]: https://mc-stan.org/
