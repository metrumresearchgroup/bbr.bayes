
`bbr.bayes` is an extension to the [bbr] package for Bayesian
modeling.  The package is in the early stages of development.  Initial
support has been added for [Stan] models (powered by [cmdstanr]).
Upcoming releases will extend Stan support and add support for NONMEM
Bayes models.

## Installation

`bbr.bayes` and two of its dependencies, `bbr` and `cmdstanr`, are not
on CRAN.  They can be installed from GitHub with, e.g.,
[remotes::install_git()][rig].

```R
> remotes::install_git("git@github.com:stan-dev/cmdstanr.git")
> remotes::install_git("git@github.com:metrumresearchgroup/bbr.git")
> remotes::install_git("git@github.com:metrumresearchgroup/bbr.bayes.git")
```

`cmdstanr` is also available from the CRAN-like repositories at
<https://mc-stan.org/r-packages/> and [MPN].

## Documentation

Documentation, including a ["Getting Started with bbr.bayes and
Stan"][gss] vignette, is available at
<https://metrumresearchgroup.github.io/bbr.bayes/>.

### Cheat Sheet

<a href="https://metrumresearchgroup.github.io/cheatsheets/bbr_bayes_stan_cheat_sheet.pdf"><img src="https://metrumresearchgroup.github.io/cheatsheets/thumbnails/bbr_bayes_stan_cheat_sheet_thumbnail.png" width="700" height="395"/></a>

## Development

`bbr.bayes` uses [pkgr] to manage development dependencies and [renv]
to provide isolation. To set up an environment with pkgr and renv:

 1. clone the repo
 2. install `pkgr`, if not already installed
 3. open package in an R session and run `renv::init(bare = TRUE)`
 4. run `pkgr install` in terminal within package directory
 5. restart session

<!-- TODO: Publish cheatsheet to https://metrumresearchgroup.github.io/cheatsheets/ and add link. -->

[bbr]: https://metrumresearchgroup.github.io/bbr
[cmdstanr]: https://mc-stan.org/cmdstanr/
[gss]: https://metrumresearchgroup.github.io/bbr.bayes/articles/getting-started-stan.html
[MPN]: https://mpn.metworx.com/docs/snapshots
[pkgr]: https://github.com/metrumresearchgroup/pkgr
[renv]: https://rstudio.github.io/renv/
[rig]: https://remotes.r-lib.org/reference/install_git.html
[Stan]: https://mc-stan.org/
