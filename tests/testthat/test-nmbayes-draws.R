
test_that("reshape_iph() return value can be converted into draws object", {
  iphs <- chain_paths(NMBAYES_MOD1, ".iph")
  dfs <- purrr::map(iphs, ~ reshape_iph(fread_draws(.x))$draws)

  draws <- posterior::as_draws(dfs)
  expect_identical(posterior::ndraws(draws), 100L)
  expect_identical(posterior::niterations(draws), 50L)
  expect_identical(posterior::nchains(draws), 2L)

  rvars <- posterior::as_draws_rvars(draws)
  expect_identical(posterior::variables(rvars),
                   c("PMIX", "PHI", "ETA", "MCMCOBJ"))
  expect_identical(dim(rvars$ETA),
                   c(5L, 1L, 160L))
  expect_identical(dim(posterior::drop(rvars$ETA)),
                   c(5L, 160L))
  expect_identical(dim(rvars$MCMCOBJ),
                   c(1L, 160L))
})

test_that("reshape_iph() encodes labels in parameter names: no subpop", {
  niter <- 4L
  nsubj <- 3L
  nobs <- niter * nsubj

  ids <- 1:nsubj * 100L
  data <- tibble::tibble(
    "ITERATION" = rep(1:niter, each = nsubj),
    "SUBJECT_NO" = rep(1:nsubj, times = niter),
    "ID" = rep(ids, times = niter),
    "SUBPOP" = rep(0L, nobs),
    "PMIX" = 22L,
    "PHI(1)" = 22L,
    "PHI(2)" = 22L,
    "ETA(1)" = 22L,
    "ETA(2)" = 22L,
    "ETA(3)" = 22L,
    # Give one parameter identifying values for debugging purposes.
    "MCMCOBJ" = rep(1:nsubj * 1000L, times = niter)
  )
  res <- reshape_iph(data)

  expect_identical(
    colnames(res$draws),
    c("PMIX[1,1]", "PMIX[1,2]", "PMIX[1,3]",
      "PHI[1,1,1]", "PHI[1,1,2]", "PHI[1,1,3]",
      "PHI[2,1,1]", "PHI[2,1,2]", "PHI[2,1,3]",
      "ETA[1,1,1]", "ETA[1,1,2]", "ETA[1,1,3]",
      "ETA[2,1,1]", "ETA[2,1,2]", "ETA[2,1,3]",
      "ETA[3,1,1]", "ETA[3,1,2]", "ETA[3,1,3]",
      "MCMCOBJ[1,1]", "MCMCOBJ[1,2]", "MCMCOBJ[1,3]"))

  rvars <- posterior::as_draws_rvars(res$draws)
  expect_identical(dim(rvars$MCMCOBJ), c(1L, nsubj))
  expect_equal(as.numeric(mean(rvars$MCMCOBJ)),
               c(1000, 2000, 3000))

  expect_identical(
    res$id_map,
    tibble::tibble(index = 1:nsubj, ID = ids))
  expect_identical(
    res$subpop_map,
    tibble::tibble(index = 1L, SUBPOP = 0L))
})

test_that("reshape_iph() encodes labels in parameter names: subpops", {
  niter <- 4L
  nsubj <- 3L
  npops <- 2L

  ids <- 1:nsubj * 100L
  data <- tibble::tibble(
    "ITERATION" = rep(1:niter, each = nsubj * npops),
    "SUBJECT_NO" = rep(1:nsubj, times = niter * npops),
    "ID" = rep(ids, times = niter * npops),
    "SUBPOP" = rep(1:npops, times = niter * nsubj),
    "PMIX" = 22L,
    "PHI(1)" = 22L,
    "PHI(2)" = 22L,
    "ETA(1)" = 22L,
    "ETA(2)" = 22L,
    "ETA(3)" = 22L,
    # Give one parameter identifying values for debugging purposes.
    "MCMCOBJ" = rep(1:nsubj, times = niter * npops)  * c(1000L, 10000L)
  )
  res <- reshape_iph(data)

  expect_identical(
    colnames(res$draws),
    c("PMIX[1,1]", "PMIX[1,2]", "PMIX[1,3]",
      "PMIX[2,1]", "PMIX[2,2]", "PMIX[2,3]",
      "PHI[1,1,1]", "PHI[1,1,2]", "PHI[1,1,3]",
      "PHI[1,2,1]", "PHI[1,2,2]", "PHI[1,2,3]",
      "PHI[2,1,1]", "PHI[2,1,2]", "PHI[2,1,3]",
      "PHI[2,2,1]", "PHI[2,2,2]", "PHI[2,2,3]",
      "ETA[1,1,1]", "ETA[1,1,2]", "ETA[1,1,3]",
      "ETA[1,2,1]", "ETA[1,2,2]", "ETA[1,2,3]",
      "ETA[2,1,1]", "ETA[2,1,2]", "ETA[2,1,3]",
      "ETA[2,2,1]", "ETA[2,2,2]", "ETA[2,2,3]",
      "ETA[3,1,1]", "ETA[3,1,2]", "ETA[3,1,3]",
      "ETA[3,2,1]", "ETA[3,2,2]", "ETA[3,2,3]",
      "MCMCOBJ[1,1]", "MCMCOBJ[1,2]", "MCMCOBJ[1,3]",
      "MCMCOBJ[2,1]", "MCMCOBJ[2,2]", "MCMCOBJ[2,3]"))

  rvars <- posterior::as_draws_rvars(res$draws)
  expect_identical(dim(rvars$MCMCOBJ), c(npops, nsubj))
  expect_equal(as.numeric(mean(rvars$MCMCOBJ)),
               c(1000, 10000,
                 2000, 20000,
                 3000, 30000))

  expect_identical(
    res$id_map,
    tibble::tibble(index = 1:nsubj, ID = ids))
  expect_identical(
    res$subpop_map,
    tibble::tibble(index = 1:npops, SUBPOP = 1:npops))
})
