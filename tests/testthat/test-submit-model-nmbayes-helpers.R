test_that("adjust_data_path() handles quotes", {
  testthat::skip_if_not_installed("nmrec")
  cases <- list(
    list(
      input = c("$PROB p", "$DATA 'one two.csv'"),
      want = "$PROB p\n$DATA '../one two.csv'\n"
    ),
    list(
      input = c("$PROB p", '$DATA "one two.csv"'),
      want = '$PROB p\n$DATA "../one two.csv"\n'
    ),
    list(
      input = c("$PROB p", "$DATA '/tmp/one.csv'"),
      want = "$PROB p\n$DATA '/tmp/one.csv'\n"
    )
  )
  for (case in cases) {
    ctl <- nmrec::parse_ctl(case$input)
    adjust_data_path(ctl)
    expect_identical(format(ctl), case$want)
  }
})
