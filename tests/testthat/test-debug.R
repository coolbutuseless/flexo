context("test-debug")

test_that("debug works", {

  regexes <- c(a='1', b='2')

  expect_output(lex("12", regexes, verbose = TRUE), ".missing")
})




test_that(".missing handled works", {

  regexes <- c(a='1', b='2')
  res <- lex("123", regexes)
  expect_identical(names(res), c('a', 'b', '.missing'))
})
