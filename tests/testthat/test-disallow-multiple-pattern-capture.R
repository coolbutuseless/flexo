context("disallow-multiple-pattern-capture")

test_that("mutiple capture groups per pattern not allowed", {

  regexes <- c(single = 'a', multi = '(b)|(c)')
  text     <- "abc"
  expect_error(lex(text, regexes), "single capture group")

})
